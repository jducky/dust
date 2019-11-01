## JD01

library(shiny)
library(shinySignals)   # devtools::install_github("hadley/shinySignals")
library(dplyr)
library(shinydashboard)
library(leaflet)
library(curl)
library(bubbles)        # devtools::install_github("jcheng5/bubbles")
source("bloomfilter.R")

# An empty prototype of the data frame we want to create
prototype <- data.frame(date = character(), time = character(),
  size = numeric(), r_version = character(), r_arch = character(),
  r_os = character(), package = character(), version = character(),
  country = character(), ip_id = character(), received = numeric())

# Connects to streaming log data for cran.rstudio.com and
# returns a reactive expression that serves up the cumulative
# results as a data frame
packageStream <- function(session) {
  # Connect to data source
  sock <- socketConnection("cransim.rstudio.com", 6789, blocking = FALSE, open = "r")
  # Clean up when session is over
  session$onSessionEnded(function() {
    close(sock)
  })

  # Returns new lines
  newLines <- reactive({
    invalidateLater(1000, session)
    readLines(sock)
  })

  # Parses newLines() into data frame
  reactive({
    if (length(newLines()) == 0)
      return()
    read.csv(textConnection(newLines()), header=FALSE, stringsAsFactors=FALSE,
      col.names = names(prototype)
    ) %>% mutate(received = as.numeric(Sys.time()))
  })
}

# Accumulates pkgStream rows over time; throws out any older than timeWindow
# (assuming the presence of a "received" field)
packageData <- function(pkgStream, timeWindow) {
  shinySignals::reducePast(pkgStream, function(memo, value) {
    rbind(memo, value) %>%
      filter(received > as.numeric(Sys.time()) - timeWindow)
  }, prototype)
}

# Count the total nrows of pkgStream
downloadCount <- function(pkgStream) {
  shinySignals::reducePast(pkgStream, function(memo, df) {
    if (is.null(df))
      return(memo)
    memo + nrow(df)
  }, 0)
}

# Use a bloom filter to probabilistically track the number of unique
# users we have seen; using bloom filter means we will not have a
# perfectly accurate count, but the memory usage will be bounded.
userCount <- function(pkgStream) {
  # These parameters estimate that with 5000 unique users added to
  # the filter, we'll have a 1% chance of false positive on the next
  # user to be queried.
  bloomFilter <- BloomFilter$new(5000, 0.01)
  total <- 0
  reactive({
    df <- pkgStream()
    if (!is.null(df) && nrow(df) > 0) {
      # ip_id is only unique on a per-day basis. To make them unique
      # across days, include the date. And call unique() to make sure
      # we don't double-count dupes in the current data frame.
      ids <- paste(df$date, df$ip_id) %>% unique()
      # Get indices of IDs we haven't seen before
      newIds <- !sapply(ids, bloomFilter$has)
      # Add the count of new IDs
      total <<- total + length(newIds)
      # Add the new IDs so we know for next time
      sapply(ids[newIds], bloomFilter$set)
    }
    total
  })
}

# 1=South, 2=East, 3=West, 4=North
dirColors <-c("1"="#595490", "2"="#527525", "3"="#A93F35", "4"="#BA48AA")

# Download data from the Twin Cities Metro Transit API
# http://svc.metrotransit.org/NexTrip/help
getMetroData <- function(path) {
  url <- paste0("http://svc.metrotransit.org/NexTrip/", path, "?format=json")
  jsonlite::fromJSON(url)
}

# Load static trip and shape data
trips  <- readRDS("metrotransit-data/rds/trips.rds")
shapes <- readRDS("metrotransit-data/rds/shapes.rds")


# Get the shape for a particular route. This isn't perfect. Each route has a
# large number of different trips, and each trip can have a different shape.
# This function simply returns the most commonly-used shape across all trips for
# a particular route.
get_route_shape <- function(route) {
  routeid <- paste0(route, "-75")
  
  # For this route, get all the shape_ids listed in trips, and a count of how
  # many times each shape is used. We'll just pick the most commonly-used shape.
  shape_counts <- trips %>%
    filter(route_id == routeid) %>%
    group_by(shape_id) %>%
    summarise(n = n()) %>%
    arrange(-n)
  
  shapeid <- shape_counts$shape_id[1]
  
  # Get the coordinates for the shape_id
  shapes %>% filter(shape_id == shapeid)
}


function(input, output, session) {
  
  # Route select input box
  output$routeSelect <- renderUI({
    live_vehicles <- getMetroData("VehicleLocations/0")
    
    routeNums <- sort(unique(as.numeric(live_vehicles$Route)))
    # Add names, so that we can add all=0
    names(routeNums) <- routeNums
    routeNums <- c(All = 0, routeNums)
    selectInput("routeNum", "Route", choices = routeNums, selected = routeNums[2])
  })
  
  # Locations of all active vehicles
  vehicleLocations <- reactive({
    input$refresh # Refresh if button clicked
    
    # Get interval (minimum 30)
    interval <- max(as.numeric(input$interval), 30)
    # Invalidate this reactive after the interval has passed, so that data is
    # fetched again.
    invalidateLater(interval * 1000, session)
    
    getMetroData("VehicleLocations/0")
  })
  
  # Locations of vehicles for a particular route
  routeVehicleLocations <- reactive({
    if (is.null(input$routeNum))
      return()
    
    locations <- vehicleLocations()
    
    if (as.numeric(input$routeNum) == 0)
      return(locations)
    
    locations[locations$Route == input$routeNum, ]
  })
  
  # Get time that vehicles locations were updated
  lastUpdateTime <- reactive({
    vehicleLocations() # Trigger this reactive when vehicles locations are updated
    Sys.time()
  })
  
  # Number of seconds since last update
  output$timeSinceLastUpdate <- renderUI({
    # Trigger this every 5 seconds
    invalidateLater(5000, session)
    p(
      class = "text-muted",
      "Data refreshed ",
      round(difftime(Sys.time(), lastUpdateTime(), units="secs")),
      " seconds ago."
    )
  })
  
  output$numVehiclesTable <- renderUI({
    locations <- routeVehicleLocations()
    if (length(locations) == 0 || nrow(locations) == 0)
      return(NULL)
    
    # Create a Bootstrap-styled table
    tags$table(class = "table",
               tags$thead(tags$tr(
                 tags$th("Color"),
                 tags$th("Direction"),
                 tags$th("Number of vehicles")
               )),
               tags$tbody(
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     dirColors[4]
                   ))),
                   tags$td("Northbound"),
                   tags$td(nrow(locations[locations$Direction == "4",]))
                 ),
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     dirColors[1]
                   ))),
                   tags$td("Southbound"),
                   tags$td(nrow(locations[locations$Direction == "1",]))
                 ),
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     dirColors[2]
                   ))),
                   tags$td("Eastbound"),
                   tags$td(nrow(locations[locations$Direction == "2",]))
                 ),
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                     dirColors[3]
                   ))),
                   tags$td("Westbound"),
                   tags$td(nrow(locations[locations$Direction == "3",]))
                 ),
                 tags$tr(class = "active",
                         tags$td(),
                         tags$td("Total"),
                         tags$td(nrow(locations))
                 )
               )
    )
  })
  
  # Store last zoom button value so we can detect when it's clicked
  lastZoomButtonValue <- NULL
  
  output$busmap <- renderLeaflet({
    locations <- routeVehicleLocations()
    if (length(locations) == 0)
      return(NULL)
    
    # Show only selected directions
    locations <- filter(locations, Direction %in% as.numeric(input$directions))
    
    # Four possible directions for bus routes
    dirPal <- colorFactor(dirColors, names(dirColors))
    
    map <- leaflet(locations) %>%
      addTiles('http://{s}.tile.thunderforest.com/transport/{z}/{x}/{y}.png') %>%
      addCircleMarkers(
        ~VehicleLongitude,
        ~VehicleLatitude,
        color = ~dirPal(Direction),
        opacity = 0.8,
        radius = 8
      )
    
    if (as.numeric(input$routeNum) != 0) {
      route_shape <- get_route_shape(input$routeNum)
      
      map <- addPolylines(map,
                          route_shape$shape_pt_lon,
                          route_shape$shape_pt_lat,
                          fill = FALSE
      )
    }
    
    rezoom <- "first"
    # If zoom button was clicked this time, and store the value, and rezoom
    if (!identical(lastZoomButtonValue, input$zoomButton)) {
      lastZoomButtonValue <<- input$zoomButton
      rezoom <- "always"
    }
    
    map <- map %>% mapOptions(zoomToLimits = rezoom)
    
    map
  })
}
