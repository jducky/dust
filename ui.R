## JD01

dashboardPage(
  dashboardHeader(title = "ENGIS 미세먼지 예측시스템"),
  dashboardSidebar(
    sliderInput("rateThreshold", "미세먼지 알람 설정",
      min = 0, max = 50, value = 3, step = 0.1
    ),
    sidebarMenu(
      menuItem("미세먼지예측", tabName = "dashboard"),
      menuItem("미세먼지GIS", tabName = "gismap"),
      menuItem("데이터처리", tabName = "rawdata")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
        fluidRow(
          valueBoxOutput("rate"),
          valueBoxOutput("count"),
          valueBoxOutput("users")
        ),
        fluidRow(
          box(
            width = 8, status = "info", solidHeader = TRUE,
            title = "서울시 미세먼지 지도 (last 5 min)",
            leafletOutput("busmap", height = 700)
          ),
          box(
            width = 4, status = "info",
            title = "구별 미세먼지 농도 순위",
            tableOutput("packageTable")
          )
        )
      ),
      tabItem("gismap",
              fluidRow(
                column(width = 9,
                       box(width = NULL,
                           uiOutput("numVehiclesTable")
                       )
                ),
                column(width = 3,
                       box(width = NULL, status = "warning",
                           uiOutput("routeSelect"),
                           checkboxGroupInput("directions", "Show",
                                              choices = c(
                                                Northbound = 4,
                                                Southbound = 1,
                                                Eastbound = 2,
                                                Westbound = 3
                                              ),
                                              selected = c(1, 2, 3, 4)
                           ),
                           p(
                             class = "text-muted",
                             paste("Note: a route number can have several different trips, each",
                                   "with a different path. Only the most commonly-used path will",
                                   "be displayed on the map."
                             )
                           ),
                           actionButton("zoomButton", "Zoom to fit buses")
                       ),
                       box(width = NULL, status = "warning",
                           selectInput("interval", "Refresh interval",
                                       choices = c(
                                         "30 seconds" = 30,
                                         "1 minute" = 60,
                                         "2 minutes" = 120,
                                         "5 minutes" = 300,
                                         "10 minutes" = 600
                                       ),
                                       selected = "60"
                           ),
                           uiOutput("timeSinceLastUpdate"),
                           actionButton("refresh", "Refresh now"),
                           p(class = "text-muted",
                             br(),
                             "Source data updates every 30 seconds."
                           )
                       )
                )
              )
      ),
      tabItem("rawdata",
        numericInput("maxrows", "Rows to show", 25),
        verbatimTextOutput("rawtable"),
        downloadButton("downloadCsv", "Download as CSV")
      )
    )
  )
)

