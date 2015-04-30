# More info:
#   https://github.com/jcheng5/googleCharts
# Install:
#   devtools::install_github("jcheng5/googleCharts")

shinyUI(fluidPage(
#   tags$div(class = "header", checked = NA,
#            tags$p("Ready to take the Shiny tutorial? If so"),
#            tags$img(src = "logos/logo200.png")),

    # Use the Google webfont "Source Sans Pro"
  tags$head(
    # Include our custom CSS
    includeCSS("styles.css"),
    # Hide the red error messages!!!
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    )
    
  ),
  tags$h2("Select data"),
  tags$hr(),
  fluidRow(
    shiny::column(4, uiOutput("group")
    ),
    shiny::column(4, uiOutput("domain")
    ),
    shiny::column(4, uiOutput("indOrAgg")
    )
    ),
  fluidRow(
    shiny::column(4, uiOutput("item")
    ),
    shiny::column(4, uiOutput("element")
    ),
    shiny::column(4, uiOutput("yearRange")
    )
  ),
  tags$h2("Preview data"),
  tags$hr(),
  fluidRow(
      dataTableOutput("mytable")
      ),
  tags$h2("Preview time-series"),
  tags$hr(),
  fluidRow(
    plotOutput("timeSeries")
  ),
  tags$h2("Download data"),
  tags$hr(),
   fluidRow(
     shiny::column(4, downloadButton('downloadData', 'Download')
     )
   )
  )
)