# More info:
#   https://github.com/jcheng5/googleCharts
# Install:
#   devtools::install_github("jcheng5/googleCharts")

shinyUI(navbarPage("FAOSTAT data explorer", id="nav",
                   
                   tabPanel("Single-variable explorer",
                            div(class="inner",
                                
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("styles.css"),
                                  # Hide the red error messages!!!
                                  tags$style(type="text/css",
                                             ".shiny-output-error { visibility: hidden; }",
                                             ".shiny-output-error:before { visibility: hidden; }"
                                  )
                                  
                                ), 
  #tags$style(type="text/css", "body {padding-left: 70px;}"),
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
),

tabPanel("Bivariate explorer",
         
         
         div(class="inner",
             
             tags$head(
               # Include our custom CSS
               includeCSS("styles.css")
               
               
             ),
             
             tags$h2("Select data"),
             tags$h3("Variable X"),
             fluidRow(
               shiny::column(6, uiOutput("var_x_item")
               ),
               shiny::column(6, uiOutput("var_x_element")
               )
             ),
             tags$h3("Variable Y"),
             fluidRow(
               shiny::column(6, uiOutput("var_y_item")
               ),
               shiny::column(6, uiOutput("var_y_element")
               )
             ),
             tags$h3("Select Year"),
             fluidRow(
               shiny::column(12, uiOutput("bivar_year")
               )
             ),
             
             tabsetPanel(type= "tabs", position= "above",
                         tabPanel("Single year plot", plotOutput("single_scatter", width="100%", height = "750px")),
                         tabPanel("All years plot", plotOutput("multi_scatter", width="100%", height = "750px"))
             )
             

         )
)

))