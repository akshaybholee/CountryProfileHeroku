trade_facilitation_ui <- function(id) {
  ns <- NS(id)
  
  tagList(useShinyjs(),
          fluidRow(column(
            8,
            offset = 2,
            shinydashboard::box(
              id = ns("DFTBox"),
              solidHeader = TRUE,
              column(12,
                     tags$div(
                       checked = NA,
                       list(
                         tags$h4(class = "DFT_title", 'Trade Facilitation and Paperless Trade'),
                         
                         column(
                           class = "DTF_row",
                           12,
                           column(2,
                                  class = "DTF_score",
                                  checked = NA,
                                  list(
                                    htmlOutput(
                                      outputId = ns("Total"),
                                      container = tags$h2,
                                      class = "DTF_total"
                                    )
                                  )),
                           
                           column(2,
                                  class = "DTF_gauge",
                                  list(
                                    highchartOutput(
                                      outputId = ns("Transparency"),
                                      width = "190.38px",
                                      height = "100px"
                                    ),
                                    tags$p(class = "DTF_name", "Transparency")
                                  )),
                           
                           column(2,
                                  class = "DTF_gauge",
                                  list(
                                    highchartOutput(
                                      outputId = ns("formalities"),
                                      width = "190.38px",
                                      height = "100px"
                                    ),
                                    tags$p(class = "DTF_name", "Formalities")
                                  )),
                           column(2,
                                  class = "DTF_gauge",
                                  list(
                                    highchartOutput(
                                      outputId = ns("institution"),
                                      width = "190.38px",
                                      height = "100px"
                                    ),
                                    tags$p(class = "DTF_name", "Institutional Arrangement and Cooperation")
                                  )),
                           column(2,
                                  class = "DTF_gauge",
                                  list(
                                    highchartOutput(
                                      outputId = ns("paperless"),
                                      width = "190.38px",
                                      height = "100px"
                                    ),
                                    tags$p(class = "DTF_name", "Paperless Trade")
                                  )),
                           column(2,
                                  class = "DTF_gauge",
                                  list(
                                    highchartOutput(
                                      outputId = ns("cross_border"),
                                      width = "190.38px",
                                      height = "100px"
                                    ),
                                    tags$p(class = "DTF_name", "Cross-border Paperless Trade")
                                  ))
                           
                           
                           
                           
                         ),
                         tags$div(
                           class = "DTF_text",
                           checked =
                             NA,
                           htmlOutput(outputId = ns("TradeText"),
                                      container = tags$i,
                                      class = "DTF_font" )
                         )
                         
                         
                         
                       )
                     )),
              
              width = 16
            )
          )))
}

trade_facilitation_server <-
  function(id,
           reporter_iso_sel,
           reporter,
           mycolor) {
    moduleServer(id,
                 function(input, output, session) {
                   # Connect to Database set SQL Table to Data Frame
                   df_trade_facilitation <- reactive({
                     dbGetQuery(
                       con,
                       paste0(
                         "SELECT [Country_ISO],[Year],[Index_Description],[Index_Value]
                          FROM [dbo].[Facilitation_Paperless_Trade]
                          where [Country_ISO] ='",
                         reporter_iso_sel(),
                         "'
                          and Year = (Select max(year)
                                      from [Facilitation_Paperless_Trade]
                                      where [Country_ISO] ='",
                         reporter_iso_sel(),
                         "')
                         "
                       )
                     )
                   }) %>% bindCache(reporter_iso_sel())
                   
                   # Hide Section if no data is available
                   observe({
                     if (nrow(df_trade_facilitation()) > 0)
                     {
                       shinyjs::show(id = "DFTBox")
                     }
                     else
                     {
                       shinyjs::hide(id = "DFTBox")
                     }
                     
                   })
                   
                   # Display Total Text and value
                   output$Total <- renderText({
                     if (nrow(df_trade_facilitation()) > 0)
                     {
                       df_filtered_DTF <-
                         filter(
                           df_trade_facilitation(),
                           df_trade_facilitation()$Index_Description == 'tfi'
                         )
                       df_total <- df_filtered_DTF[4] * 100
                       
                       HTML(paste("Trade facilitation score of <br>", df_total, "%",
                                  sep = ""))
                       
                     }
                   }) %>% bindCache(reporter_iso_sel())
                   
                   # Filter data frame to display data for first (Transparency) gauge chart
                   output$Transparency <- renderHighchart({
                     if (nrow(df_trade_facilitation()) > 0)
                     {
                       df_filtered_DTF <-
                         filter(
                           df_trade_facilitation(),
                           df_trade_facilitation()$Index_Description == 'transparency'
                         )
                       df_transparency <- df_filtered_DTF[4] * 100
                       
                       
                       highchart() %>%
                         hc_chart(type = "solidgauge") %>%
                         hc_pane(
                           startAngle = -90,
                           endAngle = 90,
                           background = list(
                             outerRadius = '100%',
                             innerRadius = '60%',
                             shape = "arc",
                             borderWidth =  0,
                             backgroundColor = '#C9C9C9'
                           )
                         ) %>%
                         hc_tooltip(enabled = FALSE) %>%
                         hc_yAxis(
                           min = 0,
                           max = 100,
                           lineWidth = 0,
                           tickPositions = list()
                         ) %>%
                         hc_add_series(
                           data = list(list(color = '#0291da', y = df_transparency[[1]])),
                           dataLabels = list(
                             y = -30,
                             borderWidth = 0,
                             useHTML = TRUE,
                             formatter = htmlwidgets::JS("function() {return this.y+ '%'}"),
                             style = list(fontSize = "20px")
                           )
                         ) %>%
                         hc_size(width = 190.38, height = 200)
                     }
                   }) %>% bindCache(reporter_iso_sel())
                   
                   # Filter data frame to display data for formalities gauge chart
                   output$formalities <- renderHighchart({
                     if (nrow(df_trade_facilitation()) > 0)
                     {
                       df_filtered_DTF <-
                         filter(
                           df_trade_facilitation(),
                           df_trade_facilitation()$Index_Description == 'formalities'
                         )
                       df_formalities <- df_filtered_DTF[4] * 100
                       
                       
                       highchart() %>%
                         hc_chart(type = "solidgauge") %>%
                         hc_pane(
                           startAngle = -90,
                           endAngle = 90,
                           background = list(
                             outerRadius = '100%',
                             innerRadius = '60%',
                             shape = "arc",
                             borderWidth =  0,
                             backgroundColor = '#C9C9C9'
                           )
                         ) %>%
                         hc_tooltip(enabled = FALSE) %>%
                         hc_yAxis(
                           min = 0,
                           max = 100,
                           lineWidth = 0,
                           tickPositions = list()
                         ) %>%
                         hc_add_series(
                           data = list(list(color = '#0291da', y = df_formalities[[1]])),
                           dataLabels = list(
                             y = -30,
                             borderWidth = 0,
                             useHTML = TRUE,
                             formatter = htmlwidgets::JS("function() {return this.y+ '%'}"),
                             style = list(fontSize = "20px")
                           )
                         ) %>%
                         hc_size(width = 190.38, height = 200)
                     }
                   }) %>% bindCache(reporter_iso_sel())
                   
                   # Filter data frame to display data for institution gauge chart
                   output$institution <- renderHighchart({
                     if (nrow(df_trade_facilitation()) > 0)
                     {
                       df_filtered_DTF <-
                         filter(
                           df_trade_facilitation(),
                           df_trade_facilitation()$Index_Description == 'institution'
                         )
                       df_institution <- df_filtered_DTF[4] * 100
                       
                       
                       highchart() %>%
                         hc_chart(type = "solidgauge") %>%
                         hc_pane(
                           startAngle = -90,
                           endAngle = 90,
                           background = list(
                             outerRadius = '100%',
                             innerRadius = '60%',
                             shape = "arc",
                             borderWidth =  0,
                             backgroundColor = '#C9C9C9'
                           )
                         ) %>%
                         hc_tooltip(enabled = FALSE) %>%
                         hc_yAxis(
                           min = 0,
                           max = 100,
                           lineWidth = 0,
                           tickPositions = list()
                         ) %>%
                         hc_add_series(
                           data = list(list(color = '#0291da', y =  df_institution[[1]])),
                           dataLabels = list(
                             y = -30,
                             borderWidth = 0,
                             useHTML = TRUE,
                             formatter = htmlwidgets::JS("function() {return this.y+ '%'}"),
                             style = list(fontSize = "20px")
                           )
                         ) %>%
                         hc_size(width = 190.38, height = 200)
                     }
                   }) %>% bindCache(reporter_iso_sel())
                   
                   # Filter data frame to display data for paperless gauge chart
                   output$paperless <- renderHighchart({
                     if (nrow(df_trade_facilitation()) > 0)
                     {
                       df_filtered_DTF <-
                         filter(
                           df_trade_facilitation(),
                           df_trade_facilitation()$Index_Description == 'paperless'
                         )
                       df_paperless <- df_filtered_DTF[4] * 100
                       
                       
                       highchart() %>%
                         hc_chart(type = "solidgauge") %>%
                         hc_pane(
                           startAngle = -90,
                           endAngle = 90,
                           background = list(
                             outerRadius = '100%',
                             innerRadius = '60%',
                             shape = "arc",
                             borderWidth =  0,
                             backgroundColor = '#C9C9C9'
                           )
                         ) %>%
                         hc_tooltip(enabled = FALSE) %>%
                         hc_yAxis(
                           min = 0,
                           max = 100,
                           lineWidth = 0,
                           tickPositions = list()
                         ) %>%
                         hc_add_series(
                           data = list(list(color = '#0291da', y =  df_paperless[[1]])),
                           dataLabels = list(
                             y = -30,
                             borderWidth = 0,
                             useHTML = TRUE,
                             formatter = htmlwidgets::JS("function() {return this.y+ '%'}"),
                             style = list(fontSize = "20px")
                           )
                         ) %>%
                         hc_size(width = 190.38, height = 200)
                     }
                   }) %>% bindCache(reporter_iso_sel())
                   
                   # Filter data frame to display data for first (Transparency) gauge chart
                   output$cross_border <- renderHighchart({
                     if (nrow(df_trade_facilitation()) > 0)
                     {
                       df_filtered_DTF <-
                         filter(
                           df_trade_facilitation(),
                           df_trade_facilitation()$Index_Description == 'cross_border'
                         )
                       df_cross_border <- df_filtered_DTF[4] * 100
                       
                       highchart() %>%
                         hc_chart(type = "solidgauge") %>%
                         hc_pane(
                           startAngle = -90,
                           endAngle = 90,
                           background = list(
                             outerRadius = '100%',
                             innerRadius = '60%',
                             shape = "arc",
                             borderWidth =  0,
                             backgroundColor = '#C9C9C9'
                           )
                         ) %>%
                         hc_tooltip(enabled = FALSE) %>%
                         hc_yAxis(
                           min = 0,
                           max = 100,
                           lineWidth = 0,
                           tickPositions = list()
                         ) %>%
                         hc_add_series(
                           data = list(list(color = '#0291da', y =  df_cross_border[[1]])),
                           dataLabels = list(
                             y = -30,
                             borderWidth = 0,
                             useHTML = TRUE,
                             formatter = htmlwidgets::JS("function() {return this.y+ '%'}"),
                             style = list(fontSize = "20px")
                           )
                         ) %>%
                         hc_size(width = 190.38, height = 200)
                     }
                   }) %>% bindCache(reporter_iso_sel())
                   
                   # Display details text
                   output$TradeText <- renderText({
                     if (nrow(df_trade_facilitation()) > 0) {
                       df_filtered_DTF <- filter(
                         df_trade_facilitation(),
                         df_trade_facilitation()$Index_Description == 'tfi'
                       )
                       
                       df_total <- df_filtered_DTF[4] * 100
                       
                       year <-
                         paste(df_trade_facilitation()$Year[[1]])
                       totalScore <- paste(df_total[[1]])
                       
                       
                       # Database connection for services text data
                       df_narrative <- dbGetQuery(
                         con,
                         "select HTML_text
                          from Dashboard_Narrative
                          where project_name = 'Country Profile'
                          and section_code = 'TF01'"
                       )
                       
                       narrative <-
                         df_narrative$HTML_text[[1]] %>% str_replace_all(c(
                           "REPORTER" = reporter(),
                           "YEAR" = year,
                           "SCORE" = totalScore
                         ))
                       
                       HTML(paste(narrative))
                       
                     }
                     
                   }) %>% bindCache(reporter())
                   
                 })
  }
