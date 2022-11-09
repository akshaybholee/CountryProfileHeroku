tradeinservices_ui <- function(id) {
  ns <- NS(id)
  
  tagList(useShinyjs(), fluidRow(column(
    8,
    offset = 2,
    shinydashboard::box(
      id = ns("ServicesBox"),
      solidHeader = TRUE,
      column(
        4,
        align = 'left',
        htmlOutput(
          outputId = ns("Title"),
          container = tags$h4,
          class = "title"
        )
      ),
      column(
        8,
        align = 'right',
        selectInput(
          inputId =  ns("ServiceTradeflow"),
          label = "Trade Flow",
          choices =  list("Export", "Import"),
          selected = "Export"
        )
      )
      ,
      width = 16
    )
  )),
  fluidRow(column(
    8,
    offset = 2,
    shinydashboard::box(
      id = ns("ServicesBox2"),
      solidHeader = TRUE,
      list(column(12,
                  list(
                    column(
                      4,
                      align = 'left',
                      class = 'barTF',
                      list(
                        htmlOutput(
                          outputId = ns("BarTF"),
                          container = tags$h4,
                          class = "demo_title"
                        ),
                        tags$i(style = "font-size: 13px; margin-bottom:0px;", 'Click on a bar to filter map by service')
                        
                        ,
                        echarts4rOutput(outputId = ns("barchart"),
                                        height = "500px"),
                        tags$div(
                          id = ns("ServicesButton"),
                          actionButton(
                            inputId = ns('reset_filter'),
                            label = 'Reset',
                            class = "button-38"
                          )
                        )
                        
                      )
                    ),
                    
                    column(8,
                           tags$div(
                             class = "col",
                             checked = NA,
                             list(
                               htmlOutput(
                                 outputId = ns("MapTF"),
                                 container = tags$h4,
                                 class = "demo_title mapTF"
                               )
                               ,
                               leafletOutput(outputId = ns("map"),
                                             height = "450px")
                               
                             )
                           ))
                    
                    
                  )),
           column(
             12,
             tags$div(
               class = "col",
               checked = NA,
               tags$div(
                 class = "servicesText",
                 checked = NA,
                 htmlOutput(
                   outputId = ns("ServicesText"),
                   container = tags$p,
                   class = "servicesnarrat"
                 )
               )
             )
           ))
      ,
      
      
      width = 16
    )
  )))
  
}

tradeinservices_server <-
  function(id,
           reporter_iso_sel,
           reporter,
           mycolor,
           services_tradeflow) {
    moduleServer(id,
                 
                 function(input, output, session) {
                   #instantiate filter_value
                   filter_value <- reactiveVal(value = NULL)
                   
                   # Select 2 value of click event
                   observeEvent(input$barchart_clicked_data, {
                     filter_value(input$barchart_clicked_data$value[2])
                     
                   })
                   
                   # Reset the filter
                   observeEvent(input$reset_filter, {
                     filter_value(NULL)
                   })
                   
                   df_services_data <- reactive({
                     dbGetQuery(
                       con,
                       paste0(
                         "SELECT EBOP_Code, Description, Short_Description, Partner, Partner_ISO, Trade_Value, [Year]
                         FROM Services_Trade
                         WHERE Reporter_ISO = '",
                         reporter_iso_sel(),
                         "'
                         AND Trade_Flow = '",
                         services_tradeflow(),
                         "'AND EBOP_Code <> 'SN'
                         AND Partner_ISO <> 'WLD'
                         "
                       )
                     )
                   }) %>% bindCache(reporter_iso_sel(),  services_tradeflow())
                   
                   # Database connection for services data
                   df_top_services <- reactive({
                     df_ser <- df_services_data()
                     
                     df <-
                       aggregate(
                         df_ser$Trade_Value,
                         list(
                           df_ser$EBOP_Code,
                           df_ser$Description,
                           df_ser$Short_Description,
                           df_ser$Year
                         ),
                         FUN = sum
                       )
                     names(df) <-
                       c("EBOP_Code",
                         "Description",
                         "Short_Description",
                         "Year",
                         "tradevalue")
                     
                     if (nrow(df) >= 1)
                     {
                       df$color <- '#0291da'
                       
                       if (!is.null(filter_value()))
                       {
                         df <-
                           within(df, color[Short_Description != filter_value()] <-
                                    '#6fd2ee')
                         df_services <- df
                         
                       } else{
                         df_services <- df
                       }
                     } else{
                       df_services <- df
                     }
                     
                     df_services
                     
                   }) %>% bindCache(
                     reporter_iso_sel(),
                     services_tradeflow(),
                     input$barchart_clicked_data,
                     input$reset_filter
                   )
                   
                   # Database connection for service markets data
                   df_countries <- reactive({
                     if (nrow(df_services_data()) >= 1) {
                       df_total_country <- df_services_data()
                       
                       total_country <-
                         aggregate(
                           df_total_country$Trade_Value,
                           list(
                             df_total_country$Partner,
                             df_total_country$Partner_ISO
                           ),
                           FUN = sum
                         )
                       names(total_country) <-
                         c("Partner", "ISO3", "tradevalue")
                       
                       total_country$Short_Description <-
                         'Total Services'
                       
                       if (!is.null(filter_value())) {
                         country <- df_services_data()
                         
                         names(country) <-
                           c(
                             "EBOP_Code",
                             "Description",
                             "Short_Description",
                             "Partner",
                             "ISO3",
                             "tradevalue",
                             "Year"
                           )
                         
                         
                         df_filtered_country <-
                           filter(country, Short_Description == filter_value())
                         
                         if (nrow(df_filtered_country) >= 1) {
                           df_country <- df_filtered_country
                           shinyjs::show(id = "ServicesButton")
                           
                         }
                         else{
                           df_country <- total_country
                           shinyjs::hide(id = "ServicesButton")
                           
                         }
                         
                       }
                       
                       else{
                         df_country <- total_country
                         shinyjs::hide(id = "ServicesButton")
                         
                       }
                       
                       df_country
                       
                     }
                     
                   }) %>% bindCache(
                     reporter_iso_sel(),
                     services_tradeflow(),
                     input$barchart_clicked_data,
                     input$reset_filter
                   )
                   
                   # Database connection for services text data
                   df_narrativeText <- reactive({
                     dbGetQuery(
                       con,
                       paste0(
                         "select text_category, HTML_text
                         from Dashboard_Narrative
                         where project_name = 'Country Profile'
                         and section_code = 'TS01'"
                         
                       )
                     )
                   })
                   
                   observe({
                     
                     df_Service <- df_services_data()
                     # Hide Services Section
                     if (nrow(df_services_data()) < 1)
                     {
                       shinyjs::hide(id = "ServicesBox")
                       shinyjs::hide(id = "ServicesBox2")
                     }
                     else{
                       shinyjs::show(id = "ServicesBox")
                       shinyjs::show(id = "ServicesBox2")
                     }
                     
                   })
                   
                   # Add Country name to services section title
                   output$Title <- renderText({
                     if (nrow(df_services_data()) >= 1) {
                       HTML(
                         paste(
                           "Trade in Services | <strong style='color:#0291da'>",
                           reporter(),
                           "</strong>"
                         )
                       )
                     }
                     
                   })
                   
                   output$BarTF <- renderText({
                     HTML(paste(services_tradeflow(), "of Services"))
                   }) %>% bindCache(services_tradeflow())
                   
                   output$MapTF <- renderText({
                     if (nrow(df_services_data()) >= 1) {
                       df_country <- df_countries()
                       
                       HTML(
                         paste(
                           services_tradeflow()
                           ,
                           "Markets | ",
                           "<strong style='color: #0291da; /*font-size: 14px'*/>",
                           df_countries()$Short_Description[1],
                           "</strong>"
                         )
                       )
                       
                       
                       
                     }
                     
                   })  %>% bindCache(
                     reporter_iso_sel(),
                     services_tradeflow(),
                     input$barchart_clicked_data,
                     input$reset_filter
                   )
                   
                   output$barchart <- renderEcharts4r({
                     if (nrow(df_services_data()) >= 1) {
                       df_Service <-
                         df_top_services()[order(df_top_services()$tradevalue), ]
                       
                       df_Service$tradevaluetxt <-
                         f_short_usdvaluetext(df_Service$tradevalue)
                       
                       df_Service$TTtrade_val <-
                         f_usdvaluetext(df_Service$tradevalue)
                       
                       df_Service$TTebop_code <-
                         df_Service$EBOP_Code
                       
                       df_Service$TTshort_description <-
                         df_Service$Short_Description
                       
                       df_Service$TTdescription <-
                         df_Service$Description
                       
                       df_Service %>%
                         e_charts(x = Short_Description) %>%
                         e_bar(
                           tradevalue,
                           colorMappingBy = 'Short_Description',
                           legend = FALSE,
                           name = "",
                           label = list(
                             show = TRUE,
                             color = "black",
                             textBorderColor = "transparent",
                             fontSize = 14,
                             position = "right",
                             formatter = htmlwidgets::JS(
                               "function(params)
                                 {
                                return(params.data.tradetxt.tradevaluetxt)
                                }"
                             )
                           )
                         ) %>%
                         e_add_nested('tradetxt', tradevaluetxt) %>%
                         e_add_nested('code', TTebop_code) %>%
                         e_add_nested('name', TTshort_description) %>%
                         e_add_nested('desc', TTdescription) %>%
                         e_add_nested('trade', TTtrade_val) %>%
                         e_tooltip(
                           confine = TRUE,
                           extraCssText = 'width:auto; white-space:pre-wrap;',
                           formatter = htmlwidgets::JS(
                             "
                                        function(params){

                                        return(
                                        '<strong>EBOP Code: </strong>' + params.data.code.TTebop_code +
                                        '<br/><strong>Service Description: </strong>'   + params.data.desc.TTdescription +
                                        '<br/><strong>Trade Value: </strong>' + params.data.trade.TTtrade_val )    }  "
                           )
                         ) %>%
                         e_add_nested("itemStyle", color) %>%
                         e_flip_coords() %>%
                         e_y_axis(
                           splitLine = list(show = FALSE),
                           axisTick = list(show = FALSE),
                           axisLine = list(show = FALSE),
                           formatter = htmlwidgets::JS(
                             "
                                          function(value){ if (value == 'Personal, Cultural and Recreational'){
                                            return ('Personal, Cultural') + `\n` + 'and Recreational'
                                          }
                                          else if(value == 'Insurance and Pension'){
                                            return ('Insurance') + `\n` + 'and Pension'
                                          }
                                          else{
                                            return(value)
                                          }}
                                                         "
                           )
                         ) %>%
                         e_x_axis(show = FALSE) %>%
                         e_animation(show = FALSE) %>%
                         e_grid(
                           right = "20%",
                           left = "35%",
                           bottom = '5%',
                           top = '5%'
                         )
                     }
                     
                   })  %>% bindCache(
                     reporter_iso_sel(),
                     services_tradeflow(),
                     input$barchart_clicked_data,
                     input$reset_filter
                   )
                   
                   output$map <- renderLeaflet({
                     if (nrow(df_services_data()) >= 1)
                     {
                       z <- df_countries()
                       
                       z$trade <- f_usdvaluetext(z$tradevalue)
                       
                       z$labels <-
                         with(
                           z,
                           paste("<strong>", Partner, "</strong></br>", trade) %>%
                             lapply(htmltools::HTML)
                         )
                       
                       lab <- HTML(paste(z$labels))
                       
                       sPDF <- joinCountryData2Map(z
                                                   , joinCode = "ISO3"
                                                   , nameJoinColumn = "ISO3")
                       
                       sPDF <-
                         sp.na.omit(sPDF, "tradevalue", margin = 1)
                       
                       pal <- colorNumeric(
                         palette = c(
                           '#fff5f0',
                           '#fee0d2',
                           '#fcbba1',
                           '#fc9272',
                           '#fb6a4a',
                           '#ef3b2c',
                           '#cb181d',
                           '#a50f15',
                           '#67000d'
                         ),
                         domain = z$tradevalue
                       )
                       
                       z <- z[order(z$tradevalue), ]
                       
                       factop <- function(x) {
                         ifelse(is.na(x), 0, 1)
                       }
                       
                       leaflet(options = leafletOptions(
                         attributionControl = FALSE,
                         zoomControl = TRUE,
                         zoomSnap = 0.1
                       )) %>%
                         addTiles(urlTemplate = "https://api.mapbox.com/styles/v1/bholee/cl75rvfqs002q14o0rwzd6oe5/tiles/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYmhvbGVlIiwiYSI6ImNrN2tibG9pNzAwajMzbWw4ZnlpcDNqY2wifQ.o-qJAmRdkh-McoubI4E2DA"
                                  ,
                                  options = tileOptions(minZoom = 1,
                                                        maxZoom = 2.5)) %>%
                         setView(lng = 0,
                                 lat = 20,
                                 zoom = 1.5) %>%
                         setMaxBounds(
                           lng1 = 58.995311187950925
                           ,
                           lat1 =  -174.0234375
                           ,
                           lng2 = -58.995311187950925
                           ,
                           lat2 = 223.2421875
                         ) %>%
                         addPolygons(
                           data = sPDF,
                           weight = 1.5,
                           fillColor = ~ pal(tradevalue) ,
                           color = "white",
                           fillOpacity = 1,
                           highlight = highlightOptions(fillColor = '#1c5d99'),
                           label = ~ labels,
                           labelOptions = labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px"),
                             textsize = "15px",
                             direction = "auto"
                           )
                         ) %>%
                         
                         addLegendNumeric(
                           pal = pal,
                           values = z$tradevalue,
                           title = 'Trade Value',
                           numberFormat = function(x) {
                             f_short_usdvaluetext(x)
                           },
                           position = 'bottomright',
                           orientation = 'horizontal',
                           shape = 'rect',
                           decreasing = FALSE,
                           height = 10,
                           width = 150,
                           tickLength = 0,
                           tickWidth = 0
                         )
                       
                     }
                     
                   })  %>% bindCache(
                     reporter_iso_sel(),
                     services_tradeflow(),
                     input$barchart_clicked_data,
                     input$reset_filter
                   )
                   
                   output$ServicesText <- renderText({
                     if (nrow(df_services_data()) >= 1) {
                       if (services_tradeflow() == 'Export') {
                         filter_value <- reactiveVal(value = NULL)
                         
                         df_narrative <- df_narrativeText()
                         
                         export_narrative <-
                           df_narrative$HTML_text[df_narrative$text_category == 'Export']
                         
                         df_sorted <-
                           df_top_services()[order(-df_top_services()$tradevalue), ]
                         
                         if (nrow(df_sorted) >= 3)
                         {
                           services_text <-
                             export_narrative %>% str_replace_all(
                               c(
                                 "YEAR" = df_sorted$Year[[1]],
                                 "REPORTER" = reporter(),
                                 "SERVICE1" = df_sorted$Description[[1]],
                                 "VALUE1" = f_usdvaluetext(df_sorted$tradevalue[[1]]),
                                 "SERVICE2" = df_sorted$Description[[2]],
                                 "VALUE2" = f_usdvaluetext(df_sorted$tradevalue[[2]]),
                                 "SERVICE3" =  df_sorted$Description[[3]],
                                 "VALUE3" = f_usdvaluetext(df_sorted$tradevalue[[3]])
                               )
                             )
                         } else if (nrow(df_sorted) == 2)
                         {
                           services_text <-
                             export_narrative %>% str_replace_all(
                               c(
                                 "YEAR" = df_sorted$Year[[1]],
                                 "REPORTER" = reporter(),
                                 "SERVICE1" = df_sorted$Description[[1]],
                                 "VALUE1" = f_usdvaluetext(df_sorted$tradevalue[[1]]),
                                 "SERVICE2" = paste("and", df_sorted$DESCRIPTION[[2]], sep = " "),
                                 "VALUE2" = f_usdvaluetext(df_sorted$tradevalue[[2]]),
                                 ", and SERVICE3 VALUE3" = ""
                               )
                             )
                         } else if (nrow(df_sorted) == 1)
                         {
                           services_text <-
                             export_narrative %>% str_replace_all(
                               c(
                                 "YEAR" = df_sorted$Year[[1]],
                                 "REPORTER" = reporter(),
                                 "SERVICE1" = df_sorted$Description[[1]],
                                 "VALUE1" = f_usdvaluetext(df_sorted$tradevalue[[1]]),
                                 ", SERVICE2 VALUE2, and SERVICE3 VALUE3" = ""
                               )
                             )
                         }
                         
                       }
                       
                       else if (services_tradeflow() == 'Import') {
                         filter_value <- reactiveVal(value = NULL)
                         
                         df_narrative <- df_narrativeText()
                         
                         import_narrative <-
                           df_narrative$HTML_text[df_narrative$text_category == 'Import']
                         
                         df_sorted <-
                           df_top_services()[order(-df_top_services()$tradevalue), ]
                         
                         if (nrow(df_sorted) >= 3)
                         {
                           services_text <-
                             import_narrative %>% str_replace_all(
                               c(
                                 "YEAR" = df_sorted$Year[[1]],
                                 "REPORTER" = reporter(),
                                 "SERVICE1" = df_sorted$Description[[1]],
                                 "VALUE1" = f_usdvaluetext(df_sorted$tradevalue[[1]]),
                                 "SERVICE2" = df_sorted$Description[[2]],
                                 "VALUE2" = f_usdvaluetext(df_sorted$tradevalue[[2]]),
                                 "SERVICE3" =  df_sorted$Description[[3]],
                                 "VALUE3" = f_usdvaluetext(df_sorted$tradevalue[[3]])
                               )
                             )
                         } else if (nrow(df_sorted) == 2)
                         {
                           services_text <-
                             import_narrative %>% str_replace_all(
                               c(
                                 "YEAR" = df_sorted$Year[[1]],
                                 "REPORTER" = reporter(),
                                 "SERVICE1" = df_sorted$Description[[1]],
                                 "VALUE1" = f_usdvaluetext(df_sorted$tradevalue[[1]]),
                                 "SERVICE2" = paste("and", df_sorted$DESCRIPTION[[2]], sep = " "),
                                 "VALUE2" = f_usdvaluetext(df_sorted$tradevalue[[2]]),
                                 ", and SERVICE3 VALUE3" = ""
                               )
                             )
                         } else if (nrow(df_sorted) == 1)
                         {
                           services_text <-
                             import_narrative %>% str_replace_all(
                               c(
                                 "YEAR" = df_sorted$Year[[1]],
                                 "REPORTER" = reporter(),
                                 "SERVICE1" = df_sorted$Description[[1]],
                                 "VALUE1" = f_usdvaluetext(df_sorted$tradevalue[[1]]),
                                 ", SERVICE2 VALUE2, and SERVICE3 VALUE3" = ""
                               )
                             )
                         }
                         
                       }
                       
                       HTML(paste(services_text,
                                  sep = " "))
                     }
                     
                   })  %>% bindCache(reporter_iso_sel(),
                                     services_tradeflow(),
                                     input$barchart_clicked_data)
                   
                   
                 })
    
  }

Services_tradeflow_server <-
  function(id) {
    moduleServer(id,
                 
                 function(input, output, session) {
                   services_tradeflow <- reactive({
                     input$ServiceTradeflow
                   })
                   return(services_tradeflow)
                 })
    
  }