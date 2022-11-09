servicesperformance_ui <- function(id) {
  ns <- NS(id)
  tagList(useShinyjs(), fluidRow(column(
    8,
    offset = 2,
    shinydashboard::box(
      id = ns("SPBox"),
      solidHeader = TRUE,
      column(12,
             tags$div(
               checked = NA,
               list(
                 htmlOutput(
                   outputId = ns("Servicestitle"),
                   container = tags$h4,
                   class = "title"
                 ),
                 
                 echarts4rOutput(outputId = ns("ServicesPerformance")),
                 
                 htmlOutput(
                   outputId = ns("ServicesPerfText"),
                   container = tags$p,
                   class = "servicesnarrat"
                 )
                 
               )
               
             )),
      
      
      width = 16
    )
  )))
}

services_performance_server <-
  function(id,
           reporter_iso_sel,
           reporter,
           mycolor) {
    moduleServer(id,
                 function(input, output, session) {
                   df_services_performance <- reactive({
                     dbGetQuery(
                       con,
                       paste0(
                         "SELECT [Country_ISO],[Description], Short_Description,
                         [Annual_Export_Growth_5y] Export_Growth,
                         [World_Market_Share_Change_5y] Market_Share,
                         [Trade_Value], Year
                          FROM [Services_Performance]
                          WHERE [Country_ISO] = '",
                         reporter_iso_sel(),
                         "'"
                       )
                     )
                   }) %>% bindCache(reporter_iso_sel())
                   
                   observe({
                     # Services Performance data
                     df_SP_data <- df_services_performance()
                     # Hide Services Section
                     if (nrow(df_SP_data) < 1)
                     {
                       shinyjs::hide(id = "SPBox")
                     }
                     else{
                       shinyjs::show(id = "SPBox")
                     }
                     
                   })
                   
                   output$Servicestitle <- renderText({
                     if (nrow(df_services_performance()) >= 1)
                     {
                       mydata <- df_services_performance() %>% filter(Export_Growth < 10)
                       
                       latest_year <- mydata$Year[1]
                       earlies_year <- latest_year - 4
                       
                       HTML(
                         paste(
                           "Services Performance in Exports ",
                           "<strong style='color: #0291da; /*font-size: 14px'*/>(",
                           earlies_year,
                           " - ",
                           latest_year,
                           ")</strong>"
                           ,
                           sep = ""
                         )
                       )
                     }
                     
                   }) %>% bindCache(reporter_iso_sel())
                   
                   # Display scatter chart
                   output$ServicesPerformance <- renderEcharts4r({
                     if (nrow(df_services_performance()) >= 1) {
                       mydata <- df_services_performance() %>% filter(Export_Growth < 10)
                       
                       mydata <-
                         mydata[order(-mydata$Trade_Value),]
                       
                       
                       my_scale <- function(Market_Share)
                         scales::rescale(Market_Share, to = c(5, 80))
                       
                       latest_year <- mydata$Year[1]
                       mydata$latest_year <- latest_year
                       
                       earlies_year <- latest_year - 4
                       mydata$earlies_year <- earlies_year
                       
                       mydata$color <-
                         mycolor()$color[1:nrow(mydata)]
                       
                       b <- 100
                       avg_X <- mean(mydata[, 5])
                       avg_X_P <- data.frame(avg_X * b)
                       
                       avg_y <- mean(mydata[, 4])
                       avg_y_P <- data.frame(avg_y * b)
                       
                       
                       mydata %>% dplyr::mutate(
                         shadowBlur = c(3),
                         shadowOffsetX = c(0) ,
                         shadowOffsetY = c(0),
                         shadow_color = c(rgb(0, 0, 0, 0.2)),
                         opacity = c(0.8)
                       ) %>%
                         e_charts(Market_Share) %>%
                         e_scatter(
                           Export_Growth,
                           Trade_Value,
                           scale = my_scale,
                           label = list(
                             show = TRUE,
                             color = "black",
                             textBorderColor = "transparent",
                             fontSize = 10,
                             position = "right",
                             formatter = htmlwidgets::JS(
                               "function(params){
                                                                        return(
                                        params.data.Sservice.Short_Description)
                                            }"
                             )
                           ),
                           labelLayout = list(hideOverlap = TRUE, dy = 0)
                         ) %>%
                         e_mark_line(
                           lineStyle = list(color = "#54c3ea", opacity = 0.8),
                           emphasis = list(disabled = TRUE),
                           label = list(
                             triggerTooltip = FALSE,
                             formatter = htmlwidgets::JS(
                               "
                                        function(params){
                         return ((params.value*100).toFixed(1)+'%')
                                        }"
                             ),
                             color = "#000000",
                             triggerTooltip = FALSE
                           ),
                           symbol = "none",
                           emphasis = list(disabled = TRUE),
                           silient = TRUE,
                           data = list(xAxis = avg_X)
                         ) %>%
                         e_mark_line(
                           symbol = "none",
                           label = list(
                             triggerTooltip = FALSE,
                             formatter =  htmlwidgets::JS(
                               "
                                        function(params){
                         return ((params.value*100).toFixed(1)+'%')
                                        }"
                             ),
                             color = "#000000",
                             silient = TRUE
                           ),
                           data = list(yAxis = avg_y)
                         ) %>%
                         e_add_nested(
                           "itemStyle",
                           shadowBlur,
                           shadowOffsetX,
                           shadowOffsetY,
                           shadow_color,
                           opacity,
                           color
                         ) %>%
                         e_add_nested('service', Description) |>
                         e_add_nested('Sservice', Short_Description) |>
                         e_add_nested('e_year', earlies_year) |>
                         e_add_nested('l_year', latest_year) |>
                         e_tooltip(
                           formatter = htmlwidgets::JS(
                             "
                                        function(params){

                                        function nFormatter(num){
                                         if (num >= 1000000000) {
                                            return (num / 1000000000).toFixed(1).replace(/0$/, '') + 'B';
                                         }
                                         if (num >= 1000000) {
                                            return (num / 1000000).toFixed(1).replace(/0$/, '') + 'M';
                                         }
                                         if (num >= 1000) {
                                            return (num / 1000).toFixed(1).replace(/0$/, '') + 'K';
                                         }
                                         return num;
                                        }

                                        return(
                                        '<strong>Service: </strong>' + params.data.service.Description +
                                        '<br/><strong>Change in world market share (Last 5 years): </strong>' + (params.value[0] * 100).toFixed(2) + '%' +
                                        '<br/><strong>Country export growth (Last 5 years): </strong>' +  (params.value[1] * 100).toFixed(2) + '%' +
                                        '<br/><strong>Exports: </strong>' + '$' +  nFormatter(params.value[2]) )   }  "
                           )
                         ) %>%
                         e_hide_grid_lines() %>%
                         e_y_axis(
                           axisLine = list(lineStyle = list(color = "#F0F0F0")),
                           axisLabel = list(
                             color = "#000000",
                             formatter = htmlwidgets::JS(
                               "
                                        function(params){

                                           return((params * 100).toFixed(1) +'%')
                                            }"
                             )
                           ),
                           axisTick = list(show = FALSE),
                           name = "Country export growth (%) (Last 5 years)",
                           nameLocation = "middle",
                           nameGap = 60,
                           nameTextStyle = list(
                             color = "#000000",
                             fontWeight = "bold",
                             fontSize = 14
                           ),
                           nameRotate = 90
                         ) %>%
                         e_x_axis(
                           axisLine = list(lineStyle = list(color = "#F0F0F0")),
                           axisLabel = list(
                             color = "#000000",
                             formatter = htmlwidgets::JS(
                               "
                                        function(params){

                                           return((params * 100).toFixed(1) +'%')
                                            }"
                             )
                           ),
                           axisTick = list(show = FALSE),
                           name = "Change in world market share (%) (Last 5 years)",
                           nameLocation = "middle",
                           nameTextStyle = list(
                             color = "#000000",
                             fontWeight = "bold",
                             padding = 20,
                             fontSize = 14
                           )
                         ) %>%
                         e_legend(FALSE) %>%
                         e_grid(right = "10%",
                                left = "7%")
                       
                     }
                   }) %>% bindCache(reporter_iso_sel())
                   
                   # Display details text
                   output$ServicesPerfText <- renderText({
                     if (nrow(df_services_performance()) >= 1) {
                       df_mydata <- df_services_performance() %>% filter(Export_Growth < 10)
                       
                       df_mydata <-
                         df_mydata[order(-df_mydata$Trade_Value), ]
                       
                       df_mydata$textval <-
                         f_usdvaluetext(df_mydata$Trade_Value)
                       
                       SERVICE1 <- df_mydata$Description[[1]]
                       
                       VALUE1 <- df_mydata$textval[[1]]
                       XGROWTH1 <-
                         paste0(round(df_mydata$Export_Growth[[1]] * 100, 2), '%')
                       MSHARE1 <-
                         paste0(round(df_mydata$Market_Share[[1]] * 100, 2), '%')
                       
                       df_mydata <-
                         df_mydata[order(-df_mydata$Export_Growth), ]
                       SERVICE2 <- df_mydata$Description[[1]]
                       XGROWTH2 <-
                         paste0(round(df_mydata$Export_Growth[[1]] * 100, 2), '%')
                       
                       df_narrative <- dbGetQuery(
                         con,
                         "select HTML_text
                                from Dashboard_Narrative
                                where project_name = 'Country Profile'
                                and section_code = 'SP01'"
                       )
                       
                       narrative <-
                         df_narrative$HTML_text[[1]] %>% str_replace_all(
                           c(
                             "REPORTER" = reporter(),
                             "SERVICE1" = SERVICE1,
                             "VALUE1" = VALUE1,
                             "SERVICE2" = SERVICE2,
                             "XGROWTH1" = XGROWTH1,
                             "MSHARE1" = MSHARE1,
                             "XGROWTH2" = XGROWTH2
                           )
                         )
                       HTML(narrative)
                       
                     }
                     
                   }) %>% bindCache(reporter())
                   
                   
                 })
  }
