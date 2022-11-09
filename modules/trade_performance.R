trade_performance_ui <- function(id) {
  ns <- NS(id)
  
  tagList(fluidRow(column(
    8,
    offset = 2,
    shinydashboard::box(
      solidHeader = TRUE,
      column(8, align = 'left', htmlOutput(outputId = ns("Title"),container = tags$h4,
                                           class = "title")),
      br(),
      br(),
      br(),
      column(8,
             align = 'left',
             echarts4rOutput(
               outputId = ns("tradeperformance")#, width = "100%"
             ))
      ,
      column(4,
             align = 'left',
             htmlOutput(outputId = ns("tptext"))),
      br(),
      br(),
      column(
        12,
        align = 'left',
        h5(
          "Note: The change and growth rates are calculated for the last 5 years of available trade data for the country. The blue dotted lines represent the average growth and market share."
        )
      ),
      width = 16
    )
  )))
}

trade_performance_server <-
  function(id, reporter_iso_sel, reporter, mycolor) {
    moduleServer(id,
                 
                 function(input, output, session) {
                   df_trade_performance <-
                     reactive({
                       reporter_iso <- reporter_iso_sel()
                       
                       df_tp <- dbGetQuery(
                         con,
                         paste(
                           "select Country_ISO,
Sector_Description,
AVG(Annual_Export_Growth_5y) y ,
AVG(World_Market_Share_Change_5y) x,
Year,
SUM(Trade_Value) tradevalue
from trade_performance tp, Sector s
where tp.HS2_Code = s.HS2_Code
and Sector_Description <> 'nes'
and Country_ISO = '",
                           reporter_iso,
                           "'
group by Country_ISO, Sector_Description,Year",
                           sep = ""
                         )
                       )
                       
                       df_tp
                     }) %>% bindCache(reporter_iso_sel())
                   
                   
                   # Output Title
                   output$Title <-
                     renderText({
                       HTML(
                         paste0(
                           "Trade Performance in Export <strong style='color:#0291da'>(",
                           (df_trade_performance()$Year[[1]] - 4),
                           " - ",
                           df_trade_performance()$Year[[1]],
                           ")</strong>"
                         )
                       )
                     }) %>% bindCache(reporter_iso_sel())
                   
                   output$tradeperformance <- renderEcharts4r({
                     mydata <- df_trade_performance() %>% filter(y < 10)
                     
                     mydata <- mydata[order(-mydata$tradevalue), ]
                     
                     
                     
                     my_scale <- function(x)
                       scales::rescale(x, to = c(5, 80))
                     
                     
                     mydata$color <- mycolor()$color[1:nrow(mydata)]
                    # if (max(mydata[,4]) > 0.01)
                    # {
                    #   a<- 0.0499
                    # }
                    #  else
                    #  {
                    #    a<- 0.0199
                    #  }
                     
                     b<-100
                     avg_X <- mean(mydata[,4])
                     avg_X_P <- data.frame(avg_X*b)
                     #max_X <- max(mydata[,4]) + a
                     
                     avg_y <- mean(mydata[,3])
                     avg_y_P <-data.frame(avg_y*b)

                     #max_y <- max(mydata[,3])
                     
                     mydata %>% dplyr::mutate(
                       shadowBlur = c(3),
                       shadowOffsetX = c(0) ,
                       shadowOffsetY = c(0),
                       shadow_color = c(rgb(0, 0, 0, 0.3)),
                       opacity = c(0.8)
                     ) %>%
                       e_charts(x) %>%
                       e_scatter(
                         y,
                         tradevalue,
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
                                        params.data.sector.Sector_Description)
                                            }"
                           )
                         ),
                         labelLayout = list(hideOverlap = TRUE, dy = 0)
                       )  %>%
                       e_add_nested(
                         "itemStyle",
                         shadowBlur,
                         shadowOffsetX,
                         shadowOffsetY,
                         shadow_color,
                         opacity,
                         color
                       ) %>%
                       e_add_nested('sector', Sector_Description) |>
                       e_tooltip(
                         trigger= 'item',
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
                                        '<strong>Sector: </strong>' + params.data.sector.Sector_Description +
                                        '<br/><strong>Change in world market share (Last 5 years): </strong>' + (params.value[0] * 100).toFixed(2) + '%' +
                                        '<br/><strong>Country export growth (Last 5 years): </strong>' +  (params.value[1] * 100).toFixed(2) + '%' +
                                        '<br/><strong>Exports: </strong>' + '$' +  nFormatter(params.value[2]) )   }  "
                         )
                       ) %>% e_hide_grid_lines() %>%
                       e_y_axis(max= htmlwidgets::JS('function (value) {
                         if (Math.abs(value.max) > Math.abs(value.min))
                                              {
                                              a = Math.abs(value.min)
                                              }
                                              else
                                              {
                                              a = Math.abs(value.max)
                                              }
                         
                         return value.max + a}'), min= htmlwidgets::JS('function (value) {
                               if (Math.abs(value.max) > Math.abs(value.min))
                                              {
                                              a = Math.abs(value.min)
                                              }
                                              else
                                              {
                                              a = Math.abs(value.max)
                                              }
                         return value.min - a}'),
                         axisLine = list(lineStyle = list(color = "#F0F0F0")),
                         axisLabel = list(color = "#000000",
                                          formatter = htmlwidgets::JS(
                                            "
                                        function(params){

     return((params * 100).toFixed(1) +'%')
                                            }") ),
                         axisTick = list(show = FALSE),
                         name = "Country export growth (%) (Last 5 years)",
                         nameLocation = "middle",
                         nameGap = 60,
                         nameTextStyle = list(
                           color = "#000000",
                           fontWeight = "bold",
                           # padding = 50,
                           fontSize = 14),
                         nameRotate = 90
                       ) %>%
                       e_x_axis(
                         max= htmlwidgets::JS('function (value) {
                         if (Math.abs(value.max) > Math.abs(value.min))
                                              {
                                              a = Math.abs(value.min)
                                              }
                                              else
                                              {
                                              a = Math.abs(value.max)
                                              }
                         
                         return value.max + a}'), min= htmlwidgets::JS('function (value) {
                               if (Math.abs(value.max) > Math.abs(value.min))
                                              {
                                              a = Math.abs(value.min)
                                              }
                                              else
                                              {
                                              a = Math.abs(value.max)
                                              }
                         return value.min - a}'),
                         axisLine = list(lineStyle = list(color = "#F0F0F0")),
                         axisLabel = list(color = "#000000",
                                          formatter = htmlwidgets::JS(
                                            "
                                        function(params){

     return((params * 100).toFixed(1) +'%')
                                            }") ),
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
                       e_mark_line(lineStyle = list(color="#54c3ea", opacity= 0.8),emphasis = list(disabled = TRUE),label = list(triggerTooltip = FALSE,formatter = htmlwidgets::JS(
                         "
                                        function(params){
                         return ((params.value*100).toFixed(1)+'%')
                                        }"),
                         color = "#000000",triggerTooltip = FALSE),symbol = "none",silient= FALSE, data = list(xAxis =avg_X  )) %>%
                       e_mark_line(symbol = "none",emphasis = list(disabled = TRUE),
                                   label = list(triggerTooltip = FALSE, formatter =  htmlwidgets::JS(
                                     "
                                        function(params){
                         return ((params.value*100).toFixed(1)+'%')
                                        }"),
                                     color = "#000000",silient= FALSE),
                                   data = list(yAxis =avg_y )) %>%
                       e_legend(FALSE)
                   }) %>% bindCache(reporter_iso_sel())
                   
                   # Output the narrative text on the screen
                   output$tptext <-
                     renderText({
                       df_mydata <- df_trade_performance() %>% filter(y < 10)
                       
                       df_mydata <-
                         df_mydata[order(-df_mydata$tradevalue), ]
                       
                       df_mydata$textval <-
                         f_usdvaluetext(df_mydata$tradevalue)
                       sector1 <- df_mydata$Sector_Description[[1]]
                       
                       value1  <- df_mydata$textval[[1]]
                       value2  <-
                         paste0(round(df_mydata$y[[1]] * 100, 2), '%')
                       value3  <-
                         paste0(round(df_mydata$x[[1]] * 100, 2), '%')
                       
                       df_mydata <-
                         df_mydata[order(-df_mydata$y), ]
                       sector2 <- df_mydata$Sector_Description[[1]]
                       value4  <-
                         paste0(round(df_mydata$y[[1]] * 100, 2), '%')
                       
                       df_narrative <-
                         dbGetQuery(
                           con,
                           "select text_category, HTML_text from Dashboard_Narrative where project_name = 'Country Profile' and section_code = 'TP01'"
                         )
                       
                       narrative_text <-
                         df_narrative$HTML_text[[1]] %>% str_replace_all(
                           c(
                             "REPORTER" = reporter(),
                             "SECTOR1" = sector1,
                             "VALUE1" = value1,
                             "SECTOR2" = sector2,
                             "VALUE2" = value2,
                             "VALUE3" = value3,
                             "VALUE4" = value4
                           )
                         )
                       
                       HTML(narrative_text)
                     }) %>% bindCache(reporter_iso_sel())
                   
                   
                 })
    
  }