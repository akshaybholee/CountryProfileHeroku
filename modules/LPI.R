LPI_ui <- function(id) {
  ns <- NS(id)
  
  tagList(useShinyjs(), fluidRow(column(
    8, offset = 2, shinydashboard::box(id=ns("LPIBox"),
      solidHeader = TRUE,
      column(
        7,
        h4(strong("Logistic Performance Index")),
        br(),
        #offset = 1,
        align = 'left',
        #, class = "text_down",
        htmlOutput(outputId = ns("LPItext"))
      ),
      column(5,
             #offset = 1,
             #height = 900,
             #align = 'right',
             class = "text_padding",
             echarts4rOutput(outputId = ns("displayradar"),width = "105%", height = 500)
             ),
      width = 16
    )
  )))
}


LPI_server <-
  function(id,
           reporter_iso_sel,
           reporter,
           mycolor) {
    moduleServer(id,
                 
                 function(input, output, session) {
                   
                   
                   df_Logistic_Performance_Index <-
                     reactive({
                       reporter_iso <- reporter_iso_sel()
                       
                       df_LPI <- dbGetQuery(
                         con,
                         paste(
                           "SELECT [Year]
      ,[LPI_Category]
      ,[Score]
  FROM [dbo].[Logistic_Performance_Index]
  where country_ISO = '",reporter_iso,"'",
                           sep = ""
                         )
                       )
                       
                       df_LPI
                     }) %>% bindCache( reporter_iso_sel())

                   observe(
                                {
                                  if (nrow(df_Logistic_Performance_Index())==0)
                                  {
                                    shinyjs::hide(id = "LPIBox")
                                  }
                                  
                   }
                   )
                   
                   
                   output$displayradar <- renderEcharts4r({
                     req(nrow(df_Logistic_Performance_Index())>0)
                     
                     df_LPI <- df_Logistic_Performance_Index()
                     
                     
                     
                     
                     
                     barcolor <- data.frame(color = c(
                       '#fbfbfb','#fbfbfb','#fbfbfb','#fbfbfb','#fbfbfb','#fbfbfb','#fbfbfb','#fbfbfb','#fbfbfb',
                       '#fbfbfb','#fbfbfb','#fbfbfb','#fbfbfb','#0291da'))
                    #  #test
                     rifo <- lapply(df_LPI$LPI_Category, function(x) {
                       list(height = 40, 
                            backgroundColor=list(image=paste0('https://iecstorageaccount.blob.core.windows.net/json/',x,'.png')),align = "center")
                    
                   })
                     
                     df_LPI$LPI_Category <- gsub(" ", "", df_LPI$LPI_Category)
                     
                   names(rifo) <- df_LPI$LPI_Category
                   
                
                   
                
                     df_LPI |> 
                       e_charts(LPI_Category) |> 
                       e_radar(Score, min = 1, max = 5, symbolSize= 5, 
                               label = list(show= TRUE, color= "black", textBorderColor= "transparent",fontSize=13,fontWeight= 'bold' ),
                               itemStyle = list(color = "#0291da", shadowColor ="black", shadowBlur = 10),
                               lineStyle= list(width= 1, color= "#37A2DA", join= "round"),title = list(show= FALSE),
                               areaStyle= list(color=htmlwidgets::JS(
                                 "new echarts.graphic.LinearGradient(
    0, 1, 0, 0,
    [
      { offset: 0, color: '#8be2f2' },
      { offset: 1, color: '#38b3e5' }
    ])"
                               ),shadowColor ="black", shadowBlur = 15, opacity= 0.9 ),legend = list(show= FALSE),
                               #series-line = list(type= "line", color = "#0291da"),
                               radar = list(axisTick = list(show = FALSE),symbol= 'circle',
                                            fillArea=TRUE,
                                            center = c("50%", "50%"),
                                            radius = "60%",
                                            axisLabel = list(show = TRUE, hideOverlap = TRUE, showMinLabel = FALSE, showMaxLabel = TRUE,
                                                             color= htmlwidgets::JS("function(params) {
                                var colorList = ['blue']
                                return colorList['blue']
                                }")), 
                                            splitNumber=14,
                                            splitLine = list(lineStyle=list(color=barcolor$color , width= 1,
                                                                            shadowColor ="black", shadowBlur = 0,
                                                                            type = "solid", cap= "round", dashOffset= 50,
                                                                            join = "miter",miterLimit=20, opacity = 0.1)),
                                            splitArea= list(areaStyle= list(color= barcolor$color, opacity = 0.8,shadowColor =htmlwidgets::JS("function(params) {
                                var colorList = ['blue']
                                return colorList['blue']
                                }"), shadowBlur = 15)),
                                            shape = "circle",
                                            
                                            axisLine = list(show = TRUE,
                                                            lineStyle = list(color= "#AAA9AD", width= 0.6,shadowColor ="black", shadowBlur = 10,
                                                                             type = "dashed", cap= "round", dashOffset= 50,
                                                                             join = "round", opacity = 0.4)) )) |>
                       e_tooltip(trigger = "item", formatter =  htmlwidgets::JS('function (params) {
                         return(params)}')) |>
                       e_radar_opts(         
                         name = list(color = "#18191A", overflow = "break", fontSize=13
                         ,formatter = htmlwidgets::JS("
        function(value){ if (value == 'Internationalshipments')
                                                                                                {
                    return (  '{' + 'Internationalshipments' + '| }') + `\n\n` + 'International' +`\n` + 'shipments  ' }
                    
               else if (value == 'Trackingandtracing')
                                                                                                                                  {
                    return (  '{' + 'Trackingandtracing' + '| }') + `\n\n` + '   Tracking ' + ` \n` + ' and tracing' }
                    
                         else if (value == 'Customs')
                                                                                                                                  {
                    return (  '{' + 'Customs' + '| }') + `\n\n` + 'Customs' }
                    
                                else if (value == 'Infrastructure')
                                                                                                                                  {
                    return (  '{' + 'Infrastructure' + '| }') + `\n\n` + 'Infrastructure' }
                    
                                else if (value == 'Logisticsqualityandcompetence')
                                                                                                                                  {
                    return  ' Logistics quality ' + `\n` + ' and competence' + `\n\n` + '{' + 'Logisticsqualityandcompetence' + '| }' }
                    
                                else if (value == 'Timeliness')
                                                                                                                                  {
                    return (  '{' + 'Timeliness' + '| }') + `\n\n` + 'Timeliness' }
                    }
                                                      "),
                         
                           
                                     rich = rifo
        ))
                     
                   }) %>% bindCache( reporter_iso_sel())
                   
                   
                   
                   
                   output$LPItext <-
                     renderText({
                       req(nrow(df_Logistic_Performance_Index())>0)
                       
                       df_LPI <- df_Logistic_Performance_Index()
                       LPI_text <- ""
                       
                       
                       MAX_LPI <- max(df_LPI[,3]) 
                       
                       MIN_LPI <- min(df_LPI[,3]) 
                       
                       df_MAX_dim<- filter(df_LPI, Score == MAX_LPI) 
                       
                       df_MIN_dim  <- filter(df_LPI, Score == MIN_LPI) 
                       
                       
                       df_LPI_narrative <-
                         dbGetQuery(
                           con,
                           "select text_category, HTML_text from Dashboard_Narrative where project_name = 'Country Profile' and section_code = 'LPI01'"
                         )
                       
                       
                       LPI_narrative <-
                         df_LPI_narrative$HTML_text[df_LPI_narrative$text_category == 'LPI score']
                       
                       if (nrow(df_MAX_dim) == 1 & nrow(df_MIN_dim) == 1)
                       {
                         LPI_text <-
                           LPI_narrative %>% str_replace_all(
                             c(
                               
                               "DIMENSION1" = paste(df_MAX_dim$LPI_Category),
                               "YEAR" = df_LPI[1,1],
                               "VALUE1" = MAX_LPI,
                               "COUNTRY" = reporter(),
                               "DIMENSION2" = paste (df_MIN_dim$LPI_Category),
                               "VALUE2" = MIN_LPI
                             )
                           )
                       }
                       else  if (nrow(df_MAX_dim) > 1 & nrow(df_MIN_dim) == 1)
                       {
                         
                         lastcell <-tail(df_MAX_dim$LPI_Category, n = 1)
                         df_MAX_dim[df_MAX_dim == lastcell] <-paste("and",lastcell)
                         
                         df_MAX_dim_merged <- df_MAX_dim %>%
                           dplyr::group_by(Year) %>%
                           dplyr::summarise(LPI_Category = paste(LPI_Category, collapse = ", "))
                         
                         LPI_text <-
                           LPI_narrative %>% str_replace_all(
                             c(
                               "COUNTRY" = reporter(),
                               "DIMENSION1" = paste(df_MAX_dim_merged$LPI_Category) ,
                               "YEAR" = df_LPI[1,1],
                               "VALUE1" = MAX_LPI,
                               "DIMENSION2" = paste(df_MIN_dim$LPI_Category),
                               "VALUE2" = MIN_LPI
                             )
                           )
                         
                       }
                       
                       else  if (nrow(df_MAX_dim) == 1 & nrow(df_MIN_dim) > 1)
                       {
                         
                         lastcell <-tail(df_MIN_dim$LPI_Category, n = 1)
                         df_MIN_dim[df_MIN_dim == lastcell] <-paste("and",lastcell)
                         
                         
                         df_MIN_dim_merged <- df_MIN_dim %>%
                           dplyr::group_by(Year) %>%
                           dplyr::summarise(LPI_Category = paste(LPI_Category, collapse = ", "))
                         
                         LPI_text <-
                           LPI_narrative %>% str_replace_all(
                             c(
                               "COUNTRY" = reporter(),
                               "DIMENSION1" = paste(df_MAX_dim$LPI_Category) ,
                               "YEAR" = df_LPI[1,1],
                               "VALUE1" = MAX_LPI,
                               "DIMENSION2" = paste(df_MIN_dim_merged$LPI_Category),
                               "VALUE2" = MIN_LPI
                             )
                           )
                         
                       }
                       
                       else  if (nrow(df_MAX_dim) > 1 & nrow(df_MIN_dim) > 1)
                       {
                         
                         lastcell <-tail(df_MAX_dim$LPI_Category, n = 1)
                         df_MAX_dim[df_MAX_dim == lastcell] <-paste("and",lastcell)
                         
                         lastcell <-tail(df_MIN_dim$LPI_Category, n = 1)
                         df_MIN_dim[df_MIN_dim == lastcell] <-paste("and",lastcell)
                         
                         
                         df_MIN_dim_merged <- df_MIN_dim %>%
                           dplyr::group_by(Year) %>%
                           dplyr::summarise(LPI_Category = paste(LPI_Category, collapse = ", "))
                         
                         df_MAX_dim_merged <- df_MAX_dim %>%
                           dplyr::group_by(Year) %>%
                           dplyr::summarise(LPI_Category = paste(LPI_Category, collapse = ", "))
                         
                         LPI_text <-
                           LPI_narrative %>% str_replace_all(
                             c(
                               "COUNTRY" = reporter(),
                               "DIMENSION1" = paste(df_MAX_dim_merged$LPI_Category) ,
                               "YEAR" = df_LPI[1,1],
                               "VALUE1" = MAX_LPI,
                               "DIMENSION2" = paste(df_MIN_dim$LPI_Category),
                               "VALUE2" = MIN_LPI
                             )
                           )
                         
                       }
                       
                       if (LPI_text != "")
                       {
                         HTML(paste(
                           df_LPI_narrative$HTML_text[df_LPI_narrative$text_category == 'General']
                           ,
                           LPI_text,
                           sep = ""
                         ))
                       }
                       
                     }) %>% bindCache( reporter())
                   
                   
                 })}