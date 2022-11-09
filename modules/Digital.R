Digital_ui <- function(id) {
  ns <- NS(id)
  
  tagList(useShinyjs(), fluidRow(column(
    8,
    offset = 2,
    shinydashboard::box( id = ns("Boxheader"),
         solidHeader = TRUE,
         column(4, align = 'left', h4(strong("Digital")))
         ,
         width = 16
    )
  )),fluidRow(column(
    8, offset = 2, shinydashboard::box(solidHeader = TRUE,
                       fluidRow(column(id=ns("DTR"),
                                       4,
                                       class = "digital_cards",
                                       align = 'left',
                                       
                                       #tags$head(tags$style(HTML(".small-box {height: 100px}"))),
                                       valueBoxOutput(ns("displayDTR"), width = 12)),
                                column(id=ns("GCI"),
                                       4,
                                       
                                       #offset = 1,
                                       align = 'left',
                                       class = "digital_cards",
                                       tags$head(tags$style(HTML(".small-box {height: 100px}"))),
                                       valueBoxOutput(ns("displaySecurity"), width = 12)),
                                
                                column(id=ns("NRI"),
                                       4,
                                       #offset = 1,
                                       class = "digital_cards",
                                       align = 'left',
                                       tags$head(tags$style(HTML(".small-box {height: 100px}"))),
                                       valueBoxOutput(ns("displayNRI"), width = 12))
                                
                       ),
                       fluidRow( column(12, class = "header_padding_1",class = "padding_right",id =ns("DTRItxt"),
                                        HTML(
                                          "The Digital Trade Restrictiveness Index identifies and categorizes trade barriers to digitally enabled services. The score ranges from 0 (completely open) to 1 (virtually restricted), with higher values representing higher levels of digital trade costs for businesses. The ranking is based on how costly their digital trade policy conditions are."
                                        )),br(),
                                 br(),
                                 br(),
                                 column(12, class = "header_padding_1",class = "padding_right",id =ns("GCItxt"),
                                        HTML(
                                          "The Global Cybersecurity Index ranks countries based on their commitment to cybersecurity on a global scale. The index score ranges from 0 to 100, with a higher score indicating greater commitment to cybersecurity."
                                        )),
                                 br(),
                                 br(),
                                 br(),
                                 column(12, class = "header_padding_1",class = "padding_right",id =ns("NRItxt"),
                                        HTML(
                                          "The Network Readiness Index (NRI) is a key indicator of how countries perform in the digital world, with a score ranging from 0 to 100, indicating worst to best performance."
                                        ))),
                     
                       fluidRow(id=ns("STRI"),
                                # column(5, class = "text_padding",
                                #                      align = 'left', h4(strong("Digital Services Trade Restrictiveness Index"))),
                                fluidRow(column(
                                  6,
                                  br(),
                                  h4(strong("Digital Services Trade Restrictiveness Index")),
                                  br(),
                                  br(),
                                  
                                  #offset = 1,
                                  align = 'left',
                                  class = "box_padding",
                                  #class = "text_down2",
                                  htmlOutput(outputId = ns("STRItext"))
                                ),
                                column(6,
                                       id=ns("STRI_chart"),
                                       #offset = 1,
                                       align = 'left',
                                       echarts4rOutput(outputId = ns("displaySTRI"))
                                )
                                )),
                       width = 16)
  )))
}


Digital_server <-
  function(id,
           reporter_iso_sel,
           reporter,
           mycolor) {
    moduleServer(id,
                 
                 function(input, output, session) {
                   
                   ###Digital Trade Restrictiveness####
                   df_Digital_Cards_data<-
                     reactive({
                       reporter_iso <- reporter_iso_sel()
                       
                       df_Digital_card <- dbGetQuery(
                         con,
                         paste(
                           "SELECT [Country_ISO]
      ,[Year]
      ,[Score]
      ,[Rank],
      'Global_Cybersecurity_Index' indicator
  FROM [dbo].[Global_Cybersecurity_Index] where [Country_ISO] ='",reporter_iso,"'
  union
  SELECT [Country_ISO]
      ,[Year]
      ,[Score]
      ,[Rank],
      'Network_Readiness_Index' indicator
  FROM [dbo].[Network_Readiness_Index] where [Country_ISO] ='",reporter_iso,"'
  union
  SELECT [Country_ISO]
      ,[Year]
      ,[Index_Value]
      ,[Rank],
      'Digital_TRI' indicator
  FROM [dbo].[Digital_TRI] where [Country_ISO] ='",reporter_iso,"'",
                           sep = ""
                         )
                       )
                       
                       df_Digital_card 
                     }) %>% bindCache(reporter_iso_sel())
                   
                   observe(
                     {
                       df_Digital_card <- df_Digital_Cards_data() 
                       df_DTR <- filter(df_Digital_card, indicator == "Digital_TRI")
                       
                       if (nrow(df_DTR)>0)
                       {
                         shinyjs::show(id = "DTR")
                         shinyjs::show(id = "DTRItxt")
                       }
                       else
                       {
                         shinyjs::hide(id = "DTRItxt")
                         shinyjs::hide(id = "DTR")
                         
                       }
                     }
                   )
                   
                   output$displayDTR <- renderValueBox({
                     
                     df_Digital_card <- df_Digital_Cards_data() 
                     df_DTR <- filter(df_Digital_card, indicator == "Digital_TRI")
                     
                     valueBox2(
                       value= tags$p(df_DTR$Score , style = "font-size: 250%;text-align: left;
                             position: absolute;
  top: 66px;
  left: 17px;
  font-size: 25px;
  font-weight: 900;"),
                       subtitle = tags$p("Score", style = "font-size: 300%;text-align: left;
                             position: absolute;
  top: 38px;
  left: 16px;
  font-size: 18px;
  font-weight: 300;"),
                       value1= tags$p(df_DTR$Rank, style = "font-size: 250%;text-align: left;
                             position: absolute;
  top: 66px;
  left: 122px;
  font-size: 25px;
  font-weight: 900;"),
                       subtitle1 = tags$p("Rank", style = "font-size: 300%;text-align: left;
                             position: absolute;
  top: 38px;
  left: 114px;
  font-size: 18px;
  font-weight: 300;"),
                       
                       year = tags$p("Year:", df_DTR$Year, style = "font-size: 300%;text-align: left;
                             position: absolute;
top: 76px;
  right: 18px;
  font-size: 12px;text-align: right;
  color:#F5C9C3;
  font-weight: 400;"),
                       
                       
                       title= tags$p("Digital Trade Restrictiveness Index", style = "position: absolute;
  top: 8px;
  left: 16px;
  font-size: 20px;text-align: left;
  font-weight: 400;"),
                       color = "red"
                     )
                     
                     
                   })
                   
                   
                   ###End of Digital Trade Restrictiveness####
                   
                   
                   
                   ###Global Cybersecurity Index####
                   
                   
                   
                   observe(
                     {
                       
                       df_Digital_card <- df_Digital_Cards_data() 
                       df_GCI <- filter(df_Digital_card, indicator == "Global_Cybersecurity_Index")
                       if (nrow(df_GCI)>0)
                       {
                         shinyjs::show(id = "GCI")
                         shinyjs::show(id = "GCItxt")
                       }
                       else
                       {
                         
                         shinyjs::hide(id = "GCI")
                         shinyjs::hide(id = "GCItxt")
                       }
                     }
                   )
                   
                   
                   output$displaySecurity <- renderValueBox({
                     
                     df_Digital_card <- df_Digital_Cards_data() 
                     df_GCI <- filter(df_Digital_card, indicator == "Global_Cybersecurity_Index")
                     
                     valueBox2(
                       value= tags$p(df_GCI$Score, style = "font-size: 250%;text-align: left;
                             position: absolute;
  top: 66px;
  left: 17px;
  font-size: 25px;
  font-weight: 900;"),
                       subtitle = tags$p("Score", style = "font-size: 300%;text-align: left;
                             position: absolute;
  top: 38px;
  left: 16px;
  font-size: 18px;
  font-weight: 300;"),
                       value1= tags$p(df_GCI$Rank, style = "font-size: 250%;text-align: left;
                             position: absolute;
  top: 66px;
  left: 122px;
  font-size: 25px;
  font-weight: 900;"),
                       subtitle1 = tags$p("Rank", style = "font-size: 300%;text-align: left;
                             position: absolute;
  top: 38px;
  left: 114px;
  font-size: 18px;
  font-weight: 300;"),
                       
                       year = tags$p("Year:", df_GCI$Year, style = "font-size: 300%;text-align: left;
                             position: absolute;
top: 76px;
  right: 18px;
  font-size: 12px;text-align: right;
  color:#FFDCBD;
  font-weight: 400;"),
                       
                       
                       title= tags$p("Global Cybersecurity Index", style = "position: absolute;
  top: 8px;
  left: 16px;
  font-size: 20px;text-align: left;
  font-weight: 400;"),
                       color = "orange"
                     )
                     
                     
                   })
                   
                   
                   
                   ###End of Global Cybersecurity Index####
                   
                   
                   
                   ###Network Readiness Index####
                   
                   
                   
                   observe(
                     {
                       
                       df_Digital_card <- df_Digital_Cards_data() 
                       df_NRI <- filter(df_Digital_card, indicator == "Network_Readiness_Index")
                       
                       if (nrow(df_NRI)>0)
                       {
                         shinyjs::show(id = "NRI")
                         shinyjs::show(id = "NRItxt")
                       }
                       else
                       {
                         
                         shinyjs::hide(id = "NRI")
                         shinyjs::hide(id = "NRItxt")
                         
                       }
                     }
                   )
                   
                   
                   
                   output$displayNRI <- renderValueBox({
                     
                     df_Digital_card <- df_Digital_Cards_data() 
                     df_NRI <- filter(df_Digital_card, indicator == "Network_Readiness_Index")
                     
                     valueBox2(
                       value= tags$p(df_NRI$Score, style = "font-size: 250%;text-align: left;
                             position: absolute;
  top: 66px;
  left: 17px;
  font-size: 25px;
  font-weight: 900;"),
                       subtitle = tags$p("Score", style = "font-size: 300%;text-align: left;
                             position: absolute;
  top: 38px;
  left: 16px;
  font-size: 18px;
  font-weight: 300;"),
                       value1= tags$p(df_NRI$Rank, style = "font-size: 250%;text-align: left;
                             position: absolute;
  top: 66px;
  left: 122px;
  font-size: 25px;
  font-weight: 900;"),
                       subtitle1 = tags$p("Rank", style = "font-size: 300%;text-align: left;
                             position: absolute;
  top: 38px;
  left: 114px;
  font-size: 18px;
  font-weight: 300;"),
                       
                       year = tags$p("Year:", df_NRI$Year, style = "font-size: 300%;text-align: left;
                             position: absolute;
top: 76px;
  right: 18px;
  font-size: 12px;text-align: right;
  color:#BDDCA8;
  font-weight: 400;"),
                       
                       
                       title= tags$p("Network Readiness Index", style = "position: absolute;
  top: 8px;
  left: 16px;
  font-size: 20px;text-align: left;
  font-weight: 400;"),
                       color = "green"
                     )
                     
                     
                     ###End of Network Readiness Index####
                     
                     
                   })
                   
                   
                   
                   df_service_trade_restrectiveness_index <-
                     reactive({
                       reporter_iso <- reporter_iso_sel()
                       
                       df_STRI <- dbGetQuery(
                         con,
                         paste(
                           "SELECT [Country_ISO],Year, [Index_Description]
      ,[Index_Value]
  FROM [dbo].[Digital_STRI]
                           where [Country_ISO]='",reporter_iso,"' and [Index_Description] <>  'Indicator STRI'",
                           sep = ""
                         )
                       )

                       df_STRI
                     }) %>% bindCache(reporter_iso_sel())
                   
                   
                   
                   observe(
                     {
                       
                       df_STRI <- df_service_trade_restrectiveness_index()
                       
                       #df_cat_0  <- filter(df_STRI, Index_Value == 0)
                       
                       if (nrow(df_service_trade_restrectiveness_index())>0)
                       {
                         shinyjs::show(id = "STRI")
                       }
                       else
                       {
                         
                         shinyjs::hide(id = "STRI")
                         
                       }
                       
                       # if (nrow(df_STRI) < 5)
                       # {
                       #   shinyjs::show(id = "STRI_chart")
                       # }
                       # else
                       # {
                       #   
                       #   shinyjs::hide(id = "STRI_chart")
                       #   
                       # }
                       
                       
                     }
                   )
                   
                   barcolor <- data.frame(color = c(
                     '#c3ffff','#f0bb91','#a8d383','#38b3e5','#0291da'))
                   
                   
                   output$displaySTRI <- renderEcharts4r({
                     
                     
                     req(nrow(df_service_trade_restrectiveness_index())>0)
                     
                     
                     df_STRI <- df_service_trade_restrectiveness_index()
                     
                     df_STRI$color <- c("#ffa600", "#0291da","#50c878","#ff7078", "#f0bb91")
                     
                     MAX_STRI <- max(df_STRI[,4])+0.05
                     #MAX_STRI <- MAX_STRI+0.05
                     #df_STRI <- df_STRI[df_STRI$Index_Value != 0,]
                     
                     req(nrow(df_STRI)>0)
                     #df_STRI<- df_STRI[order(df_STRI$Index_Value,decreasing = TRUE),]
                     
                     #df_sum <-data.frame(c("STRI indicator:", sum(df_STRI$Index_Value)))
                     df_STRI$Index_Value <- ((df_STRI$Index_Value - 1) * -1) + 0
                     
                     df_STRI[c(4,1,2,3,5),]
                     
                     df_STRI["Index_Description"][df_STRI["Index_Description"] == "Other barriers affecting trade in digitally enabled services"] <- "Other barriers affecting trade in\ndigitally enabled services\n"
                     df_STRI["Index_Description"][df_STRI["Index_Description"] == "Infrastructure and connectivity"] <- "Infrastructure and\nconnectivity     "
                     df_STRI["Index_Description"][df_STRI["Index_Description"] == "Electronic transactions"] <- " Electronic\ntransactions"
                     df_STRI["Index_Description"][df_STRI["Index_Description"] == "Payment system"] <- "Payment\nsystem "
                     df_STRI["Index_Description"][df_STRI["Index_Description"] == "Intellectual property rights"] <- "Intellectual property\n            rights"
                     #df_STRI$Index_Description <- paste(df_STRI$Index_Description, ": ",df_STRI$Index_Value)
                     
                     
                     df_STRI[c(4,5,2,3,1),] |>
                       e_charts(Index_Description) |> 
                       e_radar(Index_Value, max= 1, min= 0, symbolSize= 8,inverse=TRUE, 
                               label = list(show= TRUE, color= "black", textBorderColor= "transparent",fontSize=11,fontWeight= 'bold',ellipsis= "overflow" ),
                               title = list(show= FALSE),legend = list(show= FALSE),
                               lineStyle= list(width= 4, color= '#0291da', join= "round"),
                               #series-line = list(type= "line", color = "#0291da"),
                               areaStyle= list(color= "#AADDEC", opacity= 0.5 ),
                               radar = list(axisTick = list(show = FALSE),symbol= 'circle',   
                                            fillArea=TRUE,
                                            center = c("50%", "60%"),
                                            radius = "80%",
                                            axisName = list(color = "#18191A", overflow = "break", fontSize=13),
                                            axisLabel = list(show = FALSE, hideOverlap = TRUE, showMinLabel = FALSE, showMaxLabel = FALSE,fontSize=10),
                                            splitNumber=6,
                                            splitLine = list(lineStyle=list(color="white", width= 1,
                                                                                          shadowColor ="black", shadowBlur = 0.3,
                                                                                          type = "solid", cap= "round", dashOffset= 50,
                                                                                          join = "miter",miterLimit=20, opacity = 0.1)),
                                            splitArea= list(areaStyle= list(color= c('#DD4B39','#f0bb91','#ffefd2','#dfdfa4','#a8d383','#00A65A'), opacity = 0.8,shadowColor ='rgba(0, 0, 0, 0.2)', shadowBlur = 10)),

                                            axisLine = list(show = TRUE,
                                                            lineStyle = list(color= 'White', width= 0.6,shadowColor ="black", shadowBlur = 10,
                                                                             type = "dashed", cap= "round", dashOffset= 50,
                                                                             join = "round", opacity = 0.4))
                               )
                               ) |>
                       e_tooltip(trigger = "item", formatter = htmlwidgets::JS("
                       function(params){
                               return('<strong>Other barriers affecting trade in digitally enabled services: </strong>'+ params.value[0] + 
                              '<br /><strong>Payment system: </strong>' + params.value[1] + '<br /><strong>Infrastructure and connectivity: </strong>' + params.value[2] +
                              '<br /><strong>Intellectual property rights: </strong>' + params.value[3]  + '<br /><strong>Electronic transactions: </strong>' + params.value[4] )                                                
                       }"))
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                   #     e_charts(Index_Description) |> 
                   #     e_pie(Index_Value, radius = c("50%", "70%"),itemStyle= list(borderRadius= 10, borderColor= '#fff',
                   #                                                                 borderWidth= 2
                   #     )) |>
                   #     e_legend(show= FALSE)|>
                   #     e_title( text = paste(df_sum[1,1]) ,textStyle = list(fontWeight= 'normal'), left= 236,
                   #              top= 180,
                   #              bottom= 0  ) |>
                   #     e_title( text = paste(df_sum[2,1]),textStyle = list(fontWeight= 'bolder'), left= 275,  
                   #              top= 200,
                   #              bottom= 0  )  |>
                   #     e_grid(top= "0%") |>
                   #     e_add_nested(
                   #       "itemStyle",color) |>
                   #     e_add_nested('category', Index_Description) |>
                   #     e_add_nested('score', Index_Value) |>
                   #     e_tooltip(formatter = htmlwidgets::JS(
                   #       "
                   # function(params){
                   # 
                   # return(
                   # '<strong>Category: </strong>' + params.data.category.Index_Description +
                   #  '<br><strong>Score: </strong>'+ params.data.score.Index_Value)  }  "
                   #     )) 
                     # |>
                     #   e_labels(formatter = htmlwidgets::JS("
                     #     '{b}: {d}'"
                     #   ))
                     
                     
                     
                   }) %>% bindCache(reporter_iso_sel())
                   
                   output$STRItext <-
                     renderText({
                       req(nrow(df_service_trade_restrectiveness_index())>0)
                       
                       df_STRI <- df_service_trade_restrectiveness_index()
                       STRI_text <- ""
                       
                       
                       df_numeric <-data.frame(df_STRI[,c("Index_Value")])
                       MAX_STRI <- max(df_numeric[,1])
                       
                       MIN_STRI <- min(df_numeric[,1])
                       df_MAX_cat<- filter(df_STRI, Index_Value == MAX_STRI)
                       
                       df_MIN_cat  <- filter(df_STRI, Index_Value == MIN_STRI)
                       
                       Min_STRI_rev <- ((MAX_STRI - 1) * -1) + 0
                       
                       Max_STRI_rev <- ((MIN_STRI - 1) * -1) + 0
                       df_numeric_rev <- ((sum(df_STRI$Index_Value) - 5) * -1) + 0
                       
                       df_cat_0  <- filter(df_STRI, Index_Value == 0)
                       # df_cat_0$Index_Description <- paste0("<b>",df_cat_0$Index_Description,"</b>" )
                       # lastcell <-tail(df_cat_0$Index_Description, n = 1)
                       
                       df_STRI_narrative <- 
                         dbGetQuery(
                           con,
                           "select text_category, HTML_text from Dashboard_Narrative where project_name = 'Country Profile' and section_code = 'D01'"
                         )
                       
                       # df_STRI_narrative1 <- paste("<font size=+1>The Digital STRI indicator YEAR1 value for COUNTRY is Value0. It is completely open in terms of CAT3. With a score of VALUE1, COUNTRY has the most restriction in CAT1.") 
                       # df_STRI_narrative1 <- paste("<font size=+1>The Digital STRI indicator value for COUNTRY is Value0 in YEAR1 with the area CAT1 having the most restriction with a value of VALUE1 and its least restricted area being CAT2 with a value of VALUE2.")
                       # df_STRI_narrative2 <- paste("<font size=+1>In the Digital STRI YEAR1, Canada is completely open in all the five policy areas: CAT3.")
                       
                       if (nrow(df_cat_0) > 1 && nrow(df_cat_0) < 5)
                       {
                         
                         STRI_narrative <-
                           df_STRI_narrative$HTML_text[df_STRI_narrative$text_category == 'STRI_both']
                         
                         df_cat_0$Index_Description <- paste0("<b>",df_cat_0$Index_Description,"</b>" )
                         
                         lastcell <-tail(df_cat_0$Index_Description, n = 1)
                         df_cat_0[df_cat_0 == lastcell] <-paste("and",lastcell)
                         
                         df_cat_0_merged <- df_cat_0 %>%
                           dplyr::group_by(Year) %>%
                           dplyr::summarise(Index_Description = paste(Index_Description, collapse = ", "))
                         
                         
                         
                         STRI_text <-
                           STRI_narrative %>% str_replace_all(
                             c(
                               
                               "CAT1" = paste(df_MAX_cat$Index_Description),
                               "YEAR1" = paste(df_STRI[1,2]),
                               "VALUE1" = Min_STRI_rev,
                               "COUNTRY" = reporter(),
                               "CAT3" = paste (df_cat_0_merged$Index_Description),
                               "Value0"= df_numeric_rev
                             )
                           )
                       }
                       else if (nrow(df_cat_0) == 1 )
                       {
                         STRI_narrative <-
                           df_STRI_narrative$HTML_text[df_STRI_narrative$text_category == 'STRI_both']
                         
                         df_cat_0$Index_Description <- paste0("<b>",df_cat_0$Index_Description,"</b>" )
                         
                         STRI_text <-
                           STRI_narrative %>% str_replace_all(
                             c(
                               
                               "CAT1" = paste(df_MAX_cat$Index_Description),
                               "YEAR1" = paste(df_STRI[1,2]),
                               "VALUE1" = Min_STRI_rev,
                               "COUNTRY" = reporter(),
                               "CAT3" = paste (df_cat_0$Index_Description),
                               "Value0"= df_numeric_rev
                             )
                           )
                       }
                       
                       
                       
                       
                       else if (nrow(df_cat_0) == 5 )
                       {
                         
                         df_cat_0$Index_Description <- paste0("<b>",df_cat_0$Index_Description,"</b>" )
                         
                         lastcell <-tail(df_cat_0$Index_Description, n = 1)
                         df_cat_0[df_cat_0 == lastcell] <-paste("and",lastcell)
                         
                         df_cat_0_merged <- df_cat_0 %>%
                           dplyr::group_by(Year) %>%
                           dplyr::summarise(Index_Description = paste(Index_Description, collapse = ", "))
                         
                         
                         STRI_narrative <-
                           df_STRI_narrative$HTML_text[df_STRI_narrative$text_category == 'STRI_open']
                         
                         STRI_text <-
                           STRI_narrative %>% str_replace_all(
                             c(
                               
                               "YEAR1" = paste(df_STRI[1,2]),
                               "COUNTRY" = reporter(),
                               "CAT3" = paste (df_cat_0_merged$Index_Description)
                             )
                           )
                       }
                       
                       else if (nrow(df_cat_0) == 0 )
                       {
                         
                         
                         lastcell <-tail(df_MIN_cat$Index_Description, n = 1)
                         
                         
                         df_MIN_cat[df_MIN_cat == lastcell] <-paste("and",lastcell)
                         df_cat_MIN_merged <- df_MIN_cat %>%
                           dplyr::group_by(Year) %>%
                           dplyr::summarise(Index_Description = paste(Index_Description, collapse = ", "))
                         
                         STRI_narrative <-
                           df_STRI_narrative$HTML_text[df_STRI_narrative$text_category == 'STRI_close']
                         
                         STRI_text <-
                           STRI_narrative %>% str_replace_all(
                             c(
                               
                               "CAT1" = paste(df_MAX_cat$Index_Description),
                               "YEAR1" = paste(df_STRI[1,2]),
                               "VALUE1" = Min_STRI_rev,
                               "COUNTRY" = reporter(),
                               "CAT2" = paste (df_cat_MIN_merged$Index_Description),
                               "VALUE2" = Max_STRI_rev,
                               "Value0"= df_numeric_rev
                             )
                           )
                       }
                       
                       # df_STRI_narrative<- paste(STRI_text)
                       # if (STRI_text != "")
                       {
                         HTML(paste(df_STRI_narrative$HTML_text[df_STRI_narrative$text_category == 'STRI_general']
                                    ,
                                    STRI_text,
                                    sep = ""
                         ))
                       }
                       
                     } ) %>% bindCache(reporter())
                   
                   
                 })}