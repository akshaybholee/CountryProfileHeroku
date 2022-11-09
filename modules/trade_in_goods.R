tradeingoods_ui <- function(id) {
  ns <- NS(id)
  
  tagList(fluidRow(column(
    8,
    offset = 2,
    shinydashboard::box(
      solidHeader = TRUE,
      column(8, align = 'left', htmlOutput(outputId = ns("country"))),
      column(
        4,
        align = 'right',
        selectInput(
          inputId =  ns("Tradeflow"),
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
      solidHeader = TRUE,
      column(
        8,
        htmlOutput(outputId = ns("treetitle")),
        echarts4rOutput(outputId = ns("treemap"), height = "450px"),
        hidden(actionLink(
          inputId = ns("hs2level"), "< Back to Sector level"
        ))
      ),
      column(
        4,
        htmlOutput(outputId = ns("bartitle")),
        echarts4rOutput(outputId = ns("barchart"), height = "460px")
      ),
      br(),
      br(),
      column(12, htmlOutput(outputId = ns("titext"))),
      width = 16
    )
  )))
  
}

tradeingoods_server <-
  function(id,
           reporter_iso_sel,
           reporter,
           mycolor,
           tradeflow) {
    moduleServer(id,
                 
                 function(input, output, session) {
                   print("start trade in goods")
                   prod <- df_product
                   names(prod) <- c('name', 'description')
                   
                   treemapclick <- reactiveValues(var = NULL)
                   button <- reactiveValues(var = NULL)
                   sectorclick <- reactiveValues(var = NULL)
                   
                   #get year of data
                   year <- reactive({
                     dbGetQuery(
                       con,
                       paste0(
                         "select Latest_Year from Tracking_Trade_Data_Availability where Reporter_ISO = '",
                         reporter_iso_sel(),
                         "'"
                       )
                     )
                   }) %>% bindCache(reporter_iso_sel())
                   
                   df_top_partner_load <- reactive({
                     dbGetQuery(
                       con,
                       paste0(
                         "select top 15 Partner_iso Country_ISO, sum(Trade_value) tradevalue
   from Product_Trade
   where Reporter_ISO = '",
                         reporter_iso_sel(),
                         "' and trade_flow = '",
                         tradeflow(),
                         "' and Partner_iso <> 'WLD'
   group by Partner_iso
   order by 2 desc"
                       )
                     )
                   })  %>% bindCache(reporter_iso_sel(), tradeflow())
                   
                   df_top_partner <- reactive({
                     if (is.null(sectorclick$var)) {
                       df_partner <- df_top_partner_load()
                       
                       
                     }
                     else if (is.na(as.numeric(sectorclick$var)) &
                              is.na(as.numeric(treemapclick$var)))
                     {
                       df_partner <-  dbGetQuery(
                         con,
                         paste0(
                           "select top 15 Partner_iso Country_ISO, sum(Trade_value) tradevalue
                     from Product_Trade
                     where Reporter_ISO = '",
                           reporter_iso_sel(),
                           "' and trade_flow = '",
                           tradeflow(),
                           "' and left(Product_Code,2) in ( select hs2_code from sector where sector_description = '",
                           input$treemap_clicked_data$name,
                           "') and Partner_iso <> 'WLD'
                     group by Partner_iso
                     order by 2 desc"
                         )
                       )
                     }
                     else if (nchar(treemapclick$var) == 2) {
                       df_partner <-  dbGetQuery(
                         con,
                         paste0(
                           "select top 15 Partner_iso Country_ISO, sum(Trade_value) tradevalue
                     from Product_Trade
                     where Reporter_ISO = '",
                           reporter_iso_sel(),
                           "' and trade_flow = '",
                           tradeflow(),
                           "' and left(Product_Code,2) = '",
                           input$treemap_clicked_data$name,
                           "' and Partner_iso <> 'WLD'
                     group by Partner_iso
                     order by 2 desc"
                         )
                       )
                     }
                     else if (nchar(treemapclick$var) == 4) {
                       df_partner <-  dbGetQuery(
                         con,
                         paste0(
                           "select top 15 Partner_iso Country_ISO, sum(Trade_value) tradevalue
                     from Product_Trade
                     where Reporter_ISO = '",
                           reporter_iso_sel(),
                           "' and trade_flow = '",
                           tradeflow(),
                           "' and left(Product_Code,4) = '",
                           input$treemap_clicked_data$name,
                           "' and Partner_iso <> 'WLD'
                     group by Partner_iso
                     order by 2 desc"
                         )
                       )
                     }
                     else if (nchar(treemapclick$var) == 6) {
                       df_partner <-  dbGetQuery(
                         con,
                         paste0(
                           "select top 15 Partner_iso Country_ISO, sum(Trade_value) tradevalue
                     from Product_Trade
                     where Reporter_ISO = '",
                           reporter_iso_sel(),
                           "' and trade_flow = '",
                           tradeflow(),
                           "' and Product_Code = '",
                           input$treemap_clicked_data$name,
                           "' and Partner_iso <> 'WLD'
                     group by Partner_iso
                     order by 2 desc"
                         )
                       )
                     }
                     
                     
                     df_partner$tradevaluetxt <-
                       f_short_usdvaluetext(df_partner$tradevalue)
                     
                     df_partner_merge <-
                       merge(x = df_partner, y = df_country, by = "Country_ISO")
                     
                     df_partner_merge <-
                       df_partner_merge[order(df_partner_merge$tradevalue),]
                     
                     df_partner_merge$color <- '#0291da'
                     
                     df_partner_merge
                     
                   }) %>% bindCache(reporter_iso_sel(), tradeflow(),  input$treemap_clicked_data$name)
                   
                   
                   df_product_load <- reactive({
                    x <- dbGetQuery(
                       con,
                       paste0(
                         "select left(Product_Code,2) HS2,s.sector_description sector, sum(Trade_value) trade_value
                         from Product_Trade pt,
						 sector s where left(pt.Product_Code,2) = s.HS2_code
						  and Reporter_ISO = '",
                         reporter_iso_sel(),
                         "' and trade_flow = '",
                         tradeflow(),
                         "' and Partner_iso = 'WLD' and Product_code <> '999999' group by left(Product_Code,2),s.sector_description
   order by 3 desc"
                       ),
                       as.is = TRUE
                     )
                    
                     x
                   }) %>% bindCache(reporter_iso_sel(), tradeflow())
                   
                   
                   df_hc_products <- reactive({
                     if (is.null(sectorclick$var) |
                         button$var)
                     {
                       x <- df_product_load() %>%
                         select(c('sector', 'trade_value'))
                       
                       x$trade_value <- as.numeric(x$trade_value)
                       
                       x <-
                         as.data.frame(xtabs(trade_value ~ sector, x))
                       
                       # x$trade_value <- as.numeric(x$trade_value)
                       
                       names(x) <-
                         c('name',  'value')
                       
                       y <- x
                       
                       shinyjs::hide("hs2level")
                       
                     }
                     else if (is.na(as.numeric(sectorclick$var)) &
                              is.na(as.numeric(treemapclick$var)))
                     {
                       x <-
                         df_product_load() %>% filter(sector == input$treemap_clicked_data$name) %>% select(c('HS2', 'trade_value'))
                       x$Trade_value <- as.numeric(x$trade_value)
                       
                       names(x) <-
                         c('name',  'value')
                       
                       y <- x
                       shinyjs::show("hs2level")
                       
                       # treemapclick$var <- NULL
                       
                     }
                     else if (nchar(treemapclick$var) == 2)
                     {
                      y <- dbGetQuery(
                         con,
                         paste0(
                           "select left(Product_Code,4) HS4,sum(Trade_value) trade_value
                         from Product_Trade where Reporter_ISO = '",
                           reporter_iso_sel(),
                           "' and trade_flow = '",
                           tradeflow(),
                           "' and Partner_iso = 'WLD' and Product_code <> '999999'
                           and left(Product_Code,2) = '",
                           input$treemap_clicked_data$name,
                           "'
                           group by left(Product_Code,4)
   order by 2 desc"
                         ),
                         as.is = TRUE
                       )
                       
                       
                       
                       shinyjs::show("hs2level")
                     }
                     else if (nchar(treemapclick$var) == 4)
                     {
                      y <- dbGetQuery(
                         con,
                         paste0(
                           "select Product_Code ,sum(Trade_value) trade_value
                         from Product_Trade where Reporter_ISO = '",
                           reporter_iso_sel(),
                           "' and trade_flow = '",
                           tradeflow(),
                           "' and Partner_iso = 'WLD' and Product_code <> '999999'
                           and left(Product_Code,4) = '",
                           input$treemap_clicked_data$name,
                           "'
                           group by Product_Code
   order by 2 desc"
                         ),
                         as.is = TRUE
                       )
                       
                       
                       
                       shinyjs::show("hs2level")
                     }
                     else if (nchar(treemapclick$var) == 6)
                     {
                     y <-  dbGetQuery(
                         con,
                         paste0(
                           "select Product_Code ,sum(Trade_value) trade_value
                         from Product_Trade where Reporter_ISO = '",
                           reporter_iso_sel(),
                           "' and trade_flow = '",
                           tradeflow(),
                           "' and Partner_iso = 'WLD' and Product_code <> '999999'
                           and Product_Code = '",
                           input$treemap_clicked_data$name,
                           "'
                           group by Product_Code
   order by 2 desc"
                         ),
                         as.is = TRUE
                       )
                       
                      
                       
                       
                       shinyjs::show("hs2level")
                     }
                     
                     
                     if (nrow(y) == 0)
                     {
                       y <- x
                       treemapclick$var <- NULL
                       sectorclick$var <- NULL
                       button$var <-
                         0
                       shinyjs::hide("hs2level")
                     }
                     
                     names(y) <- c('name', 'value')
                     y$valtxt <- f_short_usdvaluetext(as.numeric(y$value))
                     
                     if (is.null(sectorclick$var))
                     {
                       df <- y
                       df$description <- y$name
                     }
                     else
                     {
                       df <- merge(x =  y , y = prod, by = "name")
                     }
                     
                     
                     df
                     
                   }) %>% bindCache(reporter_iso_sel(), tradeflow(), sectorclick$var, treemapclick$var)
                   
                   
                   observe({
                     treemapclick$var <- input$treemap_clicked_data$name
                     sectorclick$var <-
                       input$treemap_clicked_data$name
                   })
                   
                   observe({
                     if (input$hs2level)
                     {
                       treemapclick$var <- NULL
                       sectorclick$var <-  NULL
                       button$var <-
                         0
                     } else {
                       treemapclick$var <- input$treemap_clicked_data$name
                       sectorclick$var <-
                         input$treemap_clicked_data$name
                       button$var <- input$hs2level
                     }
                   })
                   
                   
                   # Output the country name
                   output$country <-
                     renderText({
                       HTML(
                         paste(
                           "<h4><strong>Trade in Goods | </strong></h4>&nbsp<strong style='color:#0291da'>",
                           reporter(),
                           "</strong>"
                         )
                       )
                     }) %>% bindCache(reporter())
                   
                   
                   tooltip <-   reactive({
                     if (is.null(sectorclick$var))
                     {
                       tooltip <- " function(params){

                                        return(
                                        '<strong>Sector: </strong>' + params.data.HS.name +
                                        '<br/><strong>Trade Value: </strong>' + params.data.tradetxt.valtxt )    }  "
                     }
                     else
                     {
                       tooltip <- "function(params){

                                        return(
                                        '<strong>Product Code: </strong>' + params.data.HS.name +
                                        '<br/><strong>Product Description: </strong>'   + params.data.Desc.description +
                                        '<br/><strong>Trade Value: </strong>' + params.data.tradetxt.valtxt )    }  "
                     }
                     tooltip
                   })
                   
                   
     
                   
                   output$treemap <- renderEcharts4r({
                     df_hc_products() |>
                       e_charts() |>
                       e_grid(height = "120%") |>
                       e_treemap(
                         nodeClick = FALSE,
                         breadcrumb = list(show = FALSE),
                         roam = FALSE,
                         width = "100%",
                         # height = "100%",
                         colorMappingBy = 'name',
                         label = list(
                           show = TRUE,
                           color = NULL,
                           formatter = htmlwidgets::JS(
                             'function(params){
                                                                        return(
                                        params.data.Desc.description + `\n\n` +
                                        params.data.tradetxt.valtxt)
                                            }'
                           )
                         ),
                         itemStyle = list(borderColor = '#fff',
                                          borderWidth = 1)
                       ) |>
                       e_add_nested('tradetxt', valtxt) |> 
                       e_add_nested('HS', name) |> e_add_nested('Desc', description) |>
                       e_tooltip(
                         confine = TRUE,
                         extraCssText = 'width:auto; white-space:pre-wrap;',
                         formatter = htmlwidgets::JS(tooltip())
                       ) |> e_visual_map(
                         inRange = list(
                           color = c(
                             '#c3ffff',
                             '#a7f0f8',
                             '#8be2f2',
                             '#6fd2ee',
                             '#54c3ea',
                             '#38b3e5',
                             '#1ca2e0',
                             '#0291da'
                           )
                         ),
                         calculable = TRUE,
                         min = 0,
                           # min(as.numeric(df_hc_products()$value)),
                         max = max(as.numeric(df_hc_products()$value)),
                         type = 'continuous',
                         orient = 'horizontal',
                         align = 'left',
                         top = "90%",
                         show = TRUE,
                         itemHeight = 300,
                         formatter = htmlwidgets::JS(
                           "
                                        function(value){

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

                                        return nFormatter(value)
                                      }  "
                         )
                       )
                     
                   }) %>% bindCache(reporter_iso_sel(), tradeflow(), sectorclick$var, treemapclick$var)
                   
                   
                   
                   output$treetitle <- renderText({
                     origin <-
                       HTML(paste0(
                         '<strong>',
                         tradeflow(),
                         's of Products in ',
                         year(),
                         '</strong>'
                       ))
                     
                     if (is.null(sectorclick$var) | button$var)
                     {
                       paste(origin,
                             HTML('<br><font size="-0.5">Total ',tradeflow(),': <strong>',f_short_usdvaluetext(sum(as.numeric(df_hc_products()$value))),'</strong></font>'),
                             HTML('<br><font size="-1"><em>Click on treemap boxes to drill down to HS2, HS4 and HS6 level</em></font>'))
                     }
                     else if (is.na(as.numeric(sectorclick$var)) &
                              is.na(as.numeric(treemapclick$var)))
                     {
                       paste(origin, HTML(
                         paste("<strong>|", "Sector:", treemapclick$var,"</strong>",
                               HTML('<br><font size="-0.5">Total ',tradeflow(),': <strong>',f_short_usdvaluetext(sum(as.numeric(df_hc_products()$value))),'</strong></font>'),
                               HTML('<br><font size="-1"><em>Click on treemap boxes to drill down to HS2, HS4 and HS6 level</em></font>'))
                       ))
                     }
                     else
                     {
                       paste(origin, HTML(
                         paste("<strong>|", "HS Code:", treemapclick$var,"</strong>",
                               HTML('<br><font size="-0.5">Total ',tradeflow(),': <strong>',f_short_usdvaluetext(sum(as.numeric(df_hc_products()$value))),'</strong></font>'),
                               HTML('<br><font size="-1"><em>Click on treemap boxes to drill down to HS2, HS4 and HS6 level</em></font>'))
                       ))
                     }
                   }) %>%  bindCache(reporter_iso_sel(), tradeflow(), sectorclick$var, treemapclick$var)
                   
                   
                   
                   output$barchart <- renderEcharts4r({
                     e_charts(data = df_top_partner(), x = Short_Name) %>%
                       e_bar(
                         tradevalue,
                         legend = FALSE,
                         name = "",
                         label = list(
                           show = TRUE,
                           color = "black",
                           textBorderColor = "transparent",
                           fontSize = 11,
                           position = "right",
                           formatter = htmlwidgets::JS(
                             "function(params){
                                                                        return(
                                        params.data.tradetxt.tradevaluetxt)
                                            }"
                           )
                         )
                       ) %>%
                       e_add_nested('tradetxt', tradevaluetxt, ) %>%
                       e_tooltip(
                         formatter = htmlwidgets::JS(
                           "function(params){
                                                                        return(
                                    '<strong>' + params.value[1] + '</strong>' + ' ' +   params.data.tradetxt.tradevaluetxt)
                                            }"
                         )
                       ) %>%
                       e_flip_coords() %>%
                       e_y_axis(
                         splitLine = list(show = FALSE),
                         axisTick = list(show = FALSE),
                         axisLine = list(show = FALSE)
                       ) %>%
                       e_x_axis(show = FALSE) %>%
                       e_grid(
                         right = "20%",
                         bottom = '0%',
                         top = '0%',
                         containLabel = TRUE
                       ) %>%
                       e_add_nested("itemStyle",
                                    color)
                   })  %>% bindCache(reporter_iso_sel(), tradeflow(), sectorclick$var, treemapclick$var)
                   
                   output$bartitle <- renderText({
                     if (nrow(df_top_partner()) == 15)
                     {
                       HTML(paste0(
                         '<strong>',
                         'Top 15 ',
                         tradeflow() ,
                         ' markets in ',
                         year()
                       ))
                     }
                     else
                     {
                       HTML(paste0(
                         '<strong>',
                         tradeflow() ,
                         ' markets in ',
                         year()
                       ))
                     }
                   }) %>% bindCache(tradeflow(), year())
                 
                   
                   # Output the narrative text on the screen
                   
                   df_narrative_1 <- dbGetQuery(
                     con,
                     "select text_category, HTML_text from Dashboard_Narrative where project_name = 'Country Profile' and section_code = 'TIG01'
                           and text_category = 'Text1'"
                   )
                   
                   df_narrative_2 <- dbGetQuery(
                     con,
                     "select text_category, HTML_text from Dashboard_Narrative where project_name = 'Country Profile' and section_code = 'TIG01'
                           and text_category = 'Text2'"
                   )
                   
                   output$titext <-
                     renderText({
                       # year <- year()
                       
                       if (tradeflow() == 'Export')
                       {
                         tradeflow <- 'exported'
                         fromto <- 'to'
                         tradeflow1 <- 'export'
                       }
                       else {
                         tradeflow <- 'imported'
                         fromto <- 'from'
                         tradeflow1 <- 'import'
                       }
                       
                       
                       # get top product
                       df_top_product <- df_product_load() %>%
                         select(c('sector', 'trade_value'))
                       
                       df_top_product$trade_value <-
                         as.numeric(df_top_product$trade_value)
                       
                       df_top_product <-
                         as.data.frame(xtabs(trade_value ~ sector, df_top_product))
                       
                       names(df_top_product) <- c('name', 'value')
                       
                       df_top_product <-
                         df_top_product[order(-df_top_product$value), ]
                       
                       
                       df_top_product <- df_top_product[1,]
                       
                       
                       df_first_text <- df_top_product
                       df_first_text$description <-
                         df_first_text$name
                       
                       rownames(df_first_text) <- c()
                       
                       
                       
                       #get top partner
                       df_partner <- df_top_partner_load()[1,]
                       
                       df_partner_merge <-
                         merge(x = df_partner, y = df_country, by = "Country_ISO")
                       
                       
                       
                       val1 <-
                         f_usdvaluetext(as.numeric(df_first_text$value[[1]]))
                       prod1 <-
                         as.character(df_first_text$name[[1]])
                       
                       partner1 <-
                         df_partner_merge$Country[[1]]
                       val2 <-
                         f_usdvaluetext(df_partner_merge$tradevalue[[1]])
                      
                      
                       
                       narrative_text_1 <-
                         df_narrative_1$HTML_text[[1]] %>% str_replace_all(
                           c(
                             "REPORTER" = reporter(),
                             "YEAR" = year()$Latest_Year[[1]],
                             "VAL1" = val1,
                             "TRADEFLOW1" = tradeflow1,
                             "TRADEFLOW" = tradeflow,
                             "PROD1" = prod1,
                             "FROMTO" = fromto,
                             "PARTNER1" = partner1,
                             "VAL2" = val2
                           )
                         )
                       
                       if (!is.null(treemapclick$var)) {
                         if (is.na(as.numeric(sectorclick$var)))
                         {
                           prod2 <- input$treemap_clicked_data$description
                         }
                         else
                         {
                           prod2 <-
                             paste0(
                               input$treemap_clicked_data$description,
                               ' (HS ',
                               input$treemap_clicked_data$name,
                               ')'
                             )
                         }
                         # hs2 <- input$treemap_clicked_data$name
                         
                         partner2 <-
                           df_top_partner()$Short_Name[[nrow(df_top_partner())]]
                         
                         val3 <-
                           f_usdvaluetext(df_top_partner()$tradevalue[[nrow(df_top_partner())]])
                     
                         
                         narrative_text_2 <-
                           df_narrative_2$HTML_text[[1]] %>% str_replace_all(
                             c(
                               "TRADEFLOW1" = tradeflow1,
                               "TRADEFLOW" = tradeflow,
                               "PROD2" = prod2,
                               "FROMTO" = fromto,
                               "PARTNER2" = partner2,
                               "VAL3" = val3
                             )
                           )
                       } else {
                         narrative_text_2 <- ""
                       }
                       
                       
                       HTML(paste0('<br/>', narrative_text_1, '<br/>', narrative_text_2))
                       
                     })  %>% bindCache(reporter_iso_sel(), tradeflow(), sectorclick$var, treemapclick$var)
                   
                 })
    
  }

tradeflow_server <-
  function(id) {
    moduleServer(id,
                 
                 function(input, output, session) {
                   tradeflow <- reactive({
                     input$Tradeflow
                   })
                   return(tradeflow)
                 })
    
  }