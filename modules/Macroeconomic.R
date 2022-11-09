Macroeconomic_ui <- function(id) {
  ns <- NS(id)
  
  tagList(fluidRow(column(
    8,
    offset = 2,
    shinydashboard::box( id = ns("Boxheader"),
         solidHeader = TRUE,
         column(4, align = 'left', h4(strong("Macroeconomic Indicators")))
         ,
         width = 16
    )
  )),
  fluidRow(column(
    8,
    offset = 2,
    shinydashboard::box(id = ns("Boxcharts"),
        solidHeader = TRUE,
        column(6,
               id = ns("columnEx"),
               #class = "text_padding",
               align = 'left', h4(strong("Exchange Rates")),
               echarts4rOutput(outputId = ns("displayExchangeRate"), height = 275),
               htmlOutput(outputId = ns("Exchangeratetext"))),
        
        column(6,
               id = ns("column1"), h4(strong("Real Interest Rate")),
               echarts4rOutput(outputId = ns("displayInterest"), height = 275),
               htmlOutput(outputId = ns("RealInteresttext")),      
               br(),
               br(),
               br()
        ),  
        
        column(6,
               align = 'left',
               id = ns("columnFDI"),
               #class = "header_padding_1", 
               h4( strong("Foreign Direct Investment, net (US$)")),
               #class = "text_padding",
               echarts4rOutput(outputId = ns("displayFDI"), height = 275),
               htmlOutput(outputId = ns("FDItext")
               ),
               br(),
               br()),
        column(6,
               id = ns("columnCAB"), align = 'left', h4(strong("Current Account Balance (US$)")),
               echarts4rOutput(outputId = ns("displayCurrentAcc"), height = 275),
               htmlOutput(outputId = ns("CurrentAcctext")), #class = "text_align", 
               br(),
               br(),
               br()
        ), 
        
        
        
        column(6,
               id = ns("columnGDPPC"),
               h4(strong("GDP Per Capita (Current US$)")),
               #class = "text_padding",
               echarts4rOutput(outputId = ns("displayGDPPC"), height = 275),
               htmlOutput(outputId = ns("GDPPCtext")),
               br(),
               br(),
               br(),
               br(),
               br()
        ),
        column(6,
               id = ns("columnGDP"),
               h4(strong("GDP (Current US$)")),
               #class = "text_padding",
               echarts4rOutput(outputId = ns("displayGDP"), height = 275),
               htmlOutput(outputId = ns("GDPtext")),
               br(),
               br(),
               br()
        ),
        
        width = 16,
        
        
    )
  ),
  )
  )
  
}

Macroeconomic_server <-
  function(id,
           reporter_iso_sel,
           reporter,
           mycolor) {
    moduleServer(id,
                 
                 function(input, output, session) {
                   
                   
                   options(scipen=999)
                   
                   df_Macroeconomic <-
                     reactive({
                       print("df_Macroeconomic")
                       reporter_iso <- reporter_iso_sel()
                       
                       df_Macro <- dbGetQuery(
                         con,
                         paste(
                           "SELECT[Year]
      ,[Current_Account_Balance]
      ,[Exchange_Rate]
      ,[FDI]
      ,[GDP_Per_Capital]
      ,[Interest_Rate]
      ,[GDP]
  FROM [dbo].[Macroeconomic]
                           where [Country_ISO]='",reporter_iso,"'",
                           sep = ""
                         )
                       )
                       
                       df_Macro
                     }) %>% bindCache(reporter_iso_sel())
                   
                   df_narrative <- reactive(
                     {
                       dbGetQuery(
                         con,
                     
                           "select Text_Category, HTML_text from Dashboard_Narrative where project_name = 'Country Profile' and section_code = 'M01'")
                     }
                   )
                   
                   
                   observe(
                     {
                       
                       
                       df_Macro <- df_Macroeconomic()
                       
                       
                       
                       
                       if (nrow(df_Macro)>0)
                       {
                         shinyjs::show(id = "Boxcharts")
                         shinyjs::show(id = "Boxheader")
                       }
                       else
                       {
                         
                         shinyjs::hide(id = "Boxcharts")
                         shinyjs::hide(id = "Boxheader")
                         
                       }
                     }
                   )
                   
                   ####Exchange rate####
                   
                   observe(
                     {
                       
                       
                       df_Macro <- df_Macroeconomic()
                       
                       
                       
                       df_Exchange <-data.frame(df_Macro[,c("Year", "Exchange_Rate")])
                       
                       df_Ex_Value <- df_Exchange %>% filter(!is.na(Exchange_Rate))
                       
                       if (nrow(df_Ex_Value)>1)
                       {
                         shinyjs::show(id = "columnEx")
                       }
                       else
                       {
                         shinyjs::hide(id = "columnEx")
                         
                       }
                     }
                   )
                   
                   
                   output$displayExchangeRate <- renderEcharts4r({
                     
                     df_Macro <- df_Macroeconomic()
                     
                     df_Exchange <-data.frame(df_Macro[,c("Year", "Exchange_Rate")])
                     
                     df_Ex_Value <- df_Exchange %>% filter(!is.na(Exchange_Rate))
                     
                     
                     
                     
                     
                     
                     df_Ex_Value |> 
                       e_charts(Year) |> 
                       e_line(Exchange_Rate, symbol = 'circle', symbolSize =5, lineStyle= list(shadowColor= 'rgba(0, 0, 0, 0.5)',shadowBlur= 10), 
                              areaStyle = list(opacity= 0.8, color =htmlwidgets::JS(
                                "new echarts.graphic.LinearGradient(
    0, 0, 0, 1,
    [
      { offset: 0, color: 'rgb(0, 221, 255)' },
      { offset: 1, color: '#38b3e5' }
    ])"
                              )),
                              xAxis =list(type = "category"), color = "#0291da",
                              legend = list(show =FALSE)) |>
                       e_tooltip(trigger = "item", axisPointer= list(type = 'shadow'), formatter = htmlwidgets::JS("
                       function(params){
                        function nFormatter(num){
                        if (params >= 100000000000){
     return((params / 1000000000000).toFixed(1)).replace(/.0$/, '') +'T'
     }
                       if (num >= 1000000000) {
       return((num / 1000000000).toFixed(1).replace(/.0$/, '')) + 'B';
     }
     if (num >= 1000000) {
        return ((num / 1000000).toFixed(1).replace(/.0$/, '')) + 'M';
     }
     if (num >= 1000) {
        return ((num / 1000).toFixed(1)).replace(/.0$/, '') + 'K';
     }
     if (num <1000){
     return ((num / 1).toFixed(1)).replace(/.0$/, '');
     }
     return (num) }
                       return('<strong>Year:</strong> ' + params.value[0] + '<br /><strong>Exchange Rate: </strong>' + nFormatter(params.value[1]))
                       }
                       "))|>
                       e_y_axis(splitLine= list(show =FALSE),
                                axisLabel = list(color = "#000000"),
                                axisTick = list(show = FALSE),
                                name = "Local currency units to US$",
                                nameLocation = "middle",
                                formatter = htmlwidgets::JS(
                                  "
                                        function(params){

          if (params >= 100000000000){
     return( + (params / 1000000000000).toFixed(1)).replace(/.0$/, '') +'T'
     }
     if (params >= 1000000000) {
        return ((params / 1000000000).toFixed(1)).replace(/.0$/, '') + 'B';
     }
     if (params >= 1000000) {
        return ((params / 1000000).toFixed(1)).replace(/.0$/, '') + 'M';
     }
     if (params >= 1000) {
        return ((params / 1000).toFixed(1)).replace(/.0$/, '') + 'K';
     }
     if (params <1000){
     return ((params / 1).toFixed(1)).replace(/.0$/, '');
     }
     return (params) }"),
                                nameTextStyle = list(
                                  color = "#000000",
                                  fontWeight = "bold",
                                  padding = 28,
                                  fontSize = 14
                                ),
                                nameRotate = 90)|>
                       e_grid(
                         top= '10%',
                         bottom = '22%',
                         right = '5%')|>
                       e_x_axis( scale = TRUE, max= htmlwidgets::JS('function (value) {
                         return value.max}'), min= htmlwidgets::JS('function (value) {
                         return value.min}'),
                                 splitLine= list(show =FALSE),
                                 axisLabel = list(show = TRUE,color = "#000000", position = 'middle',
                                                  formatter =  htmlwidgets::JS('function (params) {
                         return(params)}')),
                                 axisTick = list(show = TRUE, alignWithLabel= TRUE),
                                 name = "Year",
                                 nameLocation = "middle",
                                 nameTextStyle = list(
                                   color = "#000000",
                                   fontWeight = "bold",
                                   padding = 20,
                                   fontSize = 14
                                 ))
                     
                     
                     
                     
                     
                   }) %>% bindCache(reporter_iso_sel())
                   
                   
                   output$Exchangeratetext <-
                     renderText({
                       Exchange_rate_text <- ""
                       df_Macro <- df_Macroeconomic()
                       
                       df_Exchange <-data.frame(df_Macro[,c("Year", "Exchange_Rate")])
                       
                       df_Ex_Value <- df_Exchange %>% filter(!is.na(Exchange_Rate))
                       
                       req(nrow(df_Ex_Value)>0)
                       
                       
                       
                       
                       MAX_Exchange <- max(df_Ex_Value$Exchange_Rate, na.rm = TRUE )
                       MIN_Exchange <- min(df_Macro$Exchange_Rate, na.rm = TRUE )
                       Latest_year <- max(df_Ex_Value$Year, na.rm = TRUE )
                       df_Ex_MAx_year <- data.frame(df_Ex_Value$Year[df_Ex_Value$Exchange_Rate==MAX_Exchange])
                       df_Ex_MIN_year <- data.frame(df_Ex_Value$Year[df_Ex_Value$Exchange_Rate==MIN_Exchange])
                       
                       if (reporter() == 'United States of America' )
                       {
                         df_Exchange_narrative <- df_narrative() %>% filter(Text_Category == 'Exchange_USA')  %>% select(c('HTML_text'))
                           # dbGetQuery(
                           #   con,
                           #   "select HTML_text from Dashboard_Narrative where project_name = 'Country Profile' and section_code = 'M01' and Text_Category == 'Exchange_USA'"
                           # )
                         Exchange_rate_text <- df_Exchange_narrative
                         
                       }
                       else
                       {
                         df_Exchange_narrative <- df_narrative() %>% filter(Text_Category == 'Exchange_rate')  %>% select(c('HTML_text'))
                           # dbGetQuery(
                           #   con,
                           #   "select HTML_text from Dashboard_Narrative where project_name = 'Country Profile' and section_code = 'M01' and Text_Category == 'Exchange_rate'"
                           # )
                         
                         Exchange_rate_text <-
                           df_Exchange_narrative %>% str_replace_all(
                             c(
                               
                               "YEAR1" = paste(df_Ex_MAx_year[1,1]),
                               "VALUE1" = paste(MAX_Exchange),
                               "YEAR2" = paste(df_Ex_MIN_year[1,1]),
                               "VALUE2" = paste(MIN_Exchange),
                               "COUNTRY" = reporter(),
                               "YEAR3" = paste(Latest_year),
                               "VALUE3" = paste (df_Ex_Value$Exchange_Rate[df_Ex_Value$Year== Latest_year])
                             )
                           )
                         
                         
                       }
                       
                       Exchange_narrative<- paste(Exchange_rate_text)
                       
                       
                     })  %>% bindCache(reporter())
                   
                   ####End of Exchange rate####                  
                   
                   ####Current Account Balance####
                   
                   
                   observe(
                     {
                       
                       
                       df_Macro <- df_Macroeconomic()
                       
                       
                       
                       df_Current <-data.frame(df_Macro[,c("Year", "Current_Account_Balance")])
                       
                       df_Current_Value <- df_Current %>% filter(!is.na(Current_Account_Balance))
                       
                       if (nrow(df_Current_Value)>1)
                       {
                         shinyjs::show(id = "displayCurrentAcc")
                         shinyjs::show(id = "columnCAB")
                       }
                       else
                       {
                         
                         shinyjs::hide(id = "displayCurrentAcc")
                         shinyjs::hide(id = "columnCAB")
                         
                       }
                     }
                   )
                   
                   
                   
                   output$displayCurrentAcc <- renderEcharts4r({
                     
                     df_Macro <- df_Macroeconomic()
                     
                     df_Current <-data.frame(df_Macro[,c("Year", "Current_Account_Balance")])
                     
                     df_Current_Value <- df_Current %>% filter(!is.na(Current_Account_Balance))
                     
                     
                     df_Current_Value  |> 
                       e_charts(Year) |> 
                       e_line(Current_Account_Balance, symbol = 'circle', symbolSize =5, lineStyle= list(shadowColor= 'rgba(0, 0, 0, 0.5)',shadowBlur= 10),
                              areaStyle = list(opacity= 0.8, color =htmlwidgets::JS(
                                "new echarts.graphic.LinearGradient(
    0, 0, 0, 1,
    [
      { offset: 0, color: 'rgb(0, 221, 255)' },
      { offset: 1, color: '#38b3e5' }
    ])"
                              )),
                              xAxis =list(type = "category"), color = "#0291da",
                              legend = list(show =FALSE)) |>
                       e_grid(
                         top= '10%',
                         bottom = '22%',
                         right = '5%')|>
                       e_tooltip(trigger = "item",axisPointer = list(show = TRUE, type = 'shadow', axis = 'x'), formatter = htmlwidgets::JS("
                       function(params){
                        function nFormatter(num){
                       if (num >= 1000000000) {
       return('$' +(num / 1000000000).toFixed(1).replace(/.0$/, '')) + 'B';
     }
     if (num >= 1000000) {
        return ('$' + (num / 1000000).toFixed(1).replace(/.0$/, '')) + 'M';
     }
     if (num >= 1000) {
        return ('$' + (num / 1000).toFixed(1)).replace(/.0$/, '') + 'K';
     }
     if (num <1000 && num >0){
     return ('$' + (num / 1).toFixed(1)).replace(/.0$/, '');
     }
     if (num <= -1000000000){
     return ('-$' + (num / -1000000000).toFixed(1)).replace(/.0$/, '') + 'B';
     }
     if (num <= -1000000){
     return ('-$' + (num / -1000000).toFixed(1)).replace(/.0$/, '') + 'M';
     }
     if (num <= -1000){
     return ('-$' + (num / -1000).toFixed(1)).replace(/.0$/, '') + 'K' ;
     }
     if (num >-1000 && num <0){
     return ('-$' + (num / -1).toFixed(1)).replace(/.0$/, '');
     }
     

     return (num) }
                       return('<strong>Year:</strong> ' + params.value[0] + '<br /><strong>Current Account Balance: </strong>' + nFormatter(params.value[1]))
                       }
                       "))|>
                       e_y_axis(splitLine= list(show =FALSE),
                                axisLabel = list(color = "#000000", formatter = htmlwidgets::JS(
                                  "
                                        function(params){

          if (params >= 100000000000){
     return((params / 1000000000000).toFixed(1)).replace(/.0$/, '') +'T'
     }
     if (params >= 100000000) {
        return ((params / 1000000000).toFixed(1)).replace(/.0$/, '') + 'B';
     }
     if (params >= 1000000) {
        return ((params / 1000000).toFixed(1)).replace(/.0$/, '') + 'M';
     }
     if (params >= 1000) {
        return ((params / 1000).toFixed(1)).replace(/.0$/, '') + 'K';
     }
     if (params <1000 && params >0){
     return ((params / 1).toFixed(1)).replace(/.0$/, '');
     }
     if (params <= -100000000000){
     return((params / 1000000000000).toFixed(1)).replace(/.0$/, '') +'T'
     }
     if (params <= -100000000){
     return((params / 1000000000).toFixed(1)).replace(/.0$/, '') +'B'
     }
     if (params <= -1000000){
     return((params / 1000000).toFixed(1)).replace(/.0$/, '') + 'M'
     }
     if (params <= -1000){
     return((params / 1000).toFixed(1)).replace(/.0$/, '') + 'K' 
     }     
     if (params >-1000 && params <0){
     return ((params / 1).toFixed(1)).replace(/.0$/, '');
     }

     return (params) }") ),
                                axisTick = list(show = FALSE),
                                name = "Current Account Balance",
                                nameLocation = "middle",
                                nameTextStyle = list(
                                  color = "#000000",
                                  fontWeight = "bold",
                                  padding = 27,
                                  fontSize = 14
                                ),
                                nameRotate = 90)|>
                       e_x_axis( scale = TRUE, max= htmlwidgets::JS('function (value) {
                         return value.max}'), min= htmlwidgets::JS('function (value) {
                         return value.min}'),
                                 splitLine= list(show =FALSE),
                                 axisLabel = list(show = TRUE,color = "#000000", position = 'middle',
                                                  formatter =  htmlwidgets::JS('function (params) {
                         return(params)}')),
                                 axisTick = list(show = TRUE, alignWithLabel= TRUE),
                                 name = "Year",
                                 nameLocation = "middle",
                                 nameTextStyle = list(
                                   color = "#000000",
                                   fontWeight = "bold",
                                   padding = 20,
                                   fontSize = 14
                                 ))
                     
                     
                     
                     
                     
                   }) %>% bindCache(reporter_iso_sel())
                   
                   
                   output$CurrentAcctext <-
                     renderText({
                       Current_Acc_text <- ""
                       df_Macro <- df_Macroeconomic()
                       
                       df_CurrentAcc <-data.frame(df_Macro[,c("Year", "Current_Account_Balance")])
                       
                       df_CurrentAcc_Value <- df_CurrentAcc %>% filter(!is.na(Current_Account_Balance))
                       
                       req(nrow(df_CurrentAcc_Value)>1)
                       
                       MAX_CurrentAcc <- max(df_CurrentAcc_Value$Current_Account_Balance, na.rm = TRUE )
                       MIN_CurrentAcc <- min(df_Macro$Current_Account_Balance, na.rm = TRUE )
                       Latest_year <- max(df_CurrentAcc_Value$Year, na.rm = TRUE )
                       df_CurentAcc_MAx_year <- data.frame(df_CurrentAcc_Value$Year[df_CurrentAcc_Value$Current_Account_Balance==MAX_CurrentAcc])
                       df_CurentAcc_MIN_year <- data.frame(df_CurrentAcc_Value$Year[df_CurrentAcc_Value$Current_Account_Balance==MIN_CurrentAcc])
                       
                       CAV_row <- nrow(df_CurrentAcc_Value)
                       pos<-nrow(df_CurrentAcc_Value[df_CurrentAcc_Value$Current_Account_Balance>0,])
                       neg<-nrow(df_CurrentAcc_Value[df_CurrentAcc_Value$Current_Account_Balance<0,])
                       
                       if (CAV_row == neg)
                         
                       {
                         df_CurrentAcc_narrative <- df_narrative() %>% filter(Text_Category == 'Current_Acc_neg')  %>% select(c('HTML_text'))
                           # dbGetQuery(
                           #   con,
                           #   "select HTML_text from Dashboard_Narrative where project_name = 'Country Profile' and section_code = 'M01' and Text_Category == 'Current_Acc_neg'"
                           # )
                         
                         Current_Acc_text <-
                           df_CurrentAcc_narrative %>% str_replace_all(
                             c(
                               
                               "YEAR1" = paste(df_CurentAcc_MAx_year[1,1]),
                               "VALUE1" = paste(f_usdvalueposnegtext(MAX_CurrentAcc)),
                               "YEAR2" = paste(df_CurentAcc_MIN_year[1,1]),
                               "VALUE2" = paste(f_usdvalueposnegtext(MIN_CurrentAcc)),
                               "COUNTRY" = reporter(),
                               "YEAR3" = paste(Latest_year),
                               "VALUE3" = paste (f_usdvalueposnegtext(df_CurrentAcc_Value$Current_Account_Balance[df_CurrentAcc_Value$Year== Latest_year]))
                             )
                           )
                         
                       }
                       else {
                         
                         df_CurrentAcc_narrative <- df_narrative() %>% filter(Text_Category == 'Current_Acc')  %>% select(c('HTML_text'))
                           # dbGetQuery(
                           #   con,
                           #   "select HTML_text from Dashboard_Narrative where project_name = 'Country Profile' and section_code = 'M01' and Text_Category == 'Current_Acc'"
                           # )
                         
                         
                         
                         Current_Acc_text <-
                           df_CurrentAcc_narrative %>% str_replace_all(
                             c(
                               
                               "YEAR1" = paste(df_CurentAcc_MAx_year[1,1]),
                               "VALUE1" = paste(f_usdvalueposnegtext(MAX_CurrentAcc)),
                               "YEAR2" = paste(df_CurentAcc_MIN_year[1,1]),
                               "VALUE2" = paste(f_usdvalueposnegtext(MIN_CurrentAcc)),
                               "COUNTRY" = reporter(),
                               "YEAR3" = paste(Latest_year),
                               "VALUE3" = paste (f_usdvalueposnegtext(df_CurrentAcc_Value$Current_Account_Balance[df_CurrentAcc_Value$Year== Latest_year]))
                             )
                           )
                         
                       }
                       CurrentAcc_narrative<- paste(Current_Acc_text)
                       
                       
                     })  %>% bindCache(reporter())
                   
                   
                   
                   
                   ####End of Current Account Balance####
                   
                   
                   
                   ####Foreign Direct Investment (FDI)####
                   
                   
                   
                   observe(
                     {
                       
                       
                       df_Macro <- df_Macroeconomic()
                       
                       
                       df_FDI <-data.frame(df_Macro[,c("Year", "FDI")])
                       
                       df_FDI_value <- df_FDI %>% filter(!is.na(FDI))
                       
                       if (nrow(df_FDI_value)>1)
                       {
                         shinyjs::show(id = "displayFDI")
                         shinyjs::show(id = "columnFDI")
                       }
                       else
                       {
                         
                         shinyjs::hide(id = "displayFDI")
                         shinyjs::hide(id = "columnFDI")
                         
                       }
                     }
                   )
                   
                   
                   
                   output$displayFDI <- renderEcharts4r({
                     
                     df_Macro <- df_Macroeconomic()
                     
                     df_FDI <-data.frame(df_Macro[,c("Year", "FDI")])
                     
                     df_FDI_value <- df_FDI %>% filter(!is.na(FDI))
                     
                     
                     df_FDI_value |> 
                       e_charts(Year) |> 
                       e_line(FDI, symbol = 'circle', symbolSize =5, lineStyle= list(shadowColor= 'rgba(0, 0, 0, 0.5)',shadowBlur= 10),
                              areaStyle = list(opacity= 0.8, color =htmlwidgets::JS(
                                "new echarts.graphic.LinearGradient(
    0, 0, 0, 1,
    [
      { offset: 0, color: 'rgb(0, 221, 255)' },
      { offset: 1, color: '#38b3e5' }
    ])"
                              )), xAxis =list(type = "category"), color = "#0291da",
                              legend = list(show =FALSE)) |>
                       e_tooltip(trigger = "item", formatter = htmlwidgets::JS("
                       function(params){
                        function nFormatter(num){
                       if (num >= 1000000000) {
       return('$' +(num / 1000000000).toFixed(1).replace(/.0$/, '')) + 'B';
     }
     if (num >= 1000000) {
        return ('$' + (num / 1000000).toFixed(1).replace(/.0$/, '')) + 'M';
     }
     if (num >= 1000) {
        return ('$' + (num / 1000).toFixed(1)).replace(/.0$/, '') + 'K';
     }
     if (num <1000 && num >0){
     return ('$' + (num / 1).toFixed(1)).replace(/.0$/, '');
     }
     if (num <= -1000000000){
     return ('-$' + (num / -1000000000).toFixed(1)).replace(/.0$/, '') + 'B';
     }
     if (num <= -1000000){
     return ('-$' + (num / -1000000).toFixed(1)).replace(/.0$/, '') + 'M';
     }
     if (num <= -1000){
     return ('-$' + (num / -1000).toFixed(1)).replace(/.0$/, '') + 'K' ;
     }
     if (num > -1000 && num <0){
     return ('-$' + (num / -1).toFixed(1)).replace(/.0$/, '');
     }

     return (num) }
                       return('<strong>Year: </strong>' + params.value[0] + '<br /><strong>FDI: </strong>' + nFormatter(params.value[1]))
                       }
                       "))|>
                       e_y_axis(splitLine= list(show =FALSE),
                                axisLabel = list(color = "#000000", formatter = htmlwidgets::JS(
                                  "
                                        function(params){

          if (params >= 100000000000){
     return(params / 1000000000000) +'T'
     }
     if (params >= 100000000) {
        return (params / 1000000000) + 'B';
     }
     if (params >= 1000000) {
        return (params / 1000000) + 'M';
     }
     if (params >= 1000) {
        return (params / 1000) + 'K';
     }
     if (params <1000 && params >0){
     return ((params / 1).toFixed(1)).replace(/.0$/, '');
     }
 
     if (params <= -100000000000){
     return((params / 1000000000000).toFixed(1)).replace(/.0$/, '') +'T'
     }
     if (params <= -100000000){
     return((params / 1000000000).toFixed(1)).replace(/.0$/, '') +'B'
     }
     if (params <= -1000000){
     return((params / 1000000).toFixed(1)).replace(/.0$/, '') + 'M'
     }
     if (params <= -1000){
     return(params / 1000) + 'K' 
     }
     if (params >-1000 && params <0){
     return ((params / 1).toFixed(1)).replace(/.0$/, '');
     }

     return (params) }") ),
                                axisTick = list(show = FALSE),
                                name = "Foreign Direct Investment",
                                nameLocation = "middle",
                                nameTextStyle = list(
                                  color = "#000000",
                                  fontWeight = "bold",
                                  padding = 28,
                                  fontSize = 14
                                ),
                                nameRotate = 90)|>
                       e_grid(
                         top= '10%',
                         bottom = '22%',
                         right = '5%')|>
                       e_x_axis( scale = TRUE, max= htmlwidgets::JS('function (value) {
                         return value.max}'), min= htmlwidgets::JS('function (value) {
                         return value.min}'),
                                 splitLine= list(show =FALSE),
                                 axisLabel = list(show = TRUE,color = "#000000", position = 'middle',
                                                  formatter =  htmlwidgets::JS('function (params) {
                         return(params)}')),
                                 axisTick = list(show = TRUE, alignWithLabel= TRUE),
                                 name = "Year",
                                 nameLocation = "middle",
                                 nameTextStyle = list(
                                   color = "#000000",
                                   fontWeight = "bold",
                                   padding = 20,
                                   fontSize = 14
                                 ))
                     
                     
                     
                     
                     
                   }) %>% bindCache(reporter_iso_sel())
                   
                   
                   output$FDItext <-
                     renderText({
                       FDI_text <- ""
                       df_Macro <- df_Macroeconomic()
                       
                       df_FDI <-data.frame(df_Macro[,c("Year", "FDI")])
                       
                       df_FDI_value <- df_FDI %>% filter(!is.na(FDI))
                       
                       req(nrow(df_FDI_value)>1)
                       
                       FDI_row <- nrow(df_FDI_value)
                       pos<-nrow(df_FDI_value[df_FDI_value$FDI>0,])
                       neg<-nrow(df_FDI_value[df_FDI_value$FDI<0,])
                       
                       
                       
                       
                       MAX_FDI <- max(df_FDI_value$FDI, na.rm = TRUE )
                       # MAX_FDI<- f_usdvaluetext(MAX_FDI)
                       MIN_FDI <- min(df_Macro$FDI, na.rm = TRUE )
                       Latest_year <- max(df_FDI_value$Year, na.rm = TRUE )
                       df_FDI_MAx_year <- data.frame(df_FDI_value$Year[df_FDI_value$FDI==MAX_FDI])
                       df_FDI_MIN_year <- data.frame(df_FDI_value$Year[df_FDI_value$FDI==MIN_FDI])
                       
                       
                       df_FDI_narrative <- df_narrative() %>% filter(Text_Category == 'FDI')  %>% select(c('HTML_text'))
                           # dbGetQuery(
                           #   con,
                           #   "select HTML_text from Dashboard_Narrative where project_name = 'Country Profile' and section_code = 'M01' and Text_Category == 'FDI_neg'"
                           # )
                           #
                         FDI_text <-
                           df_FDI_narrative %>% str_replace_all(
                             c(

                               "YEAR1" = paste(df_FDI_MAx_year[1,1]),
                               "VALUE1" = paste(f_usdvalueposnegtext(MAX_FDI)),
                               "YEAR2" = paste(df_FDI_MIN_year[1,1]),
                               "VALUE2" = paste(f_usdvalueposnegtext(MIN_FDI)),
                               "COUNTRY" = reporter(),
                               "YEAR3" = paste(Latest_year),
                               "VALUE3" = paste (f_usdvalueposnegtext(df_FDI_value$FDI[df_FDI_value$Year== Latest_year]))
                             )
                           )
                       
                       
                       
                       
                       # if (FDI_row == neg)
                       # {
                       #   
                       #   df_FDI_narrative <- df_narrative() %>% filter(Text_Category == 'FDI_neg')  %>% select(c('HTML_text'))
                       #     # dbGetQuery(
                       #     #   con,
                       #     #   "select HTML_text from Dashboard_Narrative where project_name = 'Country Profile' and section_code = 'M01' and Text_Category == 'FDI_neg'"
                       #     # )
                       #     # 
                       #   FDI_text <-
                       #     df_FDI_narrative %>% str_replace_all(
                       #       c(
                       #         
                       #         "YEAR1" = paste(df_FDI_MAx_year[1,1]),
                       #         "VALUE1" = paste(f_usdvalueposnegtext(MAX_FDI)),
                       #         "YEAR2" = paste(df_FDI_MIN_year[1,1]),
                       #         "VALUE2" = paste(f_usdvalueposnegtext(MIN_FDI)),
                       #         "COUNTRY" = reporter(),
                       #         "YEAR3" = paste(Latest_year),
                       #         "VALUE3" = paste (f_usdvalueposnegtext(df_FDI_value$FDI[df_FDI_value$Year== Latest_year]))
                       #       )
                       #     )
                       # }
                       # else if (FDI_row == pos)
                       # {
                       #   
                       #   
                       #   df_FDI_narrative <- df_narrative() %>% filter(Text_Category == 'FDI_pos')  %>% select(c('HTML_text'))
                       #     # dbGetQuery(
                       #     #   con,
                       #     #   "select HTML_text from Dashboard_Narrative where project_name = 'Country Profile' and section_code = 'M01' and Text_Category == 'FDI_pos'"
                       #     # )
                       #     # 
                       #   
                       #   
                       #   FDI_text <-
                       #     df_FDI_narrative %>% str_replace_all(
                       #       c(
                       #         
                       #         "YEAR1" = paste(df_FDI_MAx_year[1,1]),
                       #         "VALUE1" = paste(f_usdvalueposnegtext(MAX_FDI)),
                       #         "YEAR2" = paste(df_FDI_MIN_year[1,1]),
                       #         "VALUE2" = paste(f_usdvalueposnegtext(MIN_FDI)),
                       #         "COUNTRY" = reporter(),
                       #         "YEAR3" = paste(Latest_year),
                       #         "VALUE3" = paste (f_usdvalueposnegtext(df_FDI_value$FDI[df_FDI_value$Year== Latest_year]))
                       #       )
                       #     )
                       # }
                       # else {
                       #   
                       #   
                       #   df_FDI_narrative <- df_narrative() %>% filter(Text_Category == 'FDI')  %>% select(c('HTML_text'))
                       #     # dbGetQuery(
                       #     #   con,
                       #     #   "select HTML_text from Dashboard_Narrative where project_name = 'Country Profile' and section_code = 'M01' and Text_Category == 'FDI'"
                       #     # )
                       #   
                       #   
                       #   FDI_text <-
                       #     df_FDI_narrative %>% str_replace_all(
                       #       c(
                       #         
                       #         "YEAR1" = paste(df_FDI_MAx_year[1,1]),
                       #         "VALUE1" = paste(f_usdvalueposnegtext(MAX_FDI)),
                       #         "YEAR2" = paste(df_FDI_MIN_year[1,1]),
                       #         "VALUE2" = paste(f_usdvalueposnegtext(MIN_FDI)),
                       #         "COUNTRY" = reporter(),
                       #         "YEAR3" = paste(Latest_year),
                       #         "VALUE3" = paste (f_usdvalueposnegtext(df_FDI_value$FDI[df_FDI_value$Year== Latest_year]))
                       #       )
                       #     )
                       #   
                       #   
                       # }
                       
                       FDI_narrative<- paste(FDI_text)
                       
                       
                     })  %>% bindCache(reporter())
                   
                   
                   
                   
                   ####End of Foreign Direct Investment (FDI)####
                   
                   
                   
                   ####Real interest Rate####
                   
                   
                   
                   observe(
                     {
                       
                       
                       df_Macro <- df_Macroeconomic()
                       
                       df_Interest <-data.frame(df_Macro[,c("Year", "Interest_Rate")])
                       
                       df_Interest_Value <- df_Interest %>% filter(!is.na(Interest_Rate))
                       
                       if (nrow(df_Interest_Value)>0)
                       {
                         shinyjs::show(id = "displayInterest")
                         shinyjs::show(id = "column1")
                       }
                       else
                       {
                         
                         shinyjs::hide(id = "displayInterest")
                         shinyjs::hide(id = "column1")
                         
                       }
                     }
                   )
                   
                   
                   
                   
                   
                   output$displayInterest <- renderEcharts4r({
                     
                     df_Macro <- df_Macroeconomic()
                     
                     df_Interest <-data.frame(df_Macro[,c("Year", "Interest_Rate")])
                     
                     df_Interest_Value <- df_Interest %>% filter(!is.na(Interest_Rate))
                     
                     
                     
                     
                     df_Interest_Value |> 
                       e_charts(Year) |> 
                       e_line(Interest_Rate, symbol = 'circle', symbolSize =5, lineStyle= list(shadowColor= 'rgba(0, 0, 0, 0.5)',shadowBlur= 10),
                              areaStyle = list(opacity= 0.8, color =htmlwidgets::JS(
                                "new echarts.graphic.LinearGradient(
    0, 0, 0, 1,
    [
      { offset: 0, color: 'rgb(0, 221, 255)' },
      { offset: 1, color: '#38b3e5' }
    ])"
                              )), xAxis =list(type = "category"), color = "#0291da",
                              legend = list(show =FALSE)) |>
                       e_tooltip(trigger = "item", formatter = htmlwidgets::JS("
                       function(params){
                       return('<strong>Year:</strong> ' + params.value[0] + '<br /><strong>Real Interest Rate:</strong> ' + params.value[1] + '%')
                       }
                       "))|>
                       e_y_axis(splitLine= list(show =FALSE),
                                axisLabel = list(color = "#000000", formatter = htmlwidgets::JS("function (params) {
                         return(params)  + '%'}")),
                                axisTick = list(show = FALSE),
                                name = "Real Interest Rate",
                                nameLocation = "middle",
                                nameTextStyle = list(
                                  color = "#000000",
                                  fontWeight = "bold",
                                  padding = 28,
                                  fontSize = 14
                                ),
                                nameRotate = 90)|>
                       e_grid(
                         top= '10%',
                         bottom = '22%',
                         right = '5%')|>
                       e_x_axis( scale = TRUE, max= htmlwidgets::JS('function (value) {
                         return value.max}'), min= htmlwidgets::JS('function (value) {
                         return value.min}'),
                                 splitLine= list(show =FALSE),
                                 axisLabel = list(show = TRUE,color = "#000000", position = 'middle',
                                                  formatter =  htmlwidgets::JS('function (params) {
                         return(params)}')),
                                 axisTick = list(show = TRUE, alignWithLabel= TRUE),
                                 name = "Year",
                                 nameLocation = "middle",
                                 nameTextStyle = list(
                                   color = "#000000",
                                   fontWeight = "bold",
                                   padding = 20,
                                   fontSize = 14
                                 ))
                     
                     
                     
                     
                     
                   }) %>% bindCache(reporter_iso_sel())
                   
                   
                   output$RealInteresttext <-
                     renderText({
                       df_Macro <- df_Macroeconomic()
                       
                       df_RealInt <-data.frame(df_Macro[,c("Year", "Interest_Rate")])
                       
                       df_RealInt_Value <- df_RealInt %>% filter(!is.na(Interest_Rate))
                       
                       req(nrow(df_RealInt_Value)>0)
                       
                       Real_int_text <- ""
                       
                       
                       df_RealInt_narrative <- df_narrative() %>% filter(Text_Category == 'Real_Interest')  %>% select(c('HTML_text'))
                         # dbGetQuery(
                         #   con,
                         #   "select HTML_text from Dashboard_Narrative where project_name = 'Country Profile' and section_code = 'M01' and Text_Category == 'Real_Interest'"
                         # )
                       
                       
                       MAX_RealInt <- max(df_RealInt_Value$Interest_Rate, na.rm = TRUE )
                       MIN_RealInt <- min(df_Macro$Interest_Rate, na.rm = TRUE )
                       Latest_year <- max(df_RealInt_Value$Year, na.rm = TRUE )
                       df_RealInt_MAx_year <- data.frame(df_RealInt_Value$Year[df_RealInt_Value$Interest_Rate==MAX_RealInt])
                       df_RealInt_MIN_year <- data.frame(df_RealInt_Value$Year[df_RealInt_Value$Interest_Rate==MIN_RealInt])
                       Current_Acc_text <-
                         df_RealInt_narrative %>% str_replace_all(
                           c(
                             
                             "YEAR1" = paste(df_RealInt_MAx_year[1,1]),
                             "VALUE1" = paste0(MAX_RealInt,'%'),
                             "YEAR2" = paste(df_RealInt_MIN_year[1,1]),
                             "VALUE2" = paste0(MIN_RealInt,'%'),
                             "COUNTRY" = reporter(),
                             "YEAR3" = paste(Latest_year),
                             "VALUE3" = paste0(df_RealInt_Value$Interest_Rate[df_RealInt_Value$Year== Latest_year],'%')
                           )
                         )
                       
                       
                       CurrentAcc_narrative<- paste(Current_Acc_text)
                       
                       
                     })  %>% bindCache(reporter())
                   
                   
                   
                   
                   ####End of Real interest Rate####
                   
                   
                   
                   
                   
                   ####GDP per Capita####
                   
                   observe(
                     {
                       
                       
                       df_Macro <- df_Macroeconomic()
                       
                       
                       
                       df_GDP <-data.frame(df_Macro[,c("Year", "GDP_Per_Capital")])
                       
                       df_GDP_Value <- df_GDP %>% filter(!is.na(GDP_Per_Capital))
                       
                       if (nrow(df_GDP_Value)>1)
                       {
                         shinyjs::show(id = "columnGDPPC")
                       }
                       else
                       {
                         shinyjs::hide(id = "columnGDPPC")
                         
                       }
                     }
                   )
                   
                   
                   
                   output$displayGDPPC <- renderEcharts4r({
                     
                     df_Macro <- df_Macroeconomic()
                     
                     df_GDP <-data.frame(df_Macro[,c("Year", "GDP_Per_Capital")])
                     
                     df_GDP_Value <- df_GDP %>% filter(!is.na(GDP_Per_Capital))
                     
                     
                     
                     
                     df_GDP_Value |>
                       e_charts(Year) |>
                       e_line(GDP_Per_Capital, symbol = 'circle', symbolSize =5, lineStyle= list(shadowColor= 'rgba(0, 0, 0, 0.5)',shadowBlur= 10),
                              areaStyle = list(opacity= 0.8, color =htmlwidgets::JS(
                                "new echarts.graphic.LinearGradient(
    0, 0, 0, 1,
    [
      { offset: 0, color: 'rgb(0, 221, 255)' },
      { offset: 1, color: '#38b3e5' }
    ])"
                              )), xAxis =list(type = "category"), color = "#0291da",
                              legend = list(show =FALSE)) |>
                       e_tooltip(trigger = "item", formatter = htmlwidgets::JS("
                       function(params){
                        function nFormatter(num){
                       if (num >= 1000000000) {
       return('$' +(num / 1000000000).toFixed(1).replace(/.0$/, '')) + 'B';
     }
     if (num >= 1000000) {
        return ('$' + (num / 1000000).toFixed(1).replace(/.0$/, '')) + 'M';
     }
     if (num >= 1000) {
        return ('$' + (num / 1000).toFixed(1)).replace(/.0$/, '') + 'K';
     }
     if (num <1000 && num >0){
     return ('$' + (num / 1).toFixed(1)).replace(/.0$/, '');
     }

     return (num) }
                       return('<strong>Year:</strong> ' + params.value[0] + '<br /><strong>GDP per Capita: </strong>' + nFormatter(params.value[1]))
                       }
                       "))|>
                       e_y_axis(splitLine= list(show =FALSE),
                                axisLabel = list(color = "#000000", formatter = htmlwidgets::JS("function (params) {
                                   if (params >= 100000000000){
     return(params / 1000000000000) +'T'
     }
     if (params >= 1000000000) {
        return (params / 1000000000) + 'B';
     }
     if (params >= 1000000) {
        return (params / 1000000) + 'M';
     }
     if (params >= 1000) {
        return (params / 1000) + 'K';
     }     
     if (params <1000 && params >0){
     return ((params / 1).toFixed(1)).replace(/.0$/, '');
     }
     return (params)}")),
                                axisTick = list(show = FALSE),
                                name = "GDP per Capita",
                                nameLocation = "middle",
                                nameTextStyle = list(
                                  color = "#000000",
                                  fontWeight = "bold",
                                  padding = 28,
                                  fontSize = 14
                                ),
                                nameRotate = 90)|>
                       e_grid(
                         top= '10%',
                         bottom = '22%',
                         right = '5%')|>
                       e_x_axis( scale = TRUE, max= htmlwidgets::JS('function (value) {
                         return value.max}'), min= htmlwidgets::JS('function (value) {
                         return value.min}'),
                                 splitLine= list(show =FALSE),
                                 axisLabel = list(show = TRUE,color = "#000000", position = 'middle',
                                                  formatter =  htmlwidgets::JS('function (params) {
                         return(params)}')),
                                 axisTick = list(show = TRUE, alignWithLabel= TRUE),
                                 name = "Year",
                                 nameLocation = "middle",
                                 nameTextStyle = list(
                                   color = "#000000",
                                   fontWeight = "bold",
                                   padding = 20,
                                   fontSize = 14
                                 ))
                     
                     
                     
                     
                     
                   }) %>% bindCache(reporter_iso_sel())
                   
                   
                   output$GDPPCtext <-
                     renderText({
                       df_Macro <- df_Macroeconomic()
                       
                       df_GDP <-data.frame(df_Macro[,c("Year", "GDP_Per_Capital")])
                       
                       df_GDP_Value <- df_GDP %>% filter(!is.na(GDP_Per_Capital))
                       req(nrow(df_GDP_Value)>0)
                       
                       
                       
                       
                       GDP_text <- ""
                       
                       
                       df_GDP_narrative <- df_narrative() %>% filter(Text_Category == 'GDP_Per_Capita')  %>% select(c('HTML_text'))
                         # dbGetQuery(
                         #   con,
                         #   "select HTML_text from Dashboard_Narrative where project_name = 'Country Profile' and section_code = 'M01' and Text_Category == 'GDP_Per_Capita'"
                         # )
                         # 
                       # df_GDP_narrative <- paste ("<font size=+1>GDP per capita of COUNTRY peaked in YEAR1 at VALUE1 and reached its lowest level in YEAR2 at VALUE2.
                       #                            The most recent GDP per capita available for COUNTRY is VALUE3 for the year YEAR3.")
                       
                       
                       MAX_GDP <- max(df_GDP_Value$GDP_Per_Capital, na.rm = TRUE )
                       MIN_GDP <- min(df_Macro$GDP_Per_Capital, na.rm = TRUE )
                       Latest_year <- max(df_GDP_Value$Year, na.rm = TRUE )
                       df_GDP_MAx_year <- data.frame(df_GDP_Value$Year[df_GDP_Value$GDP_Per_Capital==MAX_GDP])
                       df_GDP_MIN_year <- data.frame(df_GDP_Value$Year[df_GDP_Value$GDP_Per_Capital==MIN_GDP])
                       GDP_text <-
                         df_GDP_narrative %>% str_replace_all(
                           c(
                             
                             "YEAR1" = paste(df_GDP_MAx_year[1,1]),
                             "VALUE1" = paste(f_usdvaluetext(MAX_GDP)),
                             "YEAR2" = paste(df_GDP_MIN_year[1,1]),
                             "VALUE2" = paste(f_usdvaluetext_macro(MIN_GDP)),
                             "COUNTRY" = reporter(),
                             "YEAR3" = paste(Latest_year),
                             "VALUE3" = paste (f_usdvaluetext(df_GDP_Value$GDP_Per_Capital[df_GDP_Value$Year== Latest_year]))
                           )
                         )
                       
                       
                       GDP_narrative<- paste(GDP_text)
                       
                       
                     })  %>% bindCache(reporter())
                   
                   
                   
                   
                   ####End of GDP per Capita####
                   
                   
                   
                   
                   
                   
                   
                   
                   ####GDP####
                   
                   observe(
                     {
                       
                       
                       df_Macro <- df_Macroeconomic()
                       
                       
                       
                       df_GDP <-data.frame(df_Macro[,c("Year", "GDP")])
                       
                       df_GDP_Value <- df_GDP %>% filter(!is.na(GDP))
                       
                       if (nrow(df_GDP_Value)>1)
                       {
                         shinyjs::show(id = "columnGDP")
                       }
                       else
                       {
                         shinyjs::hide(id = "columnGDP")
                         
                       }
                     }
                   )
                   
                   
                   
                   output$displayGDP <- renderEcharts4r({
                     
                     df_Macro <- df_Macroeconomic()
                     
                     df_GDP <-data.frame(df_Macro[,c("Year", "GDP")])
                     
                     df_GDP_Value <- df_GDP %>% filter(!is.na(GDP))
                     
                     
                     
                     
                     df_GDP_Value |>
                       e_charts(Year) |>
                       e_line(GDP, symbol = 'circle', symbolSize =5, lineStyle= list(shadowColor= 'rgba(0, 0, 0, 0.5)',shadowBlur= 10),
                              areaStyle = list(opacity= 0.8, color =htmlwidgets::JS(
                                "new echarts.graphic.LinearGradient(
    0, 0, 0, 1,
    [
      { offset: 0, color: 'rgb(0, 221, 255)' },
      { offset: 1, color: '#38b3e5' }
    ])"
                              )), xAxis =list(type = "category"), color = "#0291da",
                              legend = list(show =FALSE)) |>
                       e_tooltip(trigger = "item", formatter = htmlwidgets::JS("
                       function(params){
                        function nFormatter(num){
                        if (num >= 100000000000){
     return('$' + (num / 1000000000000).toFixed(1).replace(/.0$/, '')) +'T'
     }
                       if (num >= 1000000000) {
       return('$' +(num / 1000000000).toFixed(1).replace(/.0$/, '')) + 'B';
     }
     if (num >= 1000000) {
        return ('$' + (num / 1000000).toFixed(1).replace(/.0$/, '')) + 'M';
     }
     if (num >= 1000) {
        return ('$' + (num / 1000).toFixed(1)).replace(/.0$/, '') + 'K';
     }
     if (num <1000 && num >0){
     return ('$' + (num / 1).toFixed(1)).replace(/.0$/, '');
     }

     return (num) }
                       return('<strong>Year:</strong> ' + params.value[0] + '<br /><strong>GDP: </strong>' + nFormatter(params.value[1]))
                       }
                       "))|>
                       e_y_axis(splitLine= list(show =FALSE),
                                axisLabel = list(color = "#000000", formatter = htmlwidgets::JS("function (params) {
                                   if (params >= 100000000000){
     return(params / 1000000000000) +'T'
     }
     if (params >= 100000000) {
        return (params / 1000000000) + 'B';
     }
     if (params >= 100000) {
        return (params / 1000000) + 'M';
     }
     if (params >= 1000) {
        return (params / 1000) + 'K';
     }     
     if (params <1000 && params >0){
     return ((params / 1).toFixed(1)).replace(/.0$/, '');
     }
     return (params)}")),
                                axisTick = list(show = FALSE),
                                name = "GDP",
                                nameLocation = "middle",
                                nameTextStyle = list(
                                  color = "#000000",
                                  fontWeight = "bold",
                                  padding = 28,
                                  fontSize = 14
                                ),
                                nameRotate = 90)|>
                       e_grid(
                         top= '10%',
                         bottom = '22%',
                         right = '5%')|>
                       e_x_axis( scale = TRUE, max= htmlwidgets::JS('function (value) {
                         return value.max}'), min= htmlwidgets::JS('function (value) {
                         return value.min}'),
                                 splitLine= list(show =FALSE),
                                 axisLabel = list(show = TRUE,color = "#000000", position = 'middle',
                                                  formatter =  htmlwidgets::JS('function (params) {
                         return(params)}')),
                                 axisTick = list(show = TRUE, alignWithLabel= TRUE),
                                 name = "Year",
                                 nameLocation = "middle",
                                 nameTextStyle = list(
                                   color = "#000000",
                                   fontWeight = "bold",
                                   padding = 20,
                                   fontSize = 14
                                 ))
                     
                     
                     
                     
                     
                   }) %>% bindCache(reporter_iso_sel())
                   
                   
                   output$GDPtext <-
                     renderText({
                       df_Macro <- df_Macroeconomic()
                       
                       df_GDP <-data.frame(df_Macro[,c("Year", "GDP")])
                       
                       df_GDP_Value <- df_GDP %>% filter(!is.na(GDP))
                       req(nrow(df_GDP_Value)>0)
                       
                       
                       
                       
                       GDP_text <- ""
                       
                       
                       df_GDP_narrative <- df_narrative() %>% filter(Text_Category == 'GDP')  %>% select(c('HTML_text'))
                         # dbGetQuery(
                         #   con,
                         #   "select HTML_text from Dashboard_Narrative where project_name = 'Country Profile' and section_code = 'M01' and Text_Category == 'GDP'"
                         # )
                       # 
                       # df_GDP_narrative <- paste ("<font size=+1>COUNTRY had its highest GDP Current US$ value in YEAR1 at a value of VALUE1 and its lowest value in YEAR2 at VALUE2.
                       #                            The most recent GDP value available for COUNTRY is VALUE3 for the year YEAR3.")
                       
                       
                       MAX_GDP <- max(df_GDP_Value$GDP, na.rm = TRUE )
                       MIN_GDP <- min(df_Macro$GDP, na.rm = TRUE )
                       Latest_year <- max(df_GDP_Value$Year, na.rm = TRUE )
                       df_GDP_MAx_year <- data.frame(df_GDP_Value$Year[df_GDP_Value$GDP==MAX_GDP])
                       df_GDP_MIN_year <- data.frame(df_GDP_Value$Year[df_GDP_Value$GDP==MIN_GDP])
                       GDP_text <-
                         df_GDP_narrative %>% str_replace_all(
                           c(
                             
                             "YEAR1" = paste(df_GDP_MAx_year[1,1]),
                             "VALUE1" = paste(f_usdvaluetext(MAX_GDP)),
                             "YEAR2" = paste(df_GDP_MIN_year[1,1]),
                             "VALUE2" = paste(f_usdvaluetext(MIN_GDP)),
                             "COUNTRY" = reporter(),
                             "YEAR3" = paste(Latest_year),
                             "VALUE3" = paste (f_usdvaluetext(df_GDP_Value$GDP[df_GDP_Value$Year== Latest_year]))
                           )
                         )
                       
                       
                       GDP_narrative<- paste(GDP_text)
                       
                       
                     }) %>% bindCache(reporter())
                   
                   
                   
                   
                   ####End of GDP####

                   
                   
                   
                   
                 })}