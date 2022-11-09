demographics_ui <- function(id) {
  ns <- NS(id)
  
  tagList(fluidRow(column(
    8,
    offset = 2,
    shinydashboard::box(
      solidHeader = TRUE,
      tags$div(
        class = "row mgtop",
        checked = NA,
        list(
          tags$div(
            class = "col demo popu demo_icon row",
            checked = NA,
            list(
              tags$div(
                class = "col demo_text",
                checked = NA,
                list(
                  tags$p(class = "demo_title", "Population"),
                  htmlOutput(
                    outputId = ns("Population"),
                    container = tags$h4,
                    class = "demo_value"
                  ),
                  tags$div(
                    class = "row Demo_ym",
                    checked = NA,
                    list(
                      tags$p(class = "demo_year", "Year "),
                      htmlOutput(
                        outputId = ns("Population_Year"),
                        container = tags$p,
                        class = "demo_year"
                      )
                    )
                  )
                )
              ),
              tags$div(class = "col demo_icon_div", checked =
                         NA,
                       list(tags$img(src="population.png", height="40px", width="32px", deleteFile=FALSE)))
            )
          ),
          tags$div(
            class = "col demo demo_icon row",
            checked = NA,
            list(
              tags$div(
                class = "col demo_text",
                checked = NA,
                list(
                  tags$p(class = "demo_title", "Region"),
                  htmlOutput(
                    outputId = ns("Region"),
                    container = tags$h4,
                    class = "demo_value"
                  )
                )
              ),
              tags$div(class = "col demo_icon_div", checked =
                         NA,
                       #list(tags$img(src="africa.svg", height="40px", width="40px", deleteFile=FALSE)),
                       imageOutput(outputId = ns("RegionIcon"),height="40px", width="40px")
              )
            )
            
          ),
          tags$div(
            class = "col demo demo_icon row",
            checked = NA,
            list(
              tags$div(
                class = "col demo_text",
                checked = NA,
                list(
                  tags$p(class = "demo_title", "GDP Per Capita"),
                  htmlOutput(
                    outputId = ns("GDP"),
                    container = tags$h4,
                    class = "demo_value"
                  ),
                  tags$div(
                    class = "row Demo_ym",
                    checked = NA,
                    list(
                      tags$p(class = "demo_year", "Year "),
                      htmlOutput(
                        outputId = ns("GDP_Year"),
                        container = tags$p,
                        class = "demo_year"
                      )
                    )
                  )
                )
              ),
              tags$div(class = "col demo_icon_div", checked =
                         NA,
                       list(tags$img(src="gdp.png", height="40px", width="40px", deleteFile=FALSE)))
            )
            
          ),
          tags$div(class = "col demo", checked = NA,
                   list(
                     tags$div(
                       class = "demo_text",
                       checked = NA,
                       list(
                         tags$p(class = "demo_title", "Development Group"),
                         htmlOutput(
                           outputId = ns("Development"),
                           container = tags$h4,
                           class = "demo_value"
                         )
                       )
                     )
                   )),
          tags$div(class = "col demo", checked = NA,
                   list(
                     tags$div(
                       class = "demo_text",
                       checked = NA,
                       list(
                         tags$p(class = "demo_title", "Income Group"),
                         htmlOutput(
                           outputId = ns("Income"),
                           container = tags$h4,
                           class = "demo_value"
                         )
                       )
                     )
                   )),
          tags$div(class = "col demo", checked = NA,
                   list(
                     tags$div(
                       class = "demo_text",
                       checked = NA,
                       list(
                         tags$p(class = "demo_title", "Total Imports"),
                         htmlOutput(
                           outputId = ns("Import"),
                           container = tags$h4,
                           class = "demo_value"
                         ),
                         tags$div(
                           class = "row Demo_ym",
                           checked = NA,
                           list(
                             tags$p(class = "demo_year", "Year "),
                             htmlOutput(
                               outputId = ns("Trade_Year"),
                               container = tags$p,
                               class = "demo_year"
                             )
                           )
                         )
                       )
                     )
                   )),
          tags$div(class = "col demo", checked = NA,
                   list(tags$div(
                     class = "demo_text",
                     checked = NA,
                     list(
                       tags$p(class = "demo_title", "Total Exports"),
                       htmlOutput(
                         outputId = ns("Export"),
                         container = tags$h4,
                         class = "demo_value"
                       ),
                       tags$div(
                         class = "row Demo_ym",
                         checked = NA,
                         list(
                           tags$p(class = "demo_year", "Year "),
                           htmlOutput(
                             outputId = ns("Trade_Year2"),
                             container = tags$p,
                             class = "demo_year"
                           )
                         )
                       )
                       
                     )
                     
                   )))
          
        )
        
      ),
      
      
      width = 16,
      
      
    )
  )))
}

demographics_server <- function(id, reporter_iso_sel) {
  moduleServer(id,
               function(input, output, session) {
                 # get demographic data from database table
                 df_demography_data <- reactive({
                   reporter_iso <- reporter_iso_sel()
                   
                   df_demo <- dbGetQuery(
                     con,
                     paste(
                       " select [Country_ISO]
                        ,[Country]
                    	  ,[Population]
                    	  ,[Population_Year]
                        ,[Development_Group]
                        ,[Income_Group]
                    	  ,[Region]
                        ,[GDP_Per_Capita]
                        ,[GDP_Year]
                        ,[Total_Exports]
                        ,[Total_Imports]
                        ,[Trade_Year]
                      from [Demographics]
                      where Country_ISO = '",
                                   reporter_iso,
                                   "'",
                       sep = ""
                     )
                   )
                   df_demo
                 }) %>% bindCache(reporter_iso_sel())
                 
                 observe({
                   output$Population <- renderText({
                     df_population <- df_demography_data()
                     population <- df_population[1, 3]
                     
                     
                     if (is.numeric(population))
                     {
                       f_valuetext(population)
                     } else{
                       population
                     }
                     
                   }) %>% bindCache(reporter_iso_sel())
                   
                   output$Population_Year <- renderText({
                     df_Year <- df_demography_data()
                     Year <- df_Year[1, 4]
                     
                   }) %>% bindCache(reporter_iso_sel())
                   
                   output$Development <- renderText({
                     df_development <- df_demography_data()
                     development <- df_development[1, 5]
                     
                   }) %>% bindCache(reporter_iso_sel())
                   
                   output$Income <- renderText({
                     df_Income <- df_demography_data()
                     income <- df_Income[1, 6]
                      
                   }) %>% bindCache(reporter_iso_sel())
                   
                   output$Region <- renderText({
                     df_region <- df_demography_data()
                     region <- df_region[1, 7]
                     
                   }) %>% bindCache(reporter_iso_sel())
                   
                   output$RegionIcon <- renderImage({
                     df_region <- df_demography_data()
                     region <- df_region[1, 7]
                     
                     if (region == "Africa"){
                       list(src="img/africa.svg", height="40px", width="40px")
                     }else if (region == "Asia"){
                       list(src="img/asia.svg", height="40px", width="40px")
                     }else if (region == "Europe"){
                       list(src="img/europe.svg", height="40px", width="40px")
                     }else if (region == "North America"){
                       list(src="img/northamerica.svg", height="40px", width="40px")
                     }else if (region == "South America"){
                       list(src="img/southamerica.svg", height="40px", width="40px")
                     }else if (region == "Oceania"){
                       list(src="img/oceania.svg", height="40px", width="40px")
                     }else{
                       list(src="img/nodata.svg", height="40px", width="40px")
                     }
                     
                     }, deleteFile=FALSE)
                   
                   output$GDP <- renderText({
                     df_GDP <- df_demography_data()
                     GDP <- df_GDP[1, 8]
                     
                     if (is.numeric(GDP))
                     {
                       label_comma(accuracy = 1, prefix = "$")(GDP)
                     } else{
                       GDP <- "No Data"
                     }
                     
                   }) %>% bindCache(reporter_iso_sel())
                   
                   output$GDP_Year <- renderText({
                     df_Year <- df_demography_data()
                     Year <- df_Year[1, 9]
                     
                   }) %>% bindCache(reporter_iso_sel())
                   
                   output$Export <- renderText({
                     df_Export <- df_demography_data()
                     export <- df_Export[1, 10]
                     
                     if (is.numeric(export))
                     {
                       f_usdvaluetext(export)
                       
                     } else{
                       export
                     }
                   }) %>% bindCache(reporter_iso_sel())
                   
                   output$Import <- renderText({
                     df_Import <- df_demography_data()
                     import <- df_Import[1, 11]
                     
                     if (is.numeric(import))
                     {
                       f_usdvaluetext(import)
                       
                     } else{
                       import <- "No Data"
                     }
                     
                   }) %>% bindCache(reporter_iso_sel())
                   
                   output$Trade_Year <- renderText({
                     df_Trade_Year <- df_demography_data()
                     Trade_Year <- df_Trade_Year [1, 12]
                     
                   }) %>% bindCache(reporter_iso_sel())
                   
                   output$Trade_Year2 <- renderText({
                     df_Trade_Year <- df_demography_data()
                     Trade_Year <- df_Trade_Year [1, 12]
                     
                   }) %>% bindCache(reporter_iso_sel())
                   
                 })
               })
  
}
