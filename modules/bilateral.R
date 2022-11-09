bilateral_ui <- function(id) {
  ns <- NS(id)
  
  tagList(fluidRow(column(
    8,
    offset = 2,
    shinydashboard::box(
      solidHeader = TRUE,
      column(4, align = 'left', htmlOutput(outputId = ns("Title"),container = tags$h4,
                                           class = "title")),
      column(
        5,
        align = 'right',
        selectInput(
          inputId = ns('tradeflow'),
          label = 'Trade Flow',
          choices =  list("Export", "Import"),
          selected = "Export"
        )
      ),
      column(
        3,
        align = 'right',
        selectInput(
          inputId = ns('Partner'),
          label = 'Partner',
          choices = NULL
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
      column(5,
             align = 'left',
             htmlOutput(outputId = ns("bilateraltext"))),
      column(
        7,
        align = 'left',
        chorddiagOutput(
          outputId = ns("distPlot"),
          height = 500,
          width = "100%"
        )
      ),
      width = 16,
      height = 535
    )
  )))
  
}

bilateral_server <-
  function(id,
           reporter_iso_sel,
           reporter,
           mycolor
           ) {
    moduleServer(id,
                 
                 function(input, output, session) {
                   print("start bilateral")
                   # get the trade data from database table
                   df_trade_sector <- 
                     reactive({
                       print("df_trade_sector")
                       reporter_iso <- reporter_iso_sel()
                       
                       tradeflow <-  input$tradeflow
                 
                       dbGetQuery(
                         con,
                         paste(
                           " select year,Reporter, Partner, Trade_Flow tradeflow, sector SECTOR_DESCRIPTION, Trade_Value tradevalue
from Bilateral_Trade_Sector t
where  reporter_iso = '",
                           reporter_iso,
                           "' and Trade_Flow =  '",
                           tradeflow,"'",
                           sep = ""
                         )
                       )
                    
                
                     }) %>% bindCache(reporter_iso_sel(),input$tradeflow)
                  
                   
                   # Filter the data by the selected partner
                   df_sector <- reactive({
                     print("df_sector")
                     dfsector <- df_trade_sector()
                     
                     df <- dfsector %>% filter(Partner == input$Partner) %>% select(c('year','Reporter','SECTOR_DESCRIPTION','tradevalue'))
                  
                     
                     df
                     
                   }) %>% bindCache(reporter_iso_sel(),input$tradeflow, input$Partner)
                   
                   # update the select input Partner to display only partners for the selected reporter in the dropdown
                 observe(priority = 1, {
                     # if (input$Partner == "")
                     # {
                     #   select <- 'World'
                     # }
                     # else {
                     #   select <- input$Partner
                     # }

                     updateSelectInput(
                       session,
                       'Partner',
                       choices = sort(df_trade_sector()$Partner),
                       # server = TRUE,
                       selected = 'World'
                     )
                   })
                   
                   
                   
                   observe({
                     req(nrow(df_sector()) > 0)
                     
                     # transform the data into a square matrix to pass in the chorddiag function to generate chord diagram
                     df_sector_matrix <- reactive({
                       dfsector <- df_sector()
                       
                       dfsector <-
                         dfsector[order(-dfsector$tradevalue),]
                       
                       
                       if (max(dfsector$tradevalue) >= 10000)
                       {
                         dfsector$tradevalue <- round(dfsector$tradevalue / 1000000, 2)
                       }
                       
                       
                       
                       nameRow <- unique(unlist(dfsector[3]))
                       nameCol <- unique(unlist(dfsector[2]))
                       
                       # construct 0 matrix of correct dimensions with row and column names
                       df_matrix <-
                         matrix(0,
                                length(nameRow),
                                length(nameCol),
                                dimnames = list(nameRow, nameCol))
                       
                       # fill in the matrix with matrix indexing on row and column names
                       df_matrix[as.matrix(dfsector[c("SECTOR_DESCRIPTION", "Reporter")])] <-
                         dfsector[["tradevalue"]]
                       
                       
                       df_matrix
                     }) %>% bindCache(reporter_iso_sel(),input$tradeflow, input$Partner)
                     
                     output$Title <-
                       renderText({
                         print("bilateral renderText Title")
                         HTML(
                           paste0(
                             "Bilateral Trade | <strong style='color:#0291da'>",
                             input$tradeflow,
                             "</strong>"
                           )
                         )
                       })  %>% bindCache(input$tradeflow)
                     
                     # Output the chord diagram on the screen
                     output$distPlot <- renderChorddiag({
                       print("bilateral renderChorddiag distPlot")
                       df_matrix <- df_sector_matrix()
                       
                       dfsector <- df_sector()
                       
                       if (max(dfsector$tradevalue) < 10000)
                       {
                         unit <- ""
                       }
                       else {
                         unit <- "M"
                       }
                       
                       # color <-
                       #   paletteer_d("ggthemes::Tableau_20",
                       #               type = "discrete",
                       #               direction = -1)
                       
                       color <- as.list(mycolor()$color)
                       
                       if (nrow(df_sector()) > 0)
                       {
                         chorddiag(
                           df_matrix,
                           showTicks = F,
                           groupnameFontsize = 14,
                           groupnamePadding = 5,
                           groupPadding = 5,
                           margin = 110,
                           tooltipUnit =  unit,
                           # precision = 2,
                           groupThickness = 0.05,
                           type = "bipartite",
                           groupColors = color,
                           chordedgeColor = color,
                           fadeLevel = 0.8,
                           showZeroTooltips = FALSE
                         )
                         
                       }
                       
                       
                       
                     }) %>% bindCache(reporter_iso_sel(), input$Partner, input$tradeflow)
                     
                     # Output the narrative text on the screen
                     output$bilateraltext <-
                       renderText({
                         print("bilateral renderText bilateraltext")
                         export_text <- ""
                         import_text <- ""
                         
                         df <- df_sector()
                         
                         # get narrative texts from database
                         df_narrative <-
                           dbGetQuery(
                             con,
                             "select text_category, HTML_text from Dashboard_Narrative where project_name = 'Country Profile' and section_code = 'BT01'"
                           )
                         
                         # df$tradevalue <- df$tradevalue * 1000000
                         
                         df_sorted <- df[order(-df$tradevalue),]
                         
                         
                         df_sorted <-  df_sorted %>%
                           mutate(stringdollar = f_usdvaluetext(df_sorted$tradevalue))
                         
                         if (input$tradeflow == 'Export')
                         {
                           import_text <- ""
                           
                           if (input$Partner == 'World')
                           {
                             to_partner_text <- "the World"
                             
                           }
                           else
                           {
                             to_partner_text <- input$Partner
                           }
                           
                           export_narrative <-
                             df_narrative$HTML_text[df_narrative$text_category == 'Export']
                           
                           if (nrow(df_sorted) >= 3)
                           {
                             export_text <-
                               export_narrative %>% str_replace_all(
                                 c(
                                   "YEAR" = df_sorted$year[[1]],
                                   "REPORTER" = reporter(),
                                   "SECTOR1" = df_sorted$SECTOR_DESCRIPTION[[1]],
                                   "VALUE1" = df_sorted$stringdollar[[1]],
                                   "SECTOR2" = df_sorted$SECTOR_DESCRIPTION[[2]],
                                   "VALUE2" = df_sorted$stringdollar[[2]],
                                   "SECTOR3" =  df_sorted$SECTOR_DESCRIPTION[[3]],
                                   "VALUE3" = df_sorted$stringdollar[[3]],
                                   "PARTNER" = to_partner_text
                                 )
                               )
                           }
                           else if (nrow(df_sorted) == 2)
                           {
                             export_text <-
                               export_narrative %>% str_replace_all(
                                 c(
                                   "YEAR" = df_sorted$year[[1]],
                                   "REPORTER" = reporter(),
                                   "SECTOR1" = df_sorted$SECTOR_DESCRIPTION[[1]],
                                   "VALUE1" = df_sorted$stringdollar[[1]],
                                   "SECTOR2" = paste("and", df_sorted$SECTOR_DESCRIPTION[[2]], sep = " "),
                                   "VALUE2" = df_sorted$stringdollar[[2]],
                                   ", and VALUE3 in SECTOR3" =  "",
                                   "PARTNER" = to_partner_text
                                 )
                               )
                             
                           }
                           else if (nrow(df_sorted) == 1)
                           {
                             export_text <-
                               export_narrative %>% str_replace_all(
                                 c(
                                   "YEAR" = df_sorted$year[[1]],
                                   "REPORTER" = reporter(),
                                   "SECTOR1" = df_sorted$SECTOR_DESCRIPTION[[1]],
                                   "VALUE1" = df_sorted$stringdollar[[1]],
                                   ", VALUE2 in SECTOR2, and VALUE3 in SECTOR3" =  "",
                                   "PARTNER" = to_partner_text
                                 )
                               )
                           }
                           
                         }
                         
                         
                         import_narrative <-
                           df_narrative$HTML_text[df_narrative$text_category == 'Import']
                         
                         if (input$tradeflow == 'Import')
                         {
                           export_text <- ""
                           
                           if (input$Partner == 'World')
                           {
                             from_partner_text <- "the World"
                           }
                           else
                           {
                             from_partner_text <- input$Partner
                           }
                           
                           if (nrow(df_sorted) >= 3)
                           {
                             import_text <-
                               import_narrative %>% str_replace_all(
                                 c(
                                   "YEAR" = df_sorted$year[[1]],
                                   "REPORTER" = reporter(),
                                   "SECTOR1" = df_sorted$SECTOR_DESCRIPTION[[1]],
                                   "VALUE1" = df_sorted$stringdollar[[1]],
                                   "SECTOR2" = df_sorted$SECTOR_DESCRIPTION[[2]],
                                   "VALUE2" = df_sorted$stringdollar[[2]],
                                   "SECTOR3" =  df_sorted$SECTOR_DESCRIPTION[[3]],
                                   "VALUE3" = df_sorted$stringdollar[[3]],
                                   "PARTNER" = from_partner_text
                                 )
                               )
                           }
                           else if (nrow(df_sorted) == 2)
                           {
                             import_text <-
                               import_narrative %>% str_replace_all(
                                 c(
                                   "YEAR" = df_sorted$year[[1]],
                                   "REPORTER" = reporter(),
                                   "SECTOR1" = df_sorted$SECTOR_DESCRIPTION[[1]],
                                   "VALUE1" = df_sorted$stringdollar[[1]],
                                   "SECTOR2" = paste("and", df_sorted$SECTOR_DESCRIPTION[[2]], sep = " "),
                                   "VALUE2" = df_sorted$stringdollar[[2]],
                                   ", and VALUE3 in SECTOR3" =  "",
                                   "PARTNER" = from_partner_text
                                 )
                               )
                             
                           }
                           else if (nrow(df_sorted) == 1)
                           {
                             import_text <-
                               import_narrative %>% str_replace_all(
                                 c(
                                   "YEAR" = df_sorted$year[[1]],
                                   "REPORTER" = reporter(),
                                   "SECTOR1" = df_sorted$SECTOR_DESCRIPTION[[1]],
                                   "VALUE1" = df_sorted$stringdollar[[1]],
                                   ", VALUE2 in SECTOR2, and VALUE3 in SECTOR3" =  "",
                                   "PARTNER" = from_partner_text
                                 )
                               )
                           }
                         }
                         
                         if (export_text != "" || import_text != "")
                         {
                           HTML(
                             paste("<br/><br/>",
                               df_narrative$HTML_text[df_narrative$text_category == 'General']
                               ,
                               export_text,
                               import_text,
                               sep = ""
                             )
                           )
                         }
                         
                       })  %>% bindCache(reporter_iso_sel(),input$tradeflow, input$Partner)
                   })
                   
                 })
    
  }