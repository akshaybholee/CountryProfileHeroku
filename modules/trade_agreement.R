
trade_agreement_ui <- function(id) {
  ns <- NS(id)
  
  tagList(shinyjs::useShinyjs(),
          fluidRow(column(
            8,
            offset = 2,
            shinydashboard::box(id = ns("TR"),
              solidHeader = TRUE,
              column(12, align = 'left', h4(strong("Trade Agreement"))),
              br(),
              column(
                12,
                align = 'left',
                f_switchButton(
                  inputId = ns("tog"),
                  label = HTML('<font size="-1"><em> Switch to map or table</em></font>'),
                  value = TRUE,
                  col = "RG",
                  type = "TF"
                )
              ),
              br(),
              column(
                12,
                align = 'left',
                # highchartOutput(outputId = ns("map"), height = "700px"),
                # plotlyOutput(outputId = ns("map"), height = "700px"),
                br(),

                  leafletOutput(outputId = ns("map"),height = "650px", width = "100%")
              ),
              column(
                12,
                align = 'left',
                dataTableOutput(outputId = ns("table1"), height = "700px")
              ),
              width = 16
            )
            
          )))
  
}

trade_agreement_server <- function(id, reporter_iso_sel) {
  moduleServer(id,
               
               function(input, output, session) {
         
                 # data(worldgeojson, package = "highcharter")
                 print("start trade agreement")
                 df_agreement <- reactive({
                   query <-   paste0(
                     "select distinct destination_ISO ISO3,destination_ISO Country_ISO,  WTO_Name Name, WTO_Name,  Entry_Force_Year, Region_con
   from agreement a,
   trade_agreement ta
   where a.agreement_id = ta.agreement_id
   and source_ISO = '",
                     reporter_iso_sel(),
                     "'
   union
   select distinct source_ISO,source_ISO Country_ISo, WTO_Name Name, WTO_Name,  Entry_Force_Year, Region_con
   from agreement a,
   trade_agreement ta
   where a.agreement_id = ta.agreement_id
   and destination_ISO = '",
                     reporter_iso_sel(),
                     "'"
                   )
                   
                   dbGetQuery(con, query)
                   
                   
                 }) %>% bindCache(reporter_iso_sel())
                 
                 
                 
                 # colors <-  c(
                 #   '#c3ffff',
                 #   '#a7f0f8',
                 #   '#8be2f2',
                 #   '#6fd2ee',
                 #   '#54c3ea',
                 #   '#38b3e5',
                 #   '#1ca2e0',
                 #   '#0291da'
                 # )
                 
                 colors <-  c(
                   '#6ec6de',
                   '#60b9d4',
                   '#52abca',
                   '#459dc0',
                   '#3990b6',
                   '#2c83ab',
                   '#1f76a1',
                   '#116996'
                 )
                 
                 df_map <-
                   reactive({
                     df_agreement() %>% dplyr::count(ISO3)
                   }) %>% bindCache(reporter_iso_sel())
            
                 
                 
                 output$map <- renderLeaflet({
                
                   z <- df_map()
                   
                   z$Country_ISO <- z$ISO3
                   
                  z <-  merge(x = z, y = df_country, by = "Country_ISO")
                   
                   z <-z %>% mutate(
                     tooltip = case_when(n == 1 ~ paste(n, 'agreement with ', Country),
                                         n > 1 ~ paste(n, 'agreements with ', Country),
                                         TRUE ~  paste('No agreement with ', Country))
                   )
                   
                   sPDF <- joinCountryData2Map(z
                                               ,joinCode = "ISO3"
                                               ,nameJoinColumn = "ISO3") 
                   
                   # sPDF <- sp.na.omit(sPDF,"n", margin = 1)
                   
                   pal <- colorNumeric(
                     palette = c(
                       '#6ec6de',
                       '#60b9d4',
                       '#52abca',
                       '#459dc0',
                       '#3990b6',
                       '#2c83ab',
                       '#1f76a1',
                       '#116996'
                     ),
                     domain = z$n,
                     na.color = "#e2e2e2")
                   
                   factop <- function(x) {
                     ifelse(is.na(x), 0, 1)
                   }
                   
                   leaflet(options = leafletOptions(attributionControl = FALSE,zoomControl = FALSE,zoomSnap = 0.1)) %>%
                     addTiles(urlTemplate = "https://api.mapbox.com/styles/v1/bholee/cl75rvfqs002q14o0rwzd6oe5/tiles/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYmhvbGVlIiwiYSI6ImNrN2tibG9pNzAwajMzbWw4ZnlpcDNqY2wifQ.o-qJAmRdkh-McoubI4E2DA"
                              , options = tileOptions(noWrap = FALSE, minZoom = 2.1,
                                                      maxZoom = 2.1,)
                              ) %>% 
                     setView(lng = 0,lat = 20, zoom = 2.1) %>%
                     setMaxBounds(lng1 = 58.995311187950925
                                  , lat1 =  -174.0234375
                                  , lng2 = -58.995311187950925
                                  , lat2 = 223.2421875) %>%
                     addPolygons(data=sPDF, weight = 1.5, fillColor = ~pal(n) ,color="white", fillOpacity = 1, highlight = highlightOptions(
                       fillColor = '#1c5d99'),label=~as.character(tooltip)) %>%
                     addLegendNumeric(
                       pal = pal,
                       values = z$n,
                       position = 'bottomleft',
                       title = 'Number of agreements',
                       orientation = 'horizontal',
                       shape = 'rect',
                       decreasing = FALSE,
                       height = 10,
                       width = 200,
                       tickLength = 0,
                       tickWidth = 0
                     )
                   
               
                   }) %>% bindCache(reporter_iso_sel())
                 
                 output$table1 <- renderDataTable({
                   df_merge <-
                     merge(x = df_agreement(), y = df_country, by = "Country_ISO")
                   
                   
                   df_merge <-
                     df_merge %>% select(c(
                       'Country',
                       'Name',
                       'Entry_Force_Year',
                       'Region_con'
                     ))
                   
                   names(df_merge) <-
                     c('Agreement with',
                       'Agreement Name',
                       'Year in force',
                       'Region')
                   
                   
                   z1 <-
                     datatable(
                       df_merge,
                       height = '100%',
                       rownames = FALSE
                       ,
                       options = list(
                         dom = 't',
                         lengthChange = FALSE,
                         bPaginate = FALSE,
                         scrollCollapse = TRUE,
                         scrollY = "620px",
                         initComplete = JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#0291da', 'color': 'white'});
                                                                  $('td').css({'border': 'none'});
                                                                   $('th').css({'border': 'none'});",
                           "}"
                         ),
                         columnDefs = list(
                           list(width = '150px', targets = c(0)),
                           list(width = '300px', targets = c(1)),
                           list(width = '75px', targets = c(2)),
                           list(width = '200px', targets = c(3)),
                           list(className = 'text-center', targets = c(2))
                         )

                       )
                     )
                   
                   z1
                   
                 }) 
                 
              
                 # observe({
                 #   shinyjs::toggle("map", condition = input$tog)
                 #   shinyjs::toggle("table1", condition = !input$tog)
                 # })
                 
                 # observeEvent(input$tog, {
                 #   shinyjs::show("agreement-map",asis = TRUE)
                 #   shinyjs::hide("agreement-table1",asis = TRUE)
                 # })
                 # 
                 # observeEvent(!input$tog, {
                 #   shinyjs::hide("map",asis = TRUE)
                 #   shinyjs::show("table1",asis = TRUE)
                 # })
                 
                

                observe({
                
                  if(input$tog){
                    shinyjs::hide("table1")
                    shinyjs::show("map")
                                  }
                  else{
                    shinyjs::show("table1")
                    shinyjs::hide("map")
                  }
                  
                  if(nrow(df_agreement())==0)
                  {
                    shinyjs::hide("TR")
                  }
                })
                
                 
               })
  
}