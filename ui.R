#Libraries
source("global/libraries.R")

#### call modules #####
list.files("modules") %>% purrr::map(~ source(paste0("modules/", .)))
#### call modules #####


#### Connect to Production database #####
source("global/DBconnect.R")
#### End Connect to database #####


#### Initialize global dataframe ####
source("global/dataframe.R")
#### End Initialize global dataframe ####

#### Initialize global functions#### 
source("functions/functions.R")
#### End Initialize global functions ####

shinyOptions(cache = cachem::cache_disk("./app-cache"))

#### Create User Interface #####
ui <-
  tagList(  
    shinyjs::useShinyjs(),
    tags$head(# the javascript is checking the screen resolution to adapt the display
      tags$script(src = "javascripts.js")),
    tags$style(HTML(
      paste(
        "html,",
        ".container{
                    width: 100%;
                    margin: 0 auto;
                    padding: 0;
                }
               @media screen and (min-width: 700px){
                .container{
                    min-width: 1850px;
                    max-width: 1920px;
                }
               }
                          ",
        sep = " "
      )
    )),
    tags$div(id = "all",
             class = "container",
             dashboardPage(
               preloader = list(html = tagList(
                 spin_3(),
                 HTML('<font color="#000000">Country Profile</font>')
               ), color = "#fff"),
               header = dashboardHeader(disable = TRUE),
               dashboardSidebar(width = "0px"),
               body = dashboardBody(
                 tags$head(
                   tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
                 ),
                 #UI for Country
                 reporter_download_ui(id = "reporterdownload"),
                 #End of UI for Country
                 
                 #UI for Demographics
                 demographics_ui(id = "demographics"),
                 #End of UI for Demographics
                 
                 #UI for Macroeconomic
                 Macroeconomic_ui(id = "Macroeconomic"),
                 #End of UI Macroeconomic
                 
                 #UI for Trade in goods
                 tradeingoods_ui(id = "tradeingoods"),
                 #End of UI for Trade in goods
                 
                 #UI for Trade Performance
                 trade_performance_ui(id = "tradeperformance"),
                 #End of UI Trade Performance
                 
                 #UI for Logistic Performance
                 LPI_ui(id = "LPI"),
                 #End of UI Logistic Performance
                 
                 # #UI for Bilateral Trade
                 bilateral_ui(id = "bilateral"),
                 #End of UI Bilateral Trade
                 
                 #UI for Trade Agreements
                 trade_agreement_ui(id = "agreement"),
                 #End of UI Trade Agreements
                 
                 #UI for Trade in services
                 tradeinservices_ui(id = "tradeinservices"),
                 #End of UI Trade in services
                 
                 #UI for Services Performance
                 servicesperformance_ui(id = 'servicesperformance'),
                 #End of UI Services Performance
                 #
                 #UI for Digital
                 Digital_ui(id = "Digital"),
                 #End of UI Digital
                 
                 #UI for Digital
                 trade_facilitation_ui(id = "tradefacilitation")
                 #End of UI Digital
                 
               )
             )
    )
  )
#### End Create User Interface #####