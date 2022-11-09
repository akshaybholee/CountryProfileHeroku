reporter_download_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
          fluidRow(column(
    8, offset = 2, shinydashboard::box(
      solidHeader = TRUE,
      column(6,selectInput(
        inputId =   ns("Reporter"),
        label = "",
        choices =  df_country$Country,
        selected = "United States of America"
      ), class = "Reporter_Padding"),  
      column(6, align = 'right',  class = 'download_padding', actionButton(
        inputId =   ns("download"),
        label = "Download Report",
        class = 'download_button',
        width = 150
      )
      ),
      width = 16,
      
    )
    
  )))
  
}

reporter_download_server <- function(id) {
  moduleServer(id,
               
               function(input, output, session) {
                 reporter_iso_sel <-  reactive({
                   reporter_iso <-
                     df_country$Country_ISO[df_country$Country == input$Reporter]
                   reporter_iso
                 }) %>% bindCache(input$Reporter)
                 
                 
                 return(list(
                   reporter_iso_sel = reporter_iso_sel,
                   reporter = reactive(input$Reporter)
                 ))
                 
               })
  
}

download_server <- function(id) {
  moduleServer( id,
                function(input, output, session){
                  observeEvent(input$download, {
                    screenshot( filename = "Country Profile")
                  })
                }

  )
}