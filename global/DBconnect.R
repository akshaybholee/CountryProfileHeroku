# use this connection when running application on local machine
# con <- dbPool(odbc(),
#               Driver = "SQL Server",
#               Server = "iecproduction.database.windows.net",
#               Database = "IECPRODDB",
#               UID = "IECPROD",
#               PWD = "International123")

# use this connection when deploying application to shinyapps.io
con <- dbPool(odbc(),
              Driver = "FreeTDS",
              Server = "iecproduction.database.windows.net",
              Database = "IECPRODDB",
              UID = "IECPROD",
              PWD = "International123",
              port = 1433)