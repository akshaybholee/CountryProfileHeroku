# init.R
#
# Example R code to install packages if not already installed
#
helpers.installPackages("rgdal")

my_packages = c("shinydashboard", "shinyjs",
                "renv","formattable",
                "dplyr","stringr",
                "sqldf","echarts4r",
                "chorddiag","scales",
                "highcharter","DT",
                "plyr","leaflet",
                "rworldmap","leaflet.extras",
                "spatialEco","leaflegend",
                "odbc","pool",
                "shinydashboardPlus","waiter",
                "feather","terra","units")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
