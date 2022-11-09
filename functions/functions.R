#function to get narrative trade value in million, billion, trillion

f_tradevaltext <- function(tradeval) {
  tradeval <- str_trim(format(tradeval, scientific = FALSE), side="both")
  
  text <- case_when(
    str_length(tradeval) > 12 ~ paste("($",
                                      round(as.numeric(tradeval) / 1000000000000, 1),
                                      " trillion)",
                                      sep = ""),
    str_length(tradeval) > 9 &
      str_length(tradeval) <= 12 ~ paste("($",
                                         round(as.numeric(tradeval) / 1000000000, 1),
                                         " billion)",
                                         sep = ""),
    str_length(tradeval) > 6 &
      str_length(tradeval) <= 9 ~ paste("($",
                                        round(as.numeric(tradeval) / 1000000, 1),
                                        " million)",
                                        sep = ""),
    TRUE ~ paste("($",
                 accounting(as.numeric(tradeval), digits =0, format = "f", big.mark = ","),
                 ")",
                 sep = "")
  )
  return(text)
}

f_usdvaluetext_macro <- function(tradeval) {

  tradeval <- str_trim(format(tradeval, scientific = FALSE), side="both")

  text <- case_when(
    tradeval > 1000000000000 && tradeval < 1000000000 ~ paste("$",
                                      round(as.numeric(tradeval) / 1000000000000, 1),
                                      " trillion",
                                      sep = ""),
    tradeval >= 1000000000 && tradeval < 1000000 ~ paste("$",
                                         round(as.numeric(tradeval) / 1000000000, 1),
                                         " billion",
                                         sep = ""),
    tradeval >= 1000000 && tradeval < 1000  ~ paste("$",
                                        round(as.numeric(tradeval) / 1000000, 1),
                                        " million",
                                        sep = ""),
    tradeval >= 1000 ~ paste("$",
                                   round(as.numeric(tradeval) / 1000, 1),
                                   " thousands",
                                   sep = ""),
    tradeval <= -1000000000000 ~ paste("-$",
                                      round(as.numeric(tradeval) / -1000000000000, 1),
                                      " trillion",
                                      sep = ""),
    tradeval <= -1000000000 && tradeval > -1000000000000 ~ paste("-$",
                                   round(as.numeric(tradeval) / -1000000000, 1),
                                   " billion",
                                   sep = ""),
    tradeval <= -1000000 && tradeval > -1000000000 ~ paste("-$",
                                round(as.numeric(tradeval) / -1000000, 1),
                                " million",
                                sep = ""),
    tradeval <= -1000 && tradeval > -1000000 ~ paste("$",
                             round(as.numeric(tradeval) / -1000, 1),
                             " thousands",
                             sep = ""),
    tradeval >-1000 && tradeval <0 ~ paste("-$",
                                           round(as.numeric(tradeval) / -1, 1),
                                             " ",
                                             sep = ""),
    TRUE ~ paste("$",
                 accounting(as.numeric(tradeval), digits =0, format = "f", big.mark = ","),
                 " ",
                 sep = "")
  )
  return(text)
}


f_usdvaluetext <- function(tradeval) {
  
  tradeval <- round(tradeval, digits = 0)
  tradeval <- str_trim(format(tradeval, scientific = FALSE), side="both")

  text <- case_when(
    str_length(tradeval) > 12 ~ paste("$",
                                      round(as.numeric(tradeval) / 1000000000000, 1),
                                      " trillion",
                                      sep = ""),
    str_length(tradeval) > 9 &
      str_length(tradeval) <= 12 ~ paste("$",
                                         round(as.numeric(tradeval) / 1000000000, 1),
                                         " billion",
                                         sep = ""),
    str_length(tradeval) > 6 &
      str_length(tradeval) <= 9 ~ paste("$",
                                        round(as.numeric(tradeval) / 1000000, 1),
                                        " million",
                                        sep = ""),
    str_length(tradeval) > 3 &
      str_length(tradeval) <= 6 ~ paste("$",
                                        round(as.numeric(tradeval) / 1000, 1),
                                        " thousands",
                                        sep = ""),
    TRUE ~ paste("$",
                 accounting(as.numeric(tradeval), digits =0, format = "f", big.mark = ","),
                 " ",
                 sep = "")
  )
  return(text)
}

f_short_usdvaluetext <- function(tradeval) {
  text <- case_when(
    str_length(tradeval) > 12 ~ paste0("$",
                                 round(as.numeric(tradeval) / 1000000000000, 1),
                                 "T",
                                 sep = ""),
    str_length(tradeval) > 9 &
      str_length(tradeval) <= 12 ~ paste0("$",
                                    round(as.numeric(tradeval) / 1000000000, 1),
                                    "B",
                                    sep = ""),
    str_length(tradeval) > 6 &
      str_length(tradeval) <= 9 ~ paste0("$",
                                   round(as.numeric(tradeval) / 1000000, 1),
                                   "M",
                                   sep = ""),
    str_length(tradeval) > 3 &
      str_length(tradeval) <= 6 ~ paste("$",
                                        round(as.numeric(tradeval) / 1000, 1),
                                        "K",
                                        sep = ""),
    TRUE ~ paste0("$",
                 accounting(as.numeric(tradeval), digits =0, format = "f", big.mark = ","),
                 " ",
                 sep = "")
  )
  return(text)
}



f_usdvalueposnegtext <- function(tradeval) {

  if (tradeval > 0) {
    text <- case_when(
      str_length(tradeval) > 12 ~ paste("$",
                                   round(as.numeric(tradeval) / 1000000000000, 1),
                                   " trillion",
                                   sep = ""),
      str_length(tradeval) > 9 &
        str_length(tradeval) <= 12 ~ paste("$",
                                      round(as.numeric(tradeval) / 1000000000, 1),
                                      " billion",
                                      sep = ""),
      str_length(tradeval) > 6 &
        str_length(tradeval) <= 9 ~ paste("$",
                                     round(as.numeric(tradeval) / 1000000, 1),
                                     " million",
                                     sep = ""),
      str_length(tradeval) > 3 &
        str_length(tradeval) <= 6 ~ paste("$",
                                     round(as.numeric(tradeval) / 1000, 1),
                                     " thousands",
                                     sep = ""),
      TRUE ~ paste("$",
                   accounting(as.numeric(tradeval), digits =0, format = "f", big.mark = ","),
                   " ",
                   sep = "")
    )
  }

  else
  {
    text <- case_when(
      str_length(tradeval) > 12 ~ paste("-$",
                                   round(as.numeric(tradeval) / -1000000000000, 1),
                                   " trillion",
                                   sep = ""),
      str_length(tradeval) > 9 &
        str_length(tradeval) <= 12 ~ paste("-$",
                                      round(as.numeric(tradeval) / -1000000000, 1),
                                      " billion",
                                      sep = ""),
      str_length(tradeval) > 6 &
        str_length(tradeval) <= 9 ~ paste("-$",
                                     round(as.numeric(tradeval) / -1000000, 1),
                                     " million",
                                     sep = ""),
      str_length(tradeval) > 3 &
        str_length(tradeval) <= 6 ~ paste("$",
                                     round(as.numeric(tradeval) / -1000, 1),
                                     " thousands",
                                     sep = ""),
      TRUE ~ paste("$",
                   accounting(as.numeric(tradeval), digits =0, format = "f", big.mark = ","),
                   " ",
                   sep = "")
    )
  }

  return(text)
}



f_valuetext <- function(tradeval) {
  text <- case_when(
    str_length(tradeval) > 12 ~ paste(round(as.numeric(tradeval) / 1000000000000, 1),
                                 " trillion",
                                 sep = ""),
    str_length(tradeval) > 9 &
      str_length(tradeval) <= 12 ~ paste(round(as.numeric(tradeval) / 1000000000, 1),
                                    " billion",
                                    sep = ""),
    str_length(tradeval) > 6 &
      str_length(tradeval) <= 9 ~ paste(round(as.numeric(tradeval) / 1000000, 1),
                                   " million",
                                   sep = ""),
    str_length(tradeval) > 4 &
      str_length(tradeval) <= 6 ~ paste("$",
                                   round(as.numeric(tradeval) / 1000, 1),
                                   " thousands",
                                   sep = ""),
    TRUE ~ paste(accounting(as.numeric(tradeval), digits =0, format = "f", big.mark = ","),
                 " ",
                 sep = "")
  )
  return(text)
}


# Customised TRUE-FALSE switch button for Rshiny
# Only sing CSS3 code (No javascript)
#
# Sébastien Rochette
# http://statnmap.com/en/
# April 2016
#
# CSS3 code was found on https://proto.io/freebies/onoff/
# For CSS3 customisation, refer to this website.

#' A function to change the Original checkbox of rshiny
#' into a nice true/false or on/off switch button
#' No javascript involved. Only CSS code.
#' 
#' To be used with CSS script 'button.css' stored in a 'www' folder in your Shiny app folder
#' 
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param value Initial value (TRUE or FALSE).
#' @param col Color set of the switch button. Choose between "GB" (Grey-Blue) and "RG" (Red-Green)
#' @param type Text type of the button. Choose between "TF" (TRUE - FALSE), "OO" (ON - OFF) or leave empty for no text.

f_switchButton <- function(inputId, label, value=FALSE, col = "GB", type="TF") {
  
  # color class
  if (col != "RG" & col != "GB") {
    stop("Please choose a color between \"RG\" (Red-Green) 
      and \"GB\" (Grey-Blue).")
  }
  if (!type %in% c("OO", "TF", "YN")){
    warning("No known text type (\"OO\", \"TF\" or \"YN\") have been specified, 
     button will be empty of text") 
  }
  if(col == "RG"){colclass <- "RedGreen"}
  if(col == "GB"){colclass <- "GreyBlue"}
  if(type == "OO"){colclass <- paste(colclass,"OnOff")}
  if(type == "TF"){colclass <- paste(colclass,"TrueFalse")}
  if(type == "YN"){colclass <- paste(colclass,"YesNo")}
  
  # No javascript button - total CSS3
  # As there is no javascript, the "checked" value implies to
  # duplicate code for giving the possibility to choose default value
  
  if(value){
    tagList(
      tags$div(class = "form-group shiny-input-container",
               tags$div(class = colclass,
                        tags$div(class = "onoffswitch",
                                 tags$input(type = "checkbox", name = "onoffswitch", class = "onoffswitch-checkbox",
                                            id = inputId, checked = ""
                                 ),
                                 tags$label(class = "onoffswitch-label", `for` = inputId,
                                            tags$span(class = "onoffswitch-inner"),
                                            tags$span(class = "onoffswitch-switch")
                                 )
                        ),
                        tags$label(label, class = "control-label")
               )
      )
    )
  } else {
    tagList(
      tags$div(class = "form-group shiny-input-container",
               tags$div(class = colclass,
                        tags$label(label, class = "control-label"),
                        tags$div(class = "onoffswitch",
                                 tags$input(type = "checkbox", name = "onoffswitch", class = "onoffswitch-checkbox",
                                            id = inputId
                                 ),
                                 tags$label(class = "onoffswitch-label", `for` = inputId,
                                            tags$span(class = "onoffswitch-inner"),
                                            tags$span(class = "onoffswitch-switch")
                                 )
                        )
               )
      )
    ) 
  }
}


valueBox2 <- function(value, title, subtitle,subtitle1,value1,year, icon = NULL, color = "aqua", width = 0.5, href = NULL){

  #shinydashboard:::validateColor(color)

  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")

  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title),
      h3(value),
      h3(value1),
      p(year),
      p(subtitle),
      p(subtitle1)
    ),
    if (!is.null(icon)) div(class = "icon-large", icon)
  )

  if (!is.null(href))
    boxContent <- a(href = href, boxContent)

  div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    boxContent
  )
}