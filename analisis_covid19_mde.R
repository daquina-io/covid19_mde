rm(list=ls())
if(!require(plotly)){install.packages("plotly")}
if(!require(lubridate)){install.packages("lubridate")}
if(!require(htmlwidgets)){install.packages("htmlwidgets")}
install.packages("jsonlite")


library("jsonlite")
json_file <- "https://pomber.github.io/covid19/timeseries.json"
json_data <- fromJSON(json_file)
colombia_data <- json_data$Colombia
colombia_data$date <- ymd(colombia_data$date)
p <- plot_ly(  x = colombia_data$date, y = colombia_data$confirmed, type ='bar', color = I("plum4") )%>%
    layout(yaxis = list(title = 'Confirmados Colombia COVID 19'))
    htmlwidgets::saveWidget(as_widget(p), "/tmp/HoursPerWeek.html")
ggplotly(p)
