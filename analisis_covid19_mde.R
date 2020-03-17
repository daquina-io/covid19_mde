if(!require(plotly)){install.packages("plotly")}
if(!require(lubridate)){install.packages("lubridate")}
if(!require(htmlwidgets)){install.packages("htmlwidgets")}
install.packages("rjson")


library("rjson")
json_file <- "https://pomber.github.io/covid19/timeseries.json"
json_data <- fromJSON(file=json_file)
colombia_data <- json_data$Colombia

p <- plot_ly(  x = colombia_data[[55]]$date, y = colombia_data[[55]]$confirmed, type ='bar', color = I("plum4") )%>%
    layout(yaxis = list(title = 'Confirmados Colombia'))
    htmlwidgets::saveWidget(as_widget(p), "/tmp/HoursPerWeek.html")
ggplotly(p)
