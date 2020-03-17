rm(list=ls())
if(!require(plotly)){install.packages("plotly")}
if(!require(lubridate)){install.packages("lubridate")}
if(!require(htmlwidgets)){install.packages("htmlwidgets")}
if(!require(jsonlite)){install.packages("lsonlite")}


json_file <- "https://pomber.github.io/covid19/timeseries.json"
json_data <- fromJSON(json_file)


colombia_data <- json_data$Colombia
colombia_data$date <- ymd(colombia_data$date)
venezuela_data <- json_data$Venezuela
venezuela_data$date <- ymd(venezuela_data$date)
p <- plot_ly(  x = colombia_data$date, y = colombia_data$confirmed, type ='bar', color = I("plum4") )%>%
    layout(yaxis = list(title = 'Confirmados Colombia COVID 19'))
    htmlwidgets::saveWidget(as_widget(p), "/tmp/covid19_col.html")
ggplotly(p)

compare_data <- cbind(colombia_data,venezuela_data)
colnames(compare_data) <- c("date","confirmados_col","deaths_col","recovered_col", "date2", "confirmed_ve", "deaths_ve", "recovered_ve")
pm <- plot_ly(  x = compare_data$date, y = compare_data$confirmados_col, type ='bar', color = I("plum4"), name= "Colombia" )%>%
    add_trace(y =  compare_data$confirmed_ve, name = 'Venezuela', color = I("darkolivegreen4"))%>%
    layout(yaxis = list(title = 'Confirmados compare  COVID 19'), barmode = 'group')
htmlwidgets::saveWidget(as_widget(p), "/tmp/covid19_w.html")
ggplotly(pm)
