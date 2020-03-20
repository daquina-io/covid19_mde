rm(list=ls())
if(!require(plotly)){install.packages("plotly")}
if(!require(lubridate)){install.packages("lubridate")}
if(!require(htmlwidgets)){install.packages("htmlwidgets")}
if(!require(jsonlite)){install.packages("lsonlite")}
if(!require(purrr)){install.packages("purrr")}


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

##
## Datos de instituto nacional de salud
##
data <- fromJSON("https://e.infogram.com/api/live/flex/0e44ab71-9a20-43ab-89b3-0e73c594668f/dfee1a5c-5cc8-4e90-8efb-d5bdf2803bf6?")
ninfectados <- dim(data$data)[2]

infectados_df <- map(1:ninfectados, function(x) {
    data$data[1,x,]
}) %>% unlist(.) %>% matrix(.,nrow=ninfectados, byrow = TRUE) %>% data.frame(., stringsAsFactors = FALSE)

colnames(infectados_df) <- infectados_df[1,]
infectados_df <- infectados_df[-1,]

infectados_df
colnames(infectados_df) <- c("id", "date", "city", "localization", "age", "sex", "type", "origin" )
infectados_df$date <- dmy(infectados_df$date)

## Medellin
mde_infectados_df <-  dplyr::filter(infectados_df, grepl("Mede",city))
mde_infectados_df <- cbind(Row.Names = rownames(mde_infectados_df), mde_infectados_df)
## corrige formato de fechas TODO sistematizar
mde_infectados_df[3,3] = "2020-03-11"
mde_infectados_df[2,3] = "2020-03-11"
mde_infectados_df[1,3] = "2020-03-09"
## deja el máximo diario TODO sistematizar
mde_total_dia <- mde_infectados_df[c(1, 3, 5, 9, 18), ]
## columna manual de totales (Row.Names la lee como factor)
mde_total_dia$total <- c(1,3,5,9,18)

## NO FUNCIONA: la columna Row.Names la lee como un factor
p <- plot_ly(  x = mde_total_dia$date, y = mde_total_dia$total, type ='bar', color = I("plum4") )%>%
    layout(yaxis = list(title = 'Confirmados Medellín COVID19'))
    htmlwidgets::saveWidget(as_widget(p), "/tmp/covid19_mde.html")
ggplotly(p)
