rm(list=ls())

if(!require(plotly)){install.packages("plotly")}
if(!require(lubridate)){install.packages("lubridate")}
if(!require(htmlwidgets)){install.packages("htmlwidgets")}
if(!require(jsonlite)){install.packages("lsonlite")}
if(!require(purrr)){install.packages("purrr")}
if(!require(profvis)){install.packages("profvis")}
##profvis({
#json_file <- "https://pomber.github.io/covid19/timeseries.json"
#json_data <- fromJSON(json_file)

## api via carlos valencia http://coronavirus-19-api.herokuapp.com/countries/
## API datos abiertos colombia  https://www.datos.gov.co/resource/gt2j-8ykr.json
## colombia_data <- json_data$Colombia

## colombia_data$date <- ymd(colombia_data$date)
## venezuela_data <- json_data$Venezuela
## venezuela_data$date <- ymd(venezuela_data$date)
## p <- plot_ly(  x = colombia_data$date, y = colombia_data$confirmed, type ='bar', color = I("plum4") )%>%
##     layout(yaxis = list(title = 'Confirmados Colombia COVID 19'))

## ggplotly(p)

## compare_data <- cbind(colombia_data,venezuela_data)
## colnames(compare_data) <- c("date","confirmados_col","deaths_col","recovered_col", "date2", "confirmed_ve", "deaths_ve", "recovered_ve")
## pm <- plot_ly(  x = compare_data$date, y = compare_data$confirmados_col, type ='bar', color = I("plum4"), name= "Colombia" )%>%
##     add_trace(y =  compare_data$confirmed_ve, name = 'Venezuela', color = I("darkolive#00bc8c4"))%>%
##     layout(yaxis = list(title = 'Confirmados compare  COVID 19'), barmode = 'group')
## htmlwidgets::saveWidget(as_widget(p), "/tmp/covid19_w.html")
## ggplotly(pm)

##
## Datos de instituto nacional de salud
##
data <- fromJSON("https://infogram.com/api/live/flex/bc384047-e71c-47d9-b606-1eb6a29962e3/664bc407-2569-4ab8-b7fb-9deb668ddb7a")
ninfectados <- dim(data$data)[2]

infectados_df <- map(1:ninfectados, function(x) {
    data$data[1,x,]
}) %>% unlist(.) %>% matrix(.,nrow=ninfectados, byrow = TRUE) %>% data.frame(., stringsAsFactors = FALSE)

colnames(infectados_df) <- infectados_df[1,]
infectados_df <- infectados_df[-1,]


##infectados_df

colnames(infectados_df) <- c("id", "date", "city", "localization","status", "age", "sex", "type", "origin" )

#infectados_df$date <- dmy(infectados_df$date)

## Dataset
ultimaFecha <- max(infectados_df$date, na.rm=TRUE)
## Colombia
totalColombiaInfectados <- max(as.numeric(infectados_df$id), na.rm=TRUE)
fallecidos <- infectados_df %>% dplyr::filter(grepl("Fallecido",status)) %>% nrow()
recuperados <- infectados_df %>% dplyr::filter(grepl("Recuperado",status)) %>% nrow()
## Medellin
mde_infectados_df <-  dplyr::filter(infectados_df, grepl("Mede",city))
mde_infectados_df$date <- dmy(mde_infectados_df$date)
#mde_infectados_df <- cbind(Row.Names = rownames(mde_infectados_df), mde_infectados_df)
mde_infectados_df<-mde_infectados_df %>% group_by(`date`) %>% summarise(totalDia=n())
mde_infectados_df$total <- cumsum(mde_infectados_df$totalDia)
totalMedellinInfectados <-  max(as.numeric(mde_infectados_df$total), na.rm=TRUE)

ti = 1:length(mde_infectados_df$totalDia)
m1 = lm(mde_infectados_df$totalDia~ti)
m2 = lm(mde_infectados_df$totalDia~ti+I(ti^2))
m3 = lm(mde_infectados_df$totalDia~ti+I(ti^2)+I(ti^3))
m4 = lm(mde_infectados_df$totalDia~exp(ti))
data.fmt = list(color=rgb(0.8,0.8,0.8,0.8), width=4)
line.fmt = list(dash="solid", width = 1.5, color=NULL)

## Acumulados MDE
acumuladosMde <- plot_ly(  x = mde_infectados_df$date, y = mde_infectados_df$total, type ='scatter', mode = 'lines', line = list(width = 10), name='Medellin' )%>%
    layout(yaxis = list(title = 'Acumulados Medellín COVID19'), plot_bgcolor ="#222", paper_bgcolor="#222", font = list(color ="#00bc8c"))%>%
    add_lines( y=predict(m1), line=line.fmt, name="Linear") %>%
    add_lines( y=predict(m2), line=line.fmt, name="Cuadratic") %>%
    add_lines( y=predict(m3), line=line.fmt, name="Cubic") %>%
    add_lines( y=predict(m4), line=line.fmt, name="Exponential")
    Graph.Acumulados.Mde <- ggplotly(acumuladosMde)

## Diagnosticados por dia MDE
porDiaMde <- plot_ly(  x = mde_infectados_df$date, y = mde_infectados_df$totalDia, type ='bar', color = I("plum4") )%>%
    layout(yaxis = list(title = 'Nuevos diagnosticos por día, Medellín COVID19'), plot_bgcolor ="#222", paper_bgcolor="#222", font = list(color ="#00bc8c"))
   Graph.Mde <- ggplotly(porDiaMde)



## Antioquia
ant_infectados_df <-  dplyr::filter(infectados_df, grepl("Antioquia",localization))
ant_infectados_df$date <- dmy(ant_infectados_df$date)

ant_infectados_df<-ant_infectados_df %>% group_by(`date`) %>% summarise(totalDia=n())

ant_infectados_df$total <- cumsum(ant_infectados_df$totalDia)
compare_data <- cbind(mde_infectados_df,ant_infectados_df)
colnames(compare_data) <- c("date", "mdeTotalDia","mdeAcumulado", "date2", "antTotalDia", "antAcumulado")
porDiaAnt <- plot_ly(  x = compare_data$date, y = compare_data$antAcumulado,  type ='scatter', mode = 'lines', line = list(width = 10), name= 'Antioquia',color = I("darkolivegreen") )%>%
    add_trace(y = compare_data$mdeAcumulado,  name = 'Medellin', color = I("plum4"))%>%
    layout(yaxis = list(title = 'Nuevos diagnosticos por día, Antioquia COVID19'), plot_bgcolor ="#222", paper_bgcolor="#222", font = list(color ="#00bc8c"))
Graph.Ant <- ggplotly(porDiaAnt)

## nuevos por día COL
infectados_df<-infectados_df %>% group_by(`date`) %>% summarise(totalDia=n())
infectados_df$date <- dmy(infectados_df$date)
porDiaCol <- plot_ly(  x = infectados_df$date, y = infectados_df$totalDia, type ='bar', color = I("slateGray") )%>%
    layout(yaxis = list(title = 'Nuevos diagnosticos por día, Colombia COVID19'), plot_bgcolor ="#222", paper_bgcolor="#222", font = list(color ="#00bc8c"))
   Graph.Col <- ggplotly(porDiaCol)

## acumulados  COL
infectados_df<-infectados_df[order(as.Date(infectados_df$date, format="%Y/%m/%d")),]
infectados_df$total <- cumsum(infectados_df$totalDia)
acumCol <- plot_ly(  x = infectados_df$date, y = infectados_df$total,  type ='scatter', mode = 'lines', line = list(width = 10), color = I("darkOliveGreen") )%>%
        layout(yaxis = list(title = 'Nuevos por día Colombia COVID19'),  plot_bgcolor ="#222", paper_bgcolor="#222", font = list(color ="#00bc8c"))
Graph.Acum.Col <- ggplotly(acumCol)


##})



