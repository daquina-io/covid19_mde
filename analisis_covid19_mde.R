rm(list=ls())

if(!require(plotly)){install.packages("plotly")}
if(!require(lubridate)){install.packages("lubridate")}
if(!require(htmlwidgets)){install.packages("htmlwidgets")}
if(!require(jsonlite)){install.packages("lsonlite")}
if(!require(purrr)){install.packages("purrr")}
if(!require(profvis)){install.packages("profvis")}
if(!require(DT)){install.packages("DT")}
##profvis({
##profvis({

##
## Datos de instituto nacional de salud
##
data <- read.csv("https://www.datos.gov.co/api/views/gt2j-8ykr/rows.csv", header = FALSE)
head(data,200)
data <- data[-1,]
infectados_df <- data
colnames(infectados_df) <- c("id", "date", "codigoDIVIPOLA", "city", "localization","status", "age", "sex", "type", "condition", "origin", "FIS", "deathDate", "diagnosisDate", "recoveredDate", "webDate" )
infectados_df$date <- lubridate::ymd_hms(as.character(infectados_df$date))
head(infectados_df$date, 5)
## Dataset
ultimaFecha <- max(infectados_df$date, na.rm=TRUE)
## Colombia
totalColombiaInfectados <- max(as.numeric(infectados_df$id), na.rm=TRUE)
fallecidos <- infectados_df %>% dplyr::filter(grepl("Fallecido",status)) %>% nrow()

recuperados <- infectados_df %>% dplyr::filter(grepl("Recuperado",status)) %>% nrow()
## Medellin
mde_infectados_df <-  dplyr::filter(infectados_df, grepl("Medel",city))
#mde_infectados_df$date <- dmy(mde_infectados_df$date)
#mde_infectados_df <- cbind(Row.Names = rownames(mde_infectados_df), mde_infectados_df)
mde_infectados_df<-mde_infectados_df %>% group_by(`date`) %>% summarise(totalDia=n())
mde_infectados_df$total <- cumsum(mde_infectados_df$totalDia)
totalMedellinInfectados <-  max(as.numeric(mde_infectados_df$total), na.rm=TRUE)
recuperados_mde <-infectados_df %>% dplyr::filter(grepl("Medel",city ))  %>%  dplyr::filter(grepl("Recuperado",status ))
recuperados_mde <- nrow(na.omit(recuperados_mde))
fallecidos_mde <-infectados_df %>% dplyr::filter(grepl("Medel",city ))  %>%  dplyr::filter(grepl("Fallecido",status ))
fallecidos_mde <- nrow(na.omit(fallecidos_mde))
uciMDE <-  infectados_df %>% dplyr::filter(grepl("Medel",city ))  %>%  dplyr::filter(grepl("UCI",status ))  ## hay 400
uciMDE <- nrow(na.omit(uciMDE))
portionCalc <- (uciMDE * 100)/400
portion <- paste0(portionCalc,"%")
relacion_fallecido_diagnoticado_MDE <-  paste0(round(fallecidos_mde/totalMedellinInfectados*100, 1)/100, "%" )
relacion_recuperados_MDE_en_Diagnosticados_MDE <- paste0(round(recuperados_mde/totalMedellinInfectados*100, 1), "%")
relacion_diagnosticados_MDE_en_COL <- paste0(round(totalMedellinInfectados/totalColombiaInfectados*100, 1), "%")

## Data
owiData <- read.csv("https://covid.ourworldindata.org/data/ecdc/total_deaths.csv")
head(owiData,5)
owiData$Colombia

## Data MDE
mdeData <- read.csv("https://www.datos.gov.co/api/views/imj6-7tfq/rows.csv")
head(mdeData,30)

## === TABLE
tableData <- c(fallecidos, totalColombiaInfectados, fallecidos_mde, totalMedellinInfectados, uciMDE, portion,  relacion_fallecido_diagnoticado_MDE, relacion_recuperados_MDE_en_Diagnosticados_MDE, relacion_diagnosticados_MDE_en_COL )
dfTableData <- data.frame(tableData)
rownames(dfTableData) <- c("Fallecidos COL",
                           "Diagnosticados COL",
                           "Fallecidos MDE",
                           "Diagnosticados MDE",
                           "En UCI MDE",
                           "Ocupacion de UCI's en MDE por COVID19 (Asumiendo 400)",
                           "Relación:  Fallecidos MDE / Diagnosticados MDE",
                           "Relación:  Recuperados MDE / Diagnosticados MDE ",
                           "Relación:   Diagnosticados MDE / Diagnosticados COL" )
table <- datatable(dfTableData,
                   style = "bootstrap",
                   options = list(
                       dom = 't',
                       columnDefs = list(list(className = 'dt-center', targets = 0)),
                       columnDefs = list(list(width='60px',targets= "_all"))
                       ))

## -- GRAPHS ?
ti = 1:length(mde_infectados_df$totalDia)
m1 = lm(mde_infectados_df$totalDia~ti)
m2 = lm(mde_infectados_df$totalDia~ti+I(ti^2))
m3 = lm(mde_infectados_df$totalDia~ti+I(ti^2)+I(ti^3))
m4 = lm(mde_infectados_df$totalDia~exp(ti))
data.fmt = list(color=rgb(0.8,0.8,0.8,0.8), width=4)
line.fmt = list(dash="solid", width = 1.5, color=NULL)

## Acumulados MDE
annotation1 <- list(yref = 'paper', xref = "x", y = 0.2, x = as.Date("2020-03-24"), text = "Inicia Cuarentena Colombia")
annotation2 <- list(yref = 'paper', xref = "x", y = 0.6, x = as.Date("2020-05-08"), text = "Dia de la Madre")
acumuladosMde <- plot_ly(  x = mde_infectados_df$date, y = mde_infectados_df$total, type ='scatter', mode = 'lines', line = list(width = 10), name='Medellin' )%>%
    layout(yaxis = list(title = 'Acumulados Medellín COVID19'), plot_bgcolor ="#222", paper_bgcolor="#222", font = list(color ="#00bc8c"))%>%
    layout(annotations= list(annotation1, annotation2))
    Graph.Acumulados.Mde <- ggplotly(acumuladosMde)
    ## add_lines( y=predict(m1), line=line.fmt, name="Linear") %>%
    ## add_lines( y=predict(m2), line=line.fmt, name="Cuadratic") %>%
    ## add_lines( y=predict(m3), line=line.fmt, name="Cubic") %>%
    ## add_lines( y=predict(m4), line=line.fmt, name="Exponential")

## Diagnosticados por dia MDE
porDiaMde <- plot_ly(  x = mde_infectados_df$date, y = mde_infectados_df$totalDia, type ='bar', color = I("plum4") )%>%
    layout(yaxis = list(title = 'Nuevos diagnosticos por día, Medellín COVID19'), plot_bgcolor ="#222", paper_bgcolor="#222", font = list(color ="#00bc8c"))
   Graph.Mde <- ggplotly(porDiaMde)



## ## Antioquia
## ant_infectados_df <-  dplyr::filter(infectados_df, grepl("Antio",localization))
## #ant_infectados_df$date <- dmy(ant_infectados_df$date)
## ant_infectados_df<-ant_infectados_df %>% group_by(`date`) %>% summarise(totalDia=n())
## ant_infectados_df$total <- cumsum(ant_infectados_df$totalDia)
## compare_data <- cbind(mde_infectados_df,ant_infectados_df)
## colnames(compare_data) <- c("date", "mdeTotalDia","mdeAcumulado", "date2", "antTotalDia", "antAcumulado")
## porDiaAnt <- plot_ly(  x = compare_data$date, y = compare_data$antAcumulado,  type ='scatter', mode = 'lines', line = list(width = 10), name= 'Antioquia',color = I("darkolivegreen") )%>%
##     add_trace(y = compare_data$mdeAcumulado,  name = 'Medellin', color = I("plum4"))%>%
##     layout(yaxis = list(title = 'Nuevos diagnosticos por día, Antioquia COVID19'), plot_bgcolor ="#222", paper_bgcolor="#222", font = list(color ="#00bc8c"))
## Graph.Ant <- ggplotly(porDiaAnt)

## nuevos por día COL
infectados_df<-infectados_df %>% group_by(`date`) %>% summarise(totalDia=n())
#infectados_df$date <- dmy(infectados_df$date)
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


## ======== OLD API BROKEN
##data <- fromJSON("https://infogram.com/api/live/flex/bc384047-e71c-47d9-b606-1eb6a29962e3/664bc407-2569-4ab8-b7fb-9deb668ddb7a")
##ninfectados <- dim(data$data)[2]

##infectados_df <- map(1:ninfectados, function(x) {
##    data$data[1,x,]
##}) %>% unlist(.) %>% matrix(.,nrow=ninfectados, byrow = TRUE) %>% data.frame(., stringsAsFactors = FALSE)

##colnames(infectados_df) <- infectados_df[1,]
##infectados_df <- infectados_df[-1,]

### OLD CODE
## data <- fromJSON("https://www.datos.gov.co/resource/gt2j-8ykr.json")
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

