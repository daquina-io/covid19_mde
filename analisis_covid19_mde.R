##rm(list=ls())

if(!require(plotly)){install.packages("plotly")}
if(!require(lubridate)){install.packages("lubridate")}
if(!require(htmlwidgets)){install.packages("htmlwidgets")}
if(!require(jsonlite)){install.packages("jsonlite")}
if(!require(purrr)){install.packages("purrr")}
if(!require(profvis)){install.packages("profvis")}
if(!require(DT)){install.packages("DT")}
if(!require(dplyr)){install.packages("dplyr")}

##profvis({
## test ===== viales
viales <- read.csv("~/DataSets/viales.csv") ## se descarga de https://www.medellin.gov.co/movilidad/cifras-estudios/viewcategory/3902-cifras-de-incidentalidad-diaria 
viales <- viales[-2,]
viales <- viales[1:(length(viales)-5) ]
colnames(viales) <- c("FechaIncidente", "FechaMuerte", "Direccion", "Condicion", "Sexo", "Edad", "Levantamiento","Comuna", "DiaIncidente", "DiaMuerte", "SemanaIncidente", "FechasIguales")
##
## Datos de instituto nacional de salud
##
##data <- read.csv("https://www.datos.gov.co/api/views/gt2j-8ykr/rows.csv", header = FALSE)
##
## Get the data with: curl https://www.datos.gov.co/api/views/gt2j-8ykr/rows.csv > Casos_positivos_de_COVID-19_en_Colombia.csv
data <- read.csv("Casos_positivos_de_COVID-19_en_Colombia.csv", header = FALSE)

#head(data, 5)
data <- data[-1,]
infectados_df <- data

colnames(infectados_df) <- c( "dateWeb","id","date", "codigoDIVIPOLA", "departamento", "codigoDivipolaMunicipio", "city", "age", "unidadEdad", "sex","type", "localization","condition", "codigoISOpais","nombrePais", "status",  "FIS", "deathDate", "diagnosisDate", "recoveredDate", "tipo_recuperacion",  "pertenencia_etnica", "nombre_grupo_etnico" )
infectados_df$date <- lubridate::dmy_hms(as.character(infectados_df$date))

unique(infectados_df$local)

infectados_df <- infectados_df %>% filter(date <= Sys.Date() - 14 ) ## filtra el futuro y descarta mediciones de los últimos 14 días (etapa de incubación)

## Dataset

ultimaFecha <- max(infectados_df$date, na.rm=TRUE)
## Colombia
totalColombiaInfectados <- max(as.numeric(infectados_df$id), na.rm=TRUE)
## Tipos de estado
tipos_estado <- c("Recuperado", "Fallecido", "Hospital", "Hospital UCI", "N/A", "Casa")

## Filtro de estados
filter_status <- function(df, status_fltr=tipos_estado, inverse_logic = FALSE) {
    if(inverse_logic) return(df %>% filter(!(status %in% status_fltr)))
    df %>% filter(status %in% status_fltr)
}


fallecidos <- filter_status(infectados_df, c("Fallecido")) %>% nrow()
recuperados <- filter_status(infectados_df, c("Recuperado")) %>% nrow()

## Tipos de ciudades
## Agrega status por ciudad y por fecha
summary_cities <- function(df=infectados_df, by_date=FALSE, date_range=c("2020-01-01","2030-01-01")){
    if(!by_date) return(
                     df %>% group_by(departamento, city, status) %>% summarise(cantidad=n())
                 )
    df %>% filter(date >= date_range[1], date <= date_range[2]) %>% group_by(departamento, city, date, status) %>% summarise(cantidad=n())
}

summary_cities(by_date = TRUE) %>% filter(city %in% c())

## Filtro de ciudades
## Se puede pasar un array de patrones regex para filtrar, o uno solo string con el patrón regex para filtrar
filter_city <- function(df, city_fltr=c("MEDEL","ENVIG")) {
    if(typeof(city_fltr)  == "character" & length(city_fltr) > 1) {
        d <- map(city_fltr, function(c) {
            df %>% filter(stringr::str_detect(df$city,c))
        }) %>% tibble::enframe(.) %>% tidyr::unnest(., cols = c(value))
        return(d[,-1])
    }
    df %>% filter(stringr::str_detect(df$city,city_fltr))
}

## Ejemplo de uso
## aca filtra los patrones por defecto c("[M|m]edel","Envi")
filter_city(infectados_df) %>% select(city) %>% unique
## Acá filtra el patrón regex solo para Medellín
infectados_df %>% filter_city("MEDELL") %>% select(city) %>% unique
##################
## Así agrego todos los datos para todas las ciudades y filtro solo las que deseo tener en cuenta
datos <- summary_cities(by_date = TRUE) %>% ungroup %>% filter_city(city_fltr=c("MEDELL","ENVI","ITAG","ESTRELL","BELLO"))

## Medellin
mde_infectados_df <- infectados_df %>% filter_city("MEDE")
bog_infectados_df <- infectados_df %>% filter_city("BOGO")
cal_infectaods_df <- infectados_df %>% filter_city("CALI")
cities_df <- rbind(mde_infectados_df,bog_infectados_df,cal_infectaods_df)
acumuladosCities <- plot_ly(  x = cities_df$date, y = mde_infectados_df$total, type ='scatter', mode = 'lines', line = list(width = 2), name='Medellin' )%>%
    layout(yaxis = list(title = 'Acumulados Medellín COVID19'), plot_bgcolor ="#222", paper_bgcolor="#222", font = list(color ="#00bc8c"))
Graph.Acumulados.Cities <- ggplotly(acumuladosCities)

#mde_infectados_df$date <- dmy(mde_infectados_df$date)
#mde_infectados_df <- cbind(Row.Names = rownames(mde_infectados_df), mde_infectados_df)
mde_infectados_df<-mde_infectados_df %>% group_by(`date`) %>% summarise(totalDia=n())
mde_infectados_df$total <- cumsum(mde_infectados_df$totalDia)

bog_infectados_df<-bog_infectados_df %>% group_by(`date`) %>% summarise(totalDia=n())
bog_infectados_df$total <- cumsum(bog_infectados_df$totalDia)

totalMedellinInfectados <-  max(as.numeric(mde_infectados_df$total), na.rm=TRUE)
totalBOGInfectados <-  max(as.numeric(bog_infectados_df$total), na.rm=TRUE)

fallecidosPor100milMDE_df <- infectados_df %>% filter_city("MEDELL") %>%  filter_status(  c("Fallecido")) %>% group_by(`date`) %>% summarise(totalDia=n())
fallecidosPor100milMDE_df$totalDia <- fallecidosPor100milMDE_df$totalDia / (2427129/100000)

fallecidosVial100milMDE_df <- viales %>%  group_by(`FechaMuerte`) %>% summarise(totalDia=n())
fallecidosVial100milMDE_df$totalDia <- fallecidosVial100milMDE_df$totalDia / (2427129/100000)


dfFallecidosPor100milBEL <- infectados_df %>% filter_city("BELL") %>%  filter_status(  c("Fallecido")) %>% group_by(`date`) %>% summarise(totalDia=n())
dfFallecidosPor100milBEL$totalDia <- dfFallecidosPor100milBEL$totalDia / (533973/100000)
dfFallecidosPor100milENV <- infectados_df %>% filter_city("ENVIG") %>%  filter_status(  c("Fallecido")) %>% group_by(`date`) %>% summarise(totalDia=n())
dfFallecidosPor100milENV$totalDia <- dfFallecidosPor100milENV$totalDia / (232854/100000)
dfFallecidosPor100milITA <- infectados_df %>% filter_city("ITAGU") %>%  filter_status(  c("Fallecido")) %>% group_by(`date`) %>% summarise(totalDia=n())
dfFallecidosPor100milITA$totalDia <- dfFallecidosPor100milITA$totalDia / (279894/100000)
dfFallecidosPor100milBOG <- infectados_df %>% filter_city("BOGOT") %>%  filter_status(  c("Fallecido")) %>% group_by(`date`) %>% summarise(totalDia=n())
dfFallecidosPor100milBOG$totalDia <- dfFallecidosPor100milBOG$totalDia / (7412566 / 100000)

## Otra manera de detectar los no Recuperados y no Fallecidos
activos_MDE <- infectados_df %>% filter_city("MEDEL") %>% filter_status(c("Recuperado","Fallecido"), inverse_logic = TRUE) %>% nrow()
## | Que se puede replicar acá

## V
##activos_MDE <-  dplyr::filter(infectados_df, grepl("MEDE",city)) %>% dplyr::filter(!grepl("Recuperado",status )) %>% dplyr::filter(!grepl("Fallecido",status )) %>% nrow()
activos_ENV <-  dplyr::filter(infectados_df, grepl("ENVI",city)) %>% dplyr::filter(!grepl("Recuperado",status )) %>% dplyr::filter(!grepl("Fallecido",status )) %>% nrow()
activos_ITA <-  dplyr::filter(infectados_df, grepl("ITAG",city)) %>% dplyr::filter(!grepl("Recuperado",status )) %>% dplyr::filter(!grepl("Fallecido",status )) %>% nrow()
activos_LAE <-  dplyr::filter(infectados_df, grepl("ESTREL",city)) %>% dplyr::filter(!grepl("Recuperado",status )) %>% dplyr::filter(!grepl("Fallecido",status )) %>% nrow()
activos_BEL <-  dplyr::filter(infectados_df, grepl("BELL",city)) %>% dplyr::filter(!grepl("Recuperado",status )) %>% dplyr::filter(!grepl("Fallecido",status )) %>% nrow()
en_casa_MDE <-   dplyr::filter(infectados_df, grepl("MEDELL",city)) %>% dplyr::filter(grepl("Casa",status ))  %>% nrow()
recuperados_mde <-infectados_df %>% dplyr::filter(grepl("MEDELL",city ))  %>%  dplyr::filter(grepl("Recuperado",status ))
recuperados_mde <- nrow(na.omit(recuperados_mde))
fallecidos_mde <-infectados_df %>% dplyr::filter(grepl("MEDELL",city ))  %>%  dplyr::filter(grepl("Fallecido",status ))
fallecidos_mde <- nrow(na.omit(fallecidos_mde))
fallecidos_por_millon_mde <- format(round(
    (fallecidos_mde/2529403)*1000000,  2), scientific = FALSE)
uciMDE <-  infectados_df %>% dplyr::filter(grepl("MEDELL",city ))  %>%  dplyr::filter(grepl("UCI",localization ))  ## asumiendo 264
uciMDE <- nrow(na.omit(uciMDE))
portionCalc <- uciMDE 
portion <- paste0(round(portionCalc,1), "%")
relacion_fallecido_diagnoticado_MDE <-  paste0(round(fallecidos_mde/totalMedellinInfectados*100, 1), "%" )
relacion_recuperados_MDE_en_Diagnosticados_MDE <- paste0(round(recuperados_mde/totalMedellinInfectados*100, 1), "%")
relacion_diagnosticados_MDE_en_COL <- paste0(round(totalMedellinInfectados/totalColombiaInfectados*100, 1), "%")

## Data
## owiData <- read.csv("https://covid.ourworldindata.org/data/ecdc/total_deaths.csv")
## head(owiData,5)
## owiData$Colombia

## Data MDE
#mdeData <- read.csv("https://www.datos.gov.co/api/views/imj6-7tfq/rows.csv")
#head(mdeData,30)

## Dataset Defunciones MDE COLUMNA, REPETIDA NO CARGA
##defunMDE <- read.csv("http://medata.gov.co/sites/default/files/medata_harvest_files/defunciones.csv")
#head(defunMDE,10)

## === TABLE
tableData <- c(fallecidos,
               totalColombiaInfectados,
               totalMedellinInfectados,
               activos_MDE,
               activos_ENV,
               activos_ITA,
               activos_LAE,
               activos_BEL,
               en_casa_MDE,
               uciMDE,
               fallecidos_mde,
               fallecidos_por_millon_mde,
               #portion,
               relacion_fallecido_diagnoticado_MDE,
               relacion_diagnosticados_MDE_en_COL )
dfTableData <- data.frame(tableData)
rownames(dfTableData) <- c("Fallecidos COL",
                           "Diagnosticados COL",
                           "Diagnosticados MDE",
                           "Activos MDE",
                           "Activos ENV",
                           "Activos ITA",
                           "Activos LAE",
                           "Activos BEL",
                           "En Casa MDE",
                           "En UCI MDE (Según CRUE son 969)",
                           "Fallecidos MDE",
                           "Fallecidos por Millon MDE",
                           #"Ocupacion de UCI's en MDE por COVID19 ",
                           "Relación:  Fallecidos MDE / Diagnosticados MDE",
                           "Relación:   Diagnosticados MDE / Diagnosticados COL" )
table <- datatable(dfTableData,
                   style = "bootstrap",
                   options = list(
                       dom = 't',
                       columnDefs = list(list(className = 'dt-right', targets = 1),list(className = 'dt-center', targets = 0)),
                       #columnDefs = list(list(width='40px',targets= "_all")),
                       pageLength = 15
                       ))

## -- GRAPHS
fun_color_range <- colorRampPalette(c("yellow", "green"))   # Apply colorRampPalette
my_colors <- fun_color_range(10)                          # Extract 100 color codes
line.fmt = list(dash="solid", width = 0.5, color=NULL)

### ------ FALLECIDOS POR 100mil MDE  ----
fallecidos100milMDE.fig <- plot_ly(  x = fallecidosPor100milMDE_df$date, y = fallecidosPor100milMDE_df$totalDia, type ='scatter', mode = 'lines', line = list(width = 2), name='Medellin', width = 4.0, color = "#e08e0b" )%>%
    layout(yaxis = list(title = 'Fallecidos por 100mil habitantes en MDE'), plot_bgcolor ="#222", paper_bgcolor="#222", font = list(color ="#00bc8c"))
Graph.FallecidosPor100milMDE <- ggplotly(fallecidos100milMDE.fig)

Graph.FallecidosPor100milMDE <- Graph.FallecidosPor100milMDE %>%
  add_lines( x=fallecidosPor100milMDE_df$date, y=fallecidosPor100milMDE_df$totalDia, line=line.fmt, name="Smooth", color = c(my_colors[3]), geom_smooth(span = 0.5))

Graph.FallecidosPor100milMDE <- Graph.FallecidosPor100milMDE %>%
  add_lines( x=fallecidosPor100milMDE_df[331:361, ]$date, y=fallecidosPor100milMDE_df[331:361, ]$totalDia, line = list(color = "green" , width = 3), name="Marchas")

##Graph.FallecidosPor100milMDE <- Graph.FallecidosPor100milMDE %>% add_lines( x=dfFallecidosPor100milENV$date, y=dfFallecidosPor100milENV$totalDia, line=line.fmt, name="Envigado", color = c(my_colors[3]))
##Graph.FallecidosPor100milMDE <- Graph.FallecidosPor100milMDE %>% add_lines( x=dfFallecidosPor100milITA$date, y=dfFallecidosPor100milITA$totalDia, line=line.fmt, name="Itagüí", color = c(my_colors[4]))
##Graph.FallecidosPor100milMDE <- Graph.FallecidosPor100milMDE %>% add_lines( x=dfFallecidosPor100milBOG$date, y=dfFallecidosPor100milBOG$totalDia, line=line.fmt, name="Bogotá", color = c(my_colors[5]))


### ------ FALLECIDOS POR 100mil comparativo municipios ----
fallecidos100milMDE_muni.fig <- plot_ly(  x = fallecidosPor100milMDE_df$date, y = fallecidosPor100milMDE_df$totalDia, type ='scatter', mode = 'lines', line = list(width = 2), name='Medellin', width = 4.0, color = "#e08e0b" )%>%
    layout(yaxis = list(title = 'Fallecidos por 100mil habitantes en Municipios por COVID19'), plot_bgcolor ="#222", paper_bgcolor="#222", font = list(color ="#00bc8c"))
Graph.FallecidosPor100milMDE_muni <- ggplotly(fallecidos100milMDE_muni.fig)
Graph.FallecidosPor100milMDE_muni <- Graph.FallecidosPor100milMDE_muni %>% add_lines( x=dfFallecidosPor100milBEL$date, y=dfFallecidosPor100milBEL$totalDia, line=line.fmt, name="Bello", color = c(my_colors[2]))
Graph.FallecidosPor100milMDE_muni <- Graph.FallecidosPor100milMDE_muni %>% add_lines( x=dfFallecidosPor100milENV$date, y=dfFallecidosPor100milENV$totalDia, line=line.fmt, name="Envigado", color = c(my_colors[3]))
Graph.FallecidosPor100milMDE_muni <- Graph.FallecidosPor100milMDE_muni %>% add_lines( x=dfFallecidosPor100milITA$date, y=dfFallecidosPor100milITA$totalDia, line=line.fmt, name="Itagüí", color = c(my_colors[4]))
Graph.FallecidosPor100milMDE_muni <- Graph.FallecidosPor100milMDE_muni %>% add_lines( x=dfFallecidosPor100milBOG$date, y=dfFallecidosPor100milBOG$totalDia, line=line.fmt, name="Bogotá", color = c(my_colors[5]))


## otras causas
fallecidos100milMDE.fig.otras <- plot_ly(  x = fallecidosPor100milMDE_df$date, y = fallecidosPor100milMDE_df$totalDia, type ='scatter', mode = 'lines', line = list(width = 1), name='Covid19', width = 1.0, color = "#e08e0b" )%>%
    layout(yaxis = list(title = 'Fallecidos por 100mil habitantes en Medellin'), plot_bgcolor ="#222", paper_bgcolor="#222", font = list(color ="#00bc8c"))
Graph.FallecidosPor100milMDE.otras <- ggplotly(fallecidos100milMDE.fig.otras)
Graph.FallecidosPor100milMDE.otras <- Graph.FallecidosPor100milMDE.otras %>% add_lines( x=fallecidosVial100milMDE_df$FechaMuerte, y=fallecidosVial100milMDE_df$totalDia, line=line.fmt, name="Viales", width = 4.0, color = c(my_colors[2]))


## ========================================== ACUMULADOS MDE ------
# ti = 1:length(mde_infectados_df$totalDia)
# m1 = lm(mde_infectados_df$totalDia~ti)
# m2 = lm(mde_infectados_df$totalDia~ti+I(ti^2))
# m3 = lm(mde_infectados_df$totalDia~ti+I(ti^2)+I(ti^3))
# m4 = lm(mde_infectados_df$totalDia~exp(ti))
# data.fmt = list(color=rgb(0.8,0.8,0.8,0.8), width=4)
line.fmt = list(dash="solid", width = 1.5, color=NULL)

# Ajuste de curva exponencial, adaptado de https://rpubs.com/mengxu/exponential-model

# La ecuacion de ajuste es E(y) = alpha * exp(beta * x) + theta.

# Extraer una porcion de la tibble, de 2020-04-27 hasta 2020-05-28.
mde_infectados_exp_df <- mde_infectados_df[46:77,]

# Representacion del tiempo en enteros. Conteo de dias desde 2020-04-27.
ti <- 1:length(mde_infectados_exp_df$total)

# Seleccionar un valor inicial de la asintota $\theta$.
# theta debe ser menor que min(y), y mayor que cero.
theta.0 <- min(mde_infectados_exp_df$total) * 0.5

# Estimar el resto de los parametros iniciales usando un modelo lineal.
exp_model.0 <- lm(log(total - theta.0) ~ ti, data=mde_infectados_exp_df)
alpha.0 <- exp(coef(exp_model.0)[1])
beta.0 <- coef(exp_model.0)[2]

# Agrupar parametros iniciales
start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)

# Realizar el ajuste usando un modelo no lineal.
exp_model <- nls(total ~ alpha * exp(beta * ti) + theta , data = mde_infectados_exp_df, start = start)

# Calcular valores de la regresion (eje y).
exp_reg <- predict(exp_model,list(Time=mde_infectados_exp_df$date))

## Acumulados MDE
## - Annotations relative Y value (cambio de rango inversamente proporcional)  TODO verificar cuando valor minimo sea 1 o mas
changeRange <- function(oldValue, oldMin, oldMax, newMin, newMax){
    oldRange <- (oldMax - oldMin)
    newRange <- (newMax - newMin)
    newValue <-(((oldValue - oldMin) / newRange) * oldRange) + newMin
    return(newValue)
}

maxAcum <- 318000 ## TODO: grab from data
annY1 <-  changeRange(210/maxAcum+0.08, 0, maxAcum, 0, totalMedellinInfectados)
annY2 <- changeRange(1647/maxAcum+0.08, 0, maxAcum, 0, totalMedellinInfectados)
annY3 <- changeRange(146789/maxAcum+0.08, 0, maxAcum, 0, totalMedellinInfectados)
annY4 <-  changeRange(154153/maxAcum+0.08, 0, maxAcum, 0, totalMedellinInfectados)
annotation1 <- list(yref = 'paper', xref = "x", y = annY1, x = as.Date("2020-03-24")+14, text = "Inicia Cuarentena Colombia")
annotation2 <- list(yref = 'paper', xref = "x", y = annY2, x = as.Date("2020-06-19")+14, text = "Dia sin IVA")
annotation3 <- list(yref = 'paper', xref = "x", y = annY3, x = as.Date("2020-12-24")+14, text = "Navidad")
annotation4 <- list(yref = 'paper', xref = "x", y = annY4, x = as.Date("2020-12-31")+14, text = "Año nuevo")
acumuladosMde <- plot_ly(  x = mde_infectados_df$date, y = mde_infectados_df$total, type ='scatter', mode = 'lines', line = list(width = 2), name='Medellin' )%>%
    layout(yaxis = list(title = 'Acumulados Medellín COVID19'), plot_bgcolor ="#222", paper_bgcolor="#222", font = list(color ="#00bc8c"))%>%
    layout(annotations= list(annotation1, annotation2, annotation3, annotation4))
    Graph.Acumulados.Mde <- ggplotly(acumuladosMde)
    ## add_lines( y=predict(m1), line=line.fmt, name="Linear") %>%
    ## add_lines( y=predict(m2), line=line.fmt, name="Cuadratic") %>%
    ## add_lines( y=predict(m3), line=line.fmt, name="Cubic") %>%
    ## add_lines( y=predict(m4), line=line.fmt, name="Exponential")
Graph.Acumulados.Mde <- Graph.Acumulados.Mde %>% add_lines( x=mde_infectados_exp_df$date, y=exp_reg, line=line.fmt, name="Exponencial")
Graph.Acumulados.Mde <- Graph.Acumulados.Mde %>% add_lines( x=mde_infectados_df[412:442, ]$date, y=mde_infectados_df[412:442, ]$total, line=line.fmt, name="Marchas")

## ======================================= ACUMULADOS MDE MARCHAS =====
annotationOffset <- 0.14
maxAcum <- 323000 ## TODO: grab from data
annY1 <-  270902/maxAcum - annotationOffset
annotation1 <- list(yref = 'paper', xref = "x", y = annY1, x = as.Date("2021-04-28"), text = "Inician Marchas")
acumuladosMdeMarchas <- plot_ly(  x = mde_infectados_df[280:length(mde_infectados_df$date), ]$date, y = mde_infectados_df[280:length(mde_infectados_df$date), ]$total, type ='scatter', mode = 'lines', line=line.fmt, name='Medellin' )%>%
    layout(yaxis = list(title = 'Acumulados Medellín COVID19'), plot_bgcolor ="#222", paper_bgcolor="#222", font = list(color ="#00bc8c"))%>%
  layout(annotations= list(annotation1))
Graph.Acumulados.Mde.Marchas <- ggplotly(acumuladosMdeMarchas)
Graph.Acumulados.Mde.Marchas <- Graph.Acumulados.Mde.Marchas %>% add_lines( x=mde_infectados_df[412:442, ]$date, y=mde_infectados_df[412:442, ]$total, line = list(color = "green" , width = 3),  name="Marchas" )
Graph.Acumulados.Mde.Marchas

## ======================================= ACUMULADOS BOG MARCHAS =====
maxAcum <- 1096153 ## TODO: grab from data
annY1 <-  801171/maxAcum-0.15
annotation1 <- list(yref = 'paper', xref = "x", y = annY1, x = as.Date("2021-04-25"), text = "Inician Marchas")
acumuladosBOGMarchas <- plot_ly(  x = bog_infectados_df[280:length(bog_infectados_df$date), ]$date, y = bog_infectados_df[280:length(bog_infectados_df$date), ]$total, type ='scatter', mode = 'lines', line = list(width = 2), name='Bogota' )%>%
    layout(yaxis = list(title = 'Acumulados Bogotá COVID19'), plot_bgcolor ="#222", paper_bgcolor="#222", font = list(color ="#00bc8a"))%>%
    layout(annotations= list(annotation1))
Graph.Acumulados.BOG.Marchas <- ggplotly(acumuladosBOGMarchas)
Graph.Acumulados.BOG.Marchas <- Graph.Acumulados.BOG.Marchas %>% add_lines( x=bog_infectados_df[417:442, ]$date, y=bog_infectados_df[417:442, ]$total, line = list(color = "green" , width = 3), name="Marchas")
Graph.Acumulados.BOG.Marchas



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

