
# ayuda a suprimir warnings
shhh <- suppressPackageStartupMessages

# instala y carga librerias
load.lib <- c('tidyverse','data.table','zoo')
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)

load.mess <- shhh(sapply(load.lib, require, character = TRUE))

options(repr.matrix.max.cols=50, repr.matrix.max.rows=100)


# bajar datos de internet dia x dia

baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

minutesSinceLastUpdate = function(fileName) {
  (as.numeric(as.POSIXlt(Sys.time())) - as.numeric(file.info(fileName)$ctime)) / 60
}

loadData = function(fileName, columnName) {
  if(!file.exists(fileName) || minutesSinceLastUpdate(fileName) > 60) {
    data = read.csv(file.path(baseURL, fileName), check.names=FALSE, stringsAsFactors=FALSE) %>%
      select(-Lat, -Long) %>% 
      pivot_longer(-(1:2), names_to="date", values_to=columnName) %>% 
      mutate(
        date=as.Date(date, format="%m/%d/%y"),
        `Country/Region`=if_else(`Country/Region` == "", "?", `Country/Region`),
        `Province/State`=if_else(`Province/State` == "", "<all>", `Province/State`)
      )
    save(data, file=fileName)  
  } else {
    load(file=fileName)
  }
  return(data)
}

allData = 
  loadData(
    "time_series_covid19_confirmed_global.csv", "CumConfirmed") %>%
  inner_join(loadData(
    "time_series_covid19_deaths_global.csv", "CumDeaths")) %>%
  inner_join(loadData(
    "time_series_covid19_recovered_global.csv","CumRecovered"))


#In [7]:

# Inicio de la figura INFECTADOS POR DIA
startConfirmed1 <- 30

data3 <- subset(data0, NewConfirmedMA >= startConfirmed1)

# Enumeracion de datos
data3 <- data3 %>% group_by(Country) %>% mutate(Dias = row_number())

# define tamano de la figura
options(repr.plot.width = 8, repr.plot.height = 5)
ggplot(data3, aes(x = Dias)) +

#       geom_line(aes(y = NewConfirmed, colour = Country), linetype="dotted") + geom_point(aes(y = NewConfirmed, colour = Country)) +
       geom_line(aes(y = NewConfirmedMA, colour = Country), size = 1) + 
       labs(title = "Infectados por d�a (media m�vil de 7 d�as)", subtitle = "Desde que se report� 30 infecciones por d�a", x = "D�a", y = "Infectados por d�a") +
       scale_y_log10() +
       annotation_logticks(sides = "lr") + 
       labs(caption = paste(Sys.time())) + 
       theme_grey()



