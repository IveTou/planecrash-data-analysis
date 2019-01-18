#Plaincrash.R

planecrash = read.csv("planecrashinfo_20181121001952.csv")
attach(planecrash)

#PARTE 1 - PREPROCESSAMENTO

#Remover as variáveis 'operator', flight_no', 'ac_type', 'registration', 'cn_ln' e 'summary'
planecrash = planecrash[,-c(4,5,7:9,13)]

#Data
## Configurações de 'localidade' do sistema
Sys.getlocale("LC_TIME");
#[1] "pt_BR.UTF-8"
Sys.setlocale("LC_TIME", "C")

##Formatar
new_dates = as.Date(date, "%B %d, %Y")

#Converter data para mês ('Jan','Feb', 'Mar',..)
library(lubridate)
new_month = month(new_dates, label=T, abbr=T)

#Converter data para dia da semana ('Sun','Mon', 'Tue',..)
new_wday = weekdays(new_dates, abbreviate=T)

#Converter data para ano
new_year = as.factor(year(new_dates))


##Verificar valores não convertidos
table(is.na(new_dates))

#Time
##Assumindo que todos os valores constantes foram preenchidos levando-se em consideração o formato de 24h, formatamos todas
##as entradas no formato HH:MM

library(stringr)
new_time = sub("(^\\d{1,2}$)", "\\1:00", sub("(\\d{2})(\\d{2})", "\\1:\\2", str_extract(time,'\\d{1,2}(:|)\\d{1,2}')))

#Location
#Aqui utilizamos o pacote ggmap que utiliza a API Google Maps (*google restricts requests to 2500 requests a day for non-business use.)

library(ggmap)
location = sub("(\\'s)","",location) #A API não reconhece cadeias com o caractere "'"

new_location_1 = geocode(as.character(location[1:2400]), output = "latlona", source = "dsk")
#save(new_location_1, file="new_location_1")

new_location_2 = geocode(as.character(location[2401:4500]), output = "latlona", source = "dsk")
#save(new_location_2, file="new_location_2")

new_location_3 = geocode(as.character(location[4501:5783]), output = "latlona", source = "dsk")
#save(new_location_3, file="new_location_3")

new_location = rbind(new_location_1, new_location_2, new_location_3)

#Para alguns locais de acidentes registrados a API Google Maps não foi capaz de identificar dados de geolocalização
#Alguns deles em razão de que os nomes constante não mais existem, como países pertencentes à extinta USSR, os quais 
#já mudaram de nome; outros os dados de endereço simplesmente estão ausentes

nas = row.names(new_location[which(is.na(new_location$lon)),])
location[as.numeric(nas)]

#Aboard
#Aqui derivamos o atributo 'aboard' nos atributos 'aboard_crew' e 'aboard_passengers'
aboard_passengers = NULL
aboard_crew = NULL
for(i in 1:length(aboard)) {
  r = as.numeric(sub(":", "", str_extract_all(aboard[i],'^*:(\\d)*\\:*')[[1]]))
  aboard_passengers = rbind(aboard_passengers, r[1])
  aboard_crew = rbind(aboard_crew, r[2])
}

#Fatalities
#Aqui derivamos o atributo 'fatalities' nos atributos 'fatalities_crew' e 'fatalities_passengers'
fatalities_passengers = NULL
fatalities_crew = NULL
for(i in 1:length(fatalities)) {
  r = as.numeric(sub(":", "", str_extract_all(fatalities[i],'^*:(\\d)*\\:*')[[1]]))
  fatalities_passengers = rbind(fatalities_passengers, r[1])
  fatalities_crew = rbind(fatalities_crew, r[2])
}

#Ground
#convertendo a variavel do tipo factor para numeric
new_ground = as.numeric(as.character(ground))

#Classificar acidentes como "na decolagem", " desvio/trajeto", "pouso": neste estudo não utilizaremos os dados
#referentes às rotas, contudo deixarei constante o preprocessamento requerido, somente à título de curiosidade
new_route = droplevels(planecrash$route)

##Transdormando endereços contendo as subseqências "?", "show" and "test", etc. para valores NA
new_route[
  new_route == "" |
  grepl("\\?", tolower(new_route)) | 
  grepl(tolower(" show"), tolower(new_route)) |
  grepl(tolower("test"), tolower(new_route)) |
  grepl(tolower("testing"), tolower(new_route)) |
  grepl(tolower("Demonstration"), tolower(new_route)) |
  grepl(tolower("Demonistration"), tolower(new_route)) |
  grepl(tolower("Sightseeing"), tolower(new_route)) |
  grepl(tolower("Instructional"), tolower(new_route)) |
  grepl(tolower("Training"), tolower(new_route))
] = NA
new_route = as.character(new_route)

##Formatar casos específicos para o formato "[ORIGEM] - [DESTINO]"
new_route[27] = gsub("  ", " - ", as.character(new_route[27]))
new_route[561] = gsub("to", " - ", as.character(new_route[561]))
new_route[562] = gsub("\\,", " - ", as.character(new_route[562]))
new_route[562] = gsub("\\.", "", as.character(new_route[562]))
new_route[927] = NA

nroute = NULL
for(i in 1:length(new_route)) {
  row = strsplit(new_route[i], split = "-", fixed = T)[[1]]
   if(length(row) > 1) {
    row = cbind(row[1], row[length(row)])
   } else {
     row = cbind(NA, NA)
   }
  nroute = rbind(nroute, row)
}

new_route = as.data.frame(nroute)
names(new_route) = c("route_start","route_end")
rm(nroute)

##Obtendo coordenadas da origem e do destino

##Origem
new_route$route_start = sub("(\\'s)","",new_route$route_start) #A API não reconhece cadeias com o caractere "'"
route_start_coordinates_1_2400 = geocode(as.character(new_route$route_start[!is.na(new_route$route_start)][1:2400]), output = "latlona", source = "dsk")
new_route$route_start[!is.na(new_route$route_start)][3921] = NA
route_start_coordinates_2401_4025 = geocode(as.character(new_route$route_start[!is.na(new_route$route_start)][2401:4025]), output = "latlona", source = "dsk")

new_route_start = as.data.frame(matrix(data = NA, nrow = nrow(new_route), ncol = 3))
new_route_start[!is.na(new_route$route_start), ] = as.matrix(rbind(route_start_coordinates_1_2400, route_start_coordinates_2401_4025))
new_route_start[,1] = as.numeric(new_route_start[,1])
new_route_start[,2] = as.numeric(new_route_start[,2])
new_route_start[,3] = as.factor(new_route_start[,3])
#save(new_route_start, file="new_route_start")

#Destino
new_route$route_end = sub("(\\'s)","",new_route$route_end) #A API não reconhece cadeias com o caractere "'"
route_end_coordinates_1_2400 = geocode(as.character(new_route$route_end[!is.na(new_route$route_end)][1:2400]), output = "latlona", source = "dsk")
route_end_coordinates_2401_4026 = geocode(as.character(new_route$route_end[!is.na(new_route$route_end)][2401:4026]), output = "latlona", source = "dsk")

new_route_end = as.data.frame(matrix(data = NA, nrow = nrow(new_route), ncol = 3))
new_route_end[!is.na(new_route$route_end),] = as.matrix(rbind(route_end_coordinates_1_2400, route_end_coordinates_2401_4026))
new_route_end[,1] = as.numeric(new_route_end[,1])
new_route_end[,2] = as.numeric(new_route_end[,2])
new_route_end[,3] = as.factor(new_route_end[,3])
#save(new_route_end, file="new_route_end")

#AGREGAÇÃO DOS DADOS TRANSFORMADOS E DERIVADOS
new_planecrash = cbind(new_dates, new_month, new_wday, new_year, new_time, new_location, new_route_start, new_route_end, aboard_crew, aboard_passengers, fatalities_crew, fatalities_passengers, new_ground)
names(new_planecrash) = c('date', 'month', 'weekday', 'year','time', 'crash_lon', 'crash_lat', 'crash_address', 'route_start_lon', 'route_start_lat', 'route_start_address','route_end_lon', 'route_end_lat', 'route_end_address','aboard_crew','aboard_passengers','fatalities_crew','fatalities_passengers','ground')
new_planecrash = droplevels(new_planecrash)

save(new_planecrash, file='new_planecrash')

#AMOSTRAGEM para o período de novembro de 1998 à novembro de 2018

attach(new_planecrash)
planecrash_last20 = new_planecrash[which(date > '1998-11-01'),][,-c(9:14)]
planecrash_last20 = planecrash_last20[complete.cases(planecrash_last20[,c(9:13)]), ]

#------------------------------------------------------------------------------------------

#PARTE 2 - ANÁLISE DE DADOS

##Caracterização de Acidentes

## Amostragem
planecrash_last20_s = planecrash_last20[,c(9:13)]
nrow(planecrash_last20_s)
#[1] 938

#bp_abroad
boxplot(planecrash_last20_s[,c('aboard_crew','aboard_passengers')] + 1, las=1, log="y")
#bp_fatalities
boxplot(planecrash_last20_s[,c('fatalities_crew','fatalities_passengers')] + 1, las=1, log="y")
#bp_fatalities_ground
boxplot(planecrash_last20_s[,c('ground')] + 1, las=1, log="y")

deaths = planecrash_last20_s[,c('aboard_crew','aboard_passengers','fatalities_crew','fatalities_passengers', 'ground')]
deaths[is.na(deaths)]= 0
deaths_l = apply(deaths, 1, sum)

#bp_fatalities_overall
boxplot(deaths_l + 1, log="y")

#death_summary
summary(deaths_l)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2.00    8.00   17.00   55.61   41.00 2934.00 

#Desvio padrão
sd(deaths_l)
#[1] 156.3742

#Moda
Mode = function(x) {
  ux = unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(deaths_l[deaths_l != 0])

#Removendo a variável os outliers 

#Corner cases
most = planecrash_last20[which(planecrash_last20$ground + planecrash_last20$fatalities_crew + planecrash_last20$fatalities_passengers > 200),]
most = cbind(most, apply(most[,c(9:13)], 1, sum))
names(most)[14] = "deaths"

#11 maiores "corner cases", ordenados por quntidade de vítimas fatais
most[order(-most$death), -c(2:7)]

#          date                                        crash_address aboard_crew aboard_passengers fatalities_crew fatalities_passengers ground deaths
#5018 2001-09-11                              New York City, New York          11                81              11                    81   2750   2934
#5017 2001-09-11                              New York City, New York           9                56               9                    56   2750   2880
#5690 2014-07-17                                     Hrabove, Ukraine          15               283              15                   283      0    596
#5131 2003-02-19                                   Near Shahdad, Iran          18               257              18                   257      0    550
#5033 2001-11-12                       Belle Harbor, Queens, New York           9               251               9                   251      5    525
#5774 2018-04-11                                 Boufarik AB, Algeria          10               247              10                   247      0    514
#5679 2014-03-08                                   South Indian Ocean          12               227              12                   227      0    478
#5489 2009-06-01 Atlantic Ocean, 570 miles northeast of Natal, Brazil          12               216              12                   216      0    456
#5078 2002-05-25                            Off Penghu Island, Taiwan          19               206              19                   206      0    450
#5722 2015-10-31                                    Near Hasna, Egypt           7               217               7                   217      0    448
#4881 1999-10-31                  Off Nantucket Island, Massachusetts          15               202              15                   202      0    434

nrow(most)
# 11

#sep11
most[which(most$ground + most$fatalities_crew + most$fatalities_passengers > 1000),]
#The greatest "corner case"

#date month weekday  time       lon     lat                 address aboard_crew aboard_passengers fatalities_crew fatalities_passengers ground
#5017 2001-09-11   Sep     Tue 09:03 -74.01024 40.7029 New York City, New York           9                56               9                    56   2750
#5018 2001-09-11   Sep     Tue 08:47 -74.01024 40.7029 New York City, New York          11                81              11                    81   2750

#NOVA AMOSTRAGEM SEM OS "CORNER CASES" -  só as mortes
deaths = planecrash_last20[which(planecrash_last20$ground + planecrash_last20$fatalities_crew + planecrash_last20$fatalities_passengers <= 200), c('aboard_crew','aboard_passengers','fatalities_crew','fatalities_passengers', 'ground')]
deaths[is.na(deaths)]= 0

deaths_l = apply(deaths[,c(3, 4, 5)], 1, sum)

#deaths_atmost200
boxplot(deaths_l + 1, log="y")

#death_summary_atmost100
summary(deaths_l)

#Desvio padrão
sd(deaths_l)
#[1] 18.12687

#Moda
Mode(deaths_l)
#2

#Quantitativo geral de mortes
deaths_c = apply(deaths[,c(3, 4, 5)], 2, sum)

lbls = c('fatality crew', 'fatality passagers','ground')
pct = round(deaths_c/sum(deaths_c)*100)
lbls = paste(lbls, pct)
lbls = paste(lbls,"%",sep="")

#pie_deaths
pie(deaths_c[c('fatalities_crew','fatalities_passengers','ground')], labels = lbls, main="Vítimas por tipo")

#MORTES APENAS ENTRE A TRIPULAÇÃO
#AGORA CONSIDERANDO A CAPACIDADE DA AERONAVE OU TRIPULAÇÃO ENVOLVIDA (ainda com amostragem sem os corner cases)

deaths_rates = cbind(deaths, deaths$fatalities_crew/deaths$aboard_crew)
deaths_rates = cbind(deaths_rates, deaths$fatalities_passengers/deaths$aboard_passengers)
deaths_rates[is.na(deaths_rates)] = 0
deaths_rates[deaths_rates == Inf] = 0
names(deaths_rates)[6:7] = c("fatalities_crew_rate", "fatalities_passengers_rate")

#Tirando a média da taxa de vitimas de ambos os tipos 

dt = apply(deaths_rates[,6:7], 2, mean)
df = data.frame(group = c("Fatalities Crew", "Fatalities Passengers"),value = dt)
#bp_deaths_rate
barplot(dt, names.arg=c("Fatalities Crew", "Fatalities Passengers"), col=c("lightblue","#ff9966"))

#ANÁLISE TEMPORAL
library(scales) 
library(gridExtra)
library(ggplot2)  

#Observar a temdencia de occorência de acordo com o mès e dia da semana em TODO O CONJUNTO DE DADOS

#pie_occr_month
pie(table(planecrash_last20$month))

#pie_occr_weekday
pie(table(planecrash_last20$weekday))

#Observa-se homogeneidade e sendo assim nenhuma tendencia pode ser verificada...

#ANÁLISE GEOGRÁFICA

##GIS
write.csv(planecrash_last20, file="planecrash_last20.csv")
#gis_overall

#PLOT AREAS UNDER DENSITY-BSED CLUSTERING ALGRTHM
library(tibble)
library(dbscan)
library(dplyr)
set.seed(101)

data = planecrash_last20

data = droplevels(distinct(data, crash_address, .keep_all = T)[, c("crash_address","crash_lon","crash_lat")])
data = data[complete.cases(data), ]
data$crash_address = as.character(data$crash_address)
rownames(data) <- c()
data = column_to_rownames(data, "crash_address")
data = as.matrix(data)

eps=0.05 #=~5km
minPts=3 #3pts
dclust = dbscan(data, eps, minPts, borderPoints=T)

#hp_density
hullplot(data, dclust, main="", solid = T, col =c("darkblue"), hull_lwd=20)

#EVOLUÇÃO DE OCORRÊNCIAS AO LONGO DOS ANOS

#Gráfico de densidade

#his_crash_density
ggplot(new_planecrash,  aes(..density..)) + 
  geom_histogram(binwidth=600, aes(x= new_planecrash$date), position='Identity') + 
  scale_x_date(name="Data", breaks=date_breaks("96 months"), labels=date_format("%Y")) +
  #scale_y_log10(name="Daily Number") # for gowalla big-size data base
  scale_y_continuous(name="Ocorrências")+
  theme_light()

#Classificação de Acidentes por número de vitimas

deaths_class = new_planecrash[, -c(2:15)]
deaths_class = cbind(deaths_class, apply(deaths_class[,c(3:5)], 1, sum))
names(deaths_class)[6] = "deaths"

classify = function(a) {
  #print(a)
  if(is.na(a)) {
    NA
  }else if(a <= 10) {
    'small'
  } else if(a > 10 & a < 50) {
    'average'
  } else if(a >= 50 & a < 100) {
    'big'
  } else if (a >= 100) {
    'catastrophe'
  } else {
    NA
  }
}

deaths_class = deaths_class[complete.cases(deaths_class), ]
deaths_class = cbind(deaths_class, sapply(deaths_class[,6], classify))
names(deaths_class)[7] = "class"

#his_crash_class
ggplot(deaths_class) + 
  geom_histogram(binwidth=600, aes(date, fill=class))

deaths_class[which(deaths_class$ground + deaths_class$fatalities_crew + deaths_class$fatalities_passengers > 1000),]


#PARTE 3 - Teste de Hipóteses
## 1- Verificar relação entre eventos (alguns tipos) e a frequencia de incidentes




