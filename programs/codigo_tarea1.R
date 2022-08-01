#########################################
#                                       #
#     Herramientas computacionales      #
#          para investigación           #
#                                       #
#       Profesora: Amelia Gibbons       #
#      Alumnos: Pacheco y Riquelme      #
#                                       #
#           MAE UdeSA 2022              #
#                                       #
#########################################


# Definimos los strings para que luego definir el directorio
main = "/Users/tomaspacheco/Desktop/Herramientas-PS5/Tarea 1"
input = paste(main, "/input", sep = "")
output = paste(main, "/output", sep = "")

setwd(input)

# Definimos la paleta de colores 

colores <- c("#00ABC5","#cfb0b4" ,"#ff3c84","#FF7F32", "#edf71c", "#941cf7")

colores2 <- c("#f5fa7b", "#3cffb7", "#ffdaaa", "#fa66f3",
              "#f84a42","#9e9d9c", "#c3f842", "#3cff56")


#### Gráficos originales ### 

## Gráfico 1 ##

# Descargamos la base de datos 

df3 <- read.csv("LoanStats.csv")


df3s <- subset(df3,grade %in% c("A","B","C","D","E","F","G"))
pb1<-ggplot(df3s,aes(x=loan_amnt))
pb1
pb2<-pb1+geom_density(bins=10,fill="cadetblue4")
pb2

pb3<-pb2+facet_wrap(~grade) 
pb3

pb4<-pb3+geom_histogram() + facet_wrap(~grade)
pb4

ggsave(file = "primergrafico_original.eps", width = 6.5, height = 4, dpi = 300)


## Gráfico 2 ##

# Descargamos la base de datos 
df <- read.csv("gapminder-data.csv")
dfs <- subset(df,Country %in% c("Germany","India","China","United States","Japan"))


ggplot(dfs, aes(gdp_per_capita,Electricity_consumption_per_capita,color=Country)) + geom_point() + stat_smooth(method=lm)

ggsave(file = "segundografico_original.eps", width = 6.5, height = 4, dpi = 300)


## Gráfico 3 ##

df_fb <- read.csv("FB.csv")
df_fb$Date <- as.Date(df_fb$Date)


df_fb$Month <- strftime(df_fb$Date,"%m")
df_fb$Month <- as.numeric(df_fb$Month)
ggplot(df_fb, aes(Month,Close)) + 
  geom_point(color="red",alpha=1/2,position = position_jitter(h=0.0,w=0.0))+
  geom_line(stat='summary',fun.y=mean, color="blue",size=1)+
  scale_x_continuous(breaks=seq(0,13,1))+
  ggtitle("Monthly Closing Stock Prices: Facebook")+theme_classic()

# Lo descargamos a mano como PDF

#### Gráficos corregidos ### 


## Gráfico 1 ##

# Juntamos todos los histogramas en un solo gráfico. Decidimos que cada barra tenga un color un poco 
# transparente para así poder comparar mejor entre categorías. 

ggplot(df3, aes(x = loan_amnt, fill = grade)) + 
  labs(x = "Préstamo", 
       y = "Frecuencia", 
       title = "Monto del préstamo para diferentes grados de crédito",
       caption = "Fuente: elaboración propia")+
  theme_minimal() +  
  geom_histogram(position = "identity", alpha = 0.4) + 
  guides(fill=guide_legend(title="Grados"))


# Guardamos el gráfico como jpg para que se mantenga la transparencia de los colores: 

jpeg("primergrafico.jpeg", quality = 85)

## Gráfico 2 ##

# Cargamos las librería para poder realizar zoom en los gráficos 

library(ggforce)
library(tidyverse)

# Graficamos pero haciendo zoom sobre dos países:

ggplot(dfs, aes(gdp_per_capita,Electricity_consumption_per_capita,color=Country)) + scale_y_log10()+
  geom_point() + labs(x = "PBI per cápita", 
                      y = "Consumo de electricidad per cápita", 
                      title = "Relación entre consumo de electricidad y PBI",
                      caption = "Fuente: elaboración propia")+ stat_smooth(method=lm)+facet_zoom(xlim = c(1000, 10000)) + theme_bw() +
  theme(legend.position = "right", legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5)) 

ggsave(file = "segundografico_modificado.eps", width = 6.5, height = 4, dpi = 300)



## Gráfico 3 ##


# Primero creamos una variable que tenga solo el año

df_fb$Year <- strftime(df_fb$Date,"%y")

# Ahora, creamos una variable que tenga el año completo (dado que la anterior solo tiene un número 
# de dos cifras): 

df_fb$Año[df_fb$Year==17]<-"2017"

df_fb$Año[df_fb$Year==18]<-"2018"


# Creamos la media de los precios de cierre para cada año: 

mean <- df_fb%>% group_by(Año)%>%summarise(mean_val=mean(Close))


# A continuación graficamos un scatter plot, pero diferenciando según los años. También 
# agregamos la media del precio de cierre de Facebook para cada año: 

ggplot(df_fb, aes(Month,Close,color=Año)) + scale_y_log10()+
  geom_point() +labs(x = "Mes", 
                     y = "Precios de cierre (USD)", 
                     title = "Precios de cierre mensuales: Facebook",
                     caption = "Fuente: elaboración propia") +
  scale_x_continuous(breaks=seq(0,13,1)) +
  geom_line(stat='summary',fun.y=mean, color=colores2[6], size=1) + 
  theme_minimal() + geom_hline(data= mean, aes(yintercept = mean_val,col=Año))
  
ggsave(file = "tercergrafico_modificado.eps", width = 6.5, height = 4, dpi = 300)





