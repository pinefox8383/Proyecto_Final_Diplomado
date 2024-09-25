#Proyecto Final  - Precios de Laptops
#Origen de dataset: https://www.kaggle.com/datasets/owm4096/laptop-prices?select=laptop_prices.csv
#Descarga de archiVO Fuente:  laptop_prices.csv
library(tidyverse)
library(dplyr)
#.......................................................................................
#2.- Preparación y Limpieza de los Datos.................................................
#.......................................................................................
setwd('C:/proyecto final') #Aplicando folder de trabajo   {opcional}
df<-read.csv("csv fuente/laptop_prices.csv") #Importando archivo csv en un dataframe
head(df) #Explorando los primeros Datos
View(df) #visualizando el data frame para comprender mejor los datos 
colSums(is.na(df)) #Visualizando nulos que puede afectar nuestro análisis
df<-drop_na(df)#limpiando nulos en dataframes    {dataset limpio no hay necesidad de reportar errores}
str(df) #visualizando estructura de datos

#.......................................................................................
#3.- Análisis Exploratorio de Datos ....................................................
#.......................................................................................
#visualizando estadísticas generales
summary(df)

#histograma de Precios porCompañías
df %>%drop_na() %>%   ggplot(aes(x=Price_euros,fill=Company)) + geom_histogram()

#histograma de Precios por Sistema Operativo
df %>%drop_na() %>%   ggplot(aes(x=Price_euros,fill=OS)) + geom_histogram()

#histograma de Precios por Compañía CPU
df %>%drop_na() %>%   ggplot(aes(x=Price_euros,fill=CPU_company)) + geom_histogram()

#Estadísticas de laptops INTEL por Compañía
Resumen_precios_Company_INTEL<-
  df %>% 
  select(Company,Price_euros,CPU_company) %>% 
  filter(CPU_company=="Intel") %>% 
  drop_na() %>% #quitando nulos
  group_by(Company) %>% 
  summarise( Precio.mediana=median(Price_euros),
             Precio.promedio=mean(Price_euros),  
             Precio.maxima=max(Price_euros),              
             Precio.minima=min(Price_euros),
             Recuento=n()
  ) %>%
  ungroup()
view(Resumen_precios_Company_INTEL)#visualizando Resumen 
Resumen_precios_Company_INTEL %>%drop_na() %>%   ggplot(aes(x=Recuento,fill=Company)) + geom_histogram()
write.csv(Resumen_precios_Company_INTEL,"Resumen_precios_Company_INTEL.csv") #EXportando Resumen a archivo csv


#Estadísticas de laptops AMD por Compañía
Resumen_precios_Company_AMD<-
  df %>% 
  select(Company,Price_euros,CPU_company) %>% 
  filter(CPU_company=="AMD") %>% 
  drop_na() %>% #quitando nulos
  group_by(Company) %>% 
  summarise( Precio.mediana=median(Price_euros),
             Precio.promedio=mean(Price_euros),  
             Precio.maxima=max(Price_euros),              
             Precio.minima=min(Price_euros),
             Recuento=n()
  ) %>%
  ungroup()
view(Resumen_precios_Company_AMD)#visualizando Resumen 
Resumen_precios_Company_AMD %>%drop_na() %>%   ggplot(aes(x=Recuento,fill=Company)) + geom_histogram()
write.csv(Resumen_precios_Company_AMD,"Resumen_precios_Company_AMD.csv") #EXportando Resumen a archivo csv


#Estadísticas de laptops Por Compañía y Modelo de CPU
Resumen_precios_Company_cpu_model<-
  df %>% 
  select(Company,CPU_model,Price_euros,CPU_company) %>% 
  #filter(CPU_company=="AMD") %>% 
  drop_na() %>% #quitando nulos
  group_by(Company,CPU_model) %>% 
  summarise( Precio.mediana=median(Price_euros),
             Precio.promedio=mean(Price_euros),  
             Precio.maxima=max(Price_euros),              
             Precio.minima=min(Price_euros),
             Recuento=n()
  ) %>%
  ungroup()
view(Resumen_precios_Company_cpu_model)#visualizando Resumen 
write.csv(Resumen_precios_Company_cpu_model,"Resumen_precios_Company_cpu_model.csv") #EXportando Resumen a archivo csv
Resumen_precios_Company_cpu_model %>%drop_na() %>%   ggplot(aes(x=Recuento,fill=CPU_model )) + geom_histogram()
Resumen_precios_Company_cpu_model %>%drop_na() %>%   ggplot(aes(x=Recuento,fill=Company )) + geom_histogram()





#Gráfico de Barras  recuento de Laptops por CPU Company fill CPU
df %>% 
  #filter(CPU_company=="AMD") %>%   
  drop_na() %>% #quitando nulos  
  ggplot(aes(x=CPU_company,fill=CPU_company)) + 
  geom_bar()

#Gráfico de Barras  recuento de Laptops por CPU Company  fill OS
df %>% 
  #filter(CPU_company=="AMD") %>%   
  drop_na() %>% #quitando nulos  
  ggplot(aes(x=CPU_company,fill=OS)) + 
  geom_bar()


#Gráfico de Barras  recuento de Laptops por Sitema operativo fill cpu
df %>% 
  #filter(CPU_company=="AMD") %>%   
  drop_na() %>% #quitando nulos  
  ggplot(aes(x=OS,fill=CPU_company)) + 
  geom_bar()






#diagrama de dispersión de los datos por Precio, Compañía y CPU Compañía
df %>%
  drop_na() %>% #quitando nulos      
  ggplot(aes(x=Company, 
             y=Price_euros,
             size=Price_euros,
             color=CPU_company)) + 
  geom_point(position = "jitter") 

#diagrama de dispersión de los datos por Precio, Compañía y CPU Compañía
df %>%
  drop_na() %>% #quitando nulos      
  ggplot(aes(x=Company, 
             y=Price_euros,
             color=CPU_company)) + 
  geom_point(position = "jitter") 



#Obervando Distribución en Matriz (Grid) con otras variables 
#Observando que: hay una gran distribución y concentración en equipos con Windows 10
# y la mayoría cuentan con CPU INTEL que es el de mayor demanda o preferencia

df %>%
  drop_na() %>% #quitando nulos      
  ggplot(aes(x=CPU_model, 
             y=Price_euros,
             #size=Price_euros,
             color=CPU_model)) + 
  geom_point(alpha=0.5)+
  facet_grid(CPU_company ~ OS)  










