# Test Inlelimetrica

library(tidyverse)
library(stargazer)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(kableExtra)


setwd("C:/Users/Samuelk/Documents/Intelimetrica")

db <- read.csv(file = "test_analista.csv",
                 header = TRUE,
                 sep = "|")
summary(db)

# Edad y superficie cuentan con demasiados Na's por lo que no serán consideradas

#Cambiamos la fecha de carácter a fecha
db$fecha_oferta <- as.POSIXct(db$fecha_oferta)

#Reviso las columnas que contienen NAs
colSums(is.na(db))

# Promedios 
Tb1 <- db %>% group_by(nombre) %>%
  summarize(precio_m2_prom=mean(precio_m2),oferta_prom=mean(precio_oferta))


#Corroboro similitudes entre variables
db$suproxy <- db$precio_oferta/db$precio_m2
db$pres <- db$precio_m2*db$area_construida

precio <- subset(db, select=c(9,18))
precio$diff <- precio$precio_oferta-precio$pres
sum(precio$diff > 1)

terreno <- subset(db, select = c(2,14,16))
db$plus <-
terreno$dif <- terreno$area_construida-terreno$suproxy
sum(terreno$dif > 1)


#Eliminamos los codigos regionales, edad y superficie  
dbs <- subset(db, select = -c(5,7,8:10,12:18))

#LLenamos las NAs con 0
dbs[is.na(dbs)] <- 0
colSums(is.na(dbs))

#ML
reg <- lm(precio_m2 ~ ., data = dbs)
stargazer::stargazer(reg, type = "text")

coef <- as.data.frame(reg$coefficients)

#Graficas

coefplot <- coef %>%
  arrange(desc(reg$coefficients)) %>%
  slice(1:10)

rownames(coefplot) <- c("San Miguel","Tlalpexco","Bosque De Chapultepec I",
                        "Club De Golf Bosques", "Desarrollo Urbano",
                        "Rincon Del Bosque", "Olivar Del Conde 2Da Seccion",
                        "Polanco", "Bosque De Chapultepec III", "San Mateo Tlaltenango")

coefplot <- tibble::rownames_to_column(coefplot, "VALUE")

colnames(coefplot) <- c("colonia", "coef")


options(scipen=10000000)

#Efecto promedio de la zona
ggplot(coefplot, aes(x=colonia, y=coef)) +      
  theme(axis.text=element_text(size=20),
         axis.title=element_text(size=20,face="bold"))+
  xlab("")+
  ylab("")+
  geom_col(color="black", fill = "#FF6666")+
  coord_flip()

#Efecto promedio variables

factores <- as.data.frame(t(coef))
factores <- factores %>% select(c(1,565:568))
rownames(factores)<- c("Coeficiente")
colnames(factores) <- c("Constante", "Area Construida", "Recamaras", "Baños", "Estacionamiento")

kbl(factores, booktabs = T, caption = "<center><strong>Efecto promedio sobre el pracio del m2</strong></center>",
    digits = c(2,2,2,2,2)) %>%
  kable_classic_2(full_width = F, html_font = "Cambria", fixed_thead = T) %>%
  add_footnote("Nota: Estos resultados se obtuvieron de una regresión lineal simple, considerando estos factores mostrados y controlando por colonia, de igual forma aunque pareciera que a mayor área construida, estacionamientos y recamaras el precio decrece es debido a la alta presencia de mayor número de estas variables en las zonas con mayor precio promedio fijo.")

