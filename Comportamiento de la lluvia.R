
########TALLER 2###########
###########################
#                         #
#                         #
#                         #
#     MATIAS LEIVA        #
#    19.4243.300-6        #  
#                         #
###########################

#####Ejercicio 1#####

# Pregunta 1 --------------------------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)
library(janitor)
library(scales)
library(skimr)
library(tidyverse)
library(epitools)
library(InformationValue)
library(ROCit)

bikes <- read_csv("bikes.csv")
View(bikes)

bikes <- bikes %>% janitor::clean_names()
bikes <- bikes %>% glimpse()

#1.a. Transforme a factor las variables categóricas.

#Variables a transformar: weekday, weather, month, year, workingday y season:


bikes$weekday <- factor(bikes$weekday,
                        levels = c(0,1,2,3,4,5,6), 
                        labels = c("Domingo", "Lunes", "Martes", "Miércoles",
                                   "Jueves", "Viernes", "Sábado"))

          
bikes$weather <- factor(bikes$weather,
                        levels = c(1,2,3,4),
                        labels = c("Claro a parcialmente nublado",
                                   "Niebla, pero sin precipitaciones más intensas",
                                   "Lluvia ligera o nieve, posiblemente con truenos",
                                   "Fuertes lluvias o nieve"))


bikes$month <- factor(bikes$month,
                      levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                      labels = c("Enero", "Febrero", "Marzo", "Abril",
                                 "Mayo", "Junio", "Julio", "Agosto",
                                 "Septiembre", "Octubre", "Noviembre",
                                 "Diciembre"))

bikes$year <- factor(bikes$year,
                     levels = c(0,1),
                     labels = c("2020", "2021"))

bikes$workingday <- factor(bikes$workingday,
                           levels = c(0,1),
                           labels = c("No laboral", "Laboral"))


bikes$season <- factor(bikes$season,
                       levels = c(1,2,3,4),
                       labels = c("spring", "summer", "fall", "winter"))
                       
                                       
bikes <- bikes %>% glimpse() 


#1.b. Transforme las variables temp y atemp a grados celsius, ya que se encuentran en escala normalizada.


bikes <- bikes %>% 
  mutate(temp = 47*temp-8,
         atemp = 66*atemp-16,
         humidity = (humidity*100),
         windspeed = (windspeed*67))

view(bikes)


# Pregunta 2 --------------------------------------------------------------

#Realice un gráfico del número de arriendos de bicicletas vs temperatura, además, añada una curva de regresión lineal. 
#¿Cuál es la relación entre el número de arriendos con la temperatura?



bikes %>% 
  ggplot(aes(x=temp, y=target))+
  geom_point(shape = 15
             , colour = "purple1", size = 1)+
  geom_smooth(method = lm, formula = y~x)+
  labs(title = "Arriendo de bicicletas VS Temperatura", caption = "Pregunta 2")

#2.R:Existe una relación positiva entré la temperatura y el numero de arriendos de bicicleta, a medida que aumenta la temperatura, aumenta el numero 
#    de arriendos de bicicletas.


# Pregunta 3 --------------------------------------------------------------

#¿Es la relación entre la temperatura y el número de bicicletas arrendas igual en los dos años?, 
#realice en un mismo gráfico la relación entre el número de arriendo vs temperatura para los dos años, añada las curvas de regresión lineal.


bikes %>% 
  ggplot(aes(x=temp, y=target, col = year))+
  geom_point(shape = 15
             , colour = "purple1", size = 1)+
  geom_smooth(method = lm, formula = y~x)+
  labs(title = "Arriendo de bicicletas VS Temperatura y el Año", caption = "Pregunta 3")


#3.R:Se mantiene la relación positiva entré temperatura y numero de arriendos de bicicleta, pero el año 'year' 2021 tiene una pendiente más alta 
#(Beta más grande) por lo que, por cada aumento de temperatura, tenemos que para el año 2021 aumenta más el número de arriendo de bicicletas que el año 2020


# Pregunta 4 --------------------------------------------------------------

#Utilice las función step() de R para construir un modelo por selección forward sin considerar las variables: date, month y registered
#Interpreta el factor asociado a la Temperatura y días feriados

bikes <- bikes %>% 
  select(-date, -month, -registered)

modelo_lm_forward <- lm(target~1 , data=bikes) 

modelo_stepwise <- step(modelo_lm_forward, direction = "forward", scope = target~holiday + temp + season + weekday +weather + year + workingday + atemp + humidity + windspeed)

summary(modelo_stepwise)

#4.R:Al realizar la función step() y armar el modelo desde la perspectiva de dirección forwad, las variables dias feriados (Holidays) y temperatura (temp),
# ingresan al modelo en el sexto y septimo lugar respectivamente, dado que el impacto en la reducción del AIC es pequeño pero siguen siendo variables significantes (significativas)


# Pregunta 5 --------------------------------------------------------------


#Utilice los residuos del modelo elegido para estudiar la validez de los supuestos: Normalidad, Independencia y Homocedasticidad

ggplot(data = NULL, aes(sample= modelo_stepwise$residuals))+
  stat_qq()+
  stat_qq_line(color = "orange")

#Test Normalidad 


## H0: residuos distribuyen normal
## H1: residuos no distribuyen normal

nortest::lillie.test(modelo_stepwise$residuals)

#Siendo el pvalue del test es menor que el nivel de significancia (0,05), se rechaza la hipótesis nula que los datos residuales 
#siguen una distribución normal

#Test Independencia 


## H0: residuos no estan correlacionados
## H1: residuos están correlacionados
lmtest::dwtest(modelo_stepwise)


#Siendo el pvalue del test menor que el nivel de significancia (0,05), se  rechaza la hipótesis nula de que los datos residuales 
#no estan auto correlacionados

#Supuesto Homocedasticidad

plot(modelo_stepwise,3)
#podria darnos indicios de que los residuos no son homocedasticos

#Test de Homocedasticidad (TEST bREUCH pAGAN)

## H0: residuos son homocedasticos
## H1: residuos no son homocedasticos

lmtest::bptest(modelo_stepwise)

#Siendo el pvalue del test menor que el nivel de significancia(0,05), se rechaza la hipótesis nula de que los errores son homocedasticos


##¿Es un modelo adecuado estadísticamente? Justifique sus afirmaciones##

#5.R:Como conclusion, al no cumplir con los 3 supuestos de datos residuales, normalidad, independencia y homocedasticidad, nos indican 
# que este no es un buen modelo estadístico.


# Pregunta 6 --------------------------------------------------------------


#Realice una predicción de arriendos de bicicletas para un día con las siguientes cualidades:



df_p6 <- tibble(season = "summer",
       year = "2021",
       holiday = 1,
       weekday = 'Sábado', 
       workingday = 'No laboral',
       weather = 'Niebla, pero sin precipitaciones más intensas',
       temp = 12,
       atemp = 11,
       humidity = 66.3,
       windspeed = 12.5)


predict(modelo_stepwise, newdata = df_p6)


#Dada las cualidades indicadas, según nuestro modelo de regresión lineal se estima un arriendo de aproximadamente 4636 bicicletas en el día.



#####Ejercicio 2#####


# Pregunta 7 --------------------------------------------------------------

lluvia <- read_csv("Lluvia_full.csv")

lluvia <- lluvia %>% janitor::clean_names()
View(lluvia)


#Codificación de la variable "lluvia_man" entre 0 y 1 

lluvia <- lluvia %>% 
  mutate(lluvia_man = as.factor(ifelse(lluvia_man == "Yes", 1, 0)))

summary(lluvia)
lluvia <- lluvia %>% glimpse()

#Semilla 2022 y proporción de 80:20

set.seed(2022)

samplelluvia <- sample(1:nrow(lluvia),
                       size = 0.8*nrow(lluvia), replace = FALSE)

train <- lluvia %>% 
  slice(samplelluvia)

test <- lluvia %>% 
  slice(-samplelluvia)



# Pregunta 8 --------------------------------------------------------------

#Realice un modelo de regresión logística para predecir si lloverá mañana utilizando la variable Evaporación,
#¿es este Un factor significativo? Intérprete el odd ratio de la evaporación.

modelo_glm <- glm(lluvia_man ~ evaporacion, data = train,
                  family = binomial(link = "logit"))

summary(modelo_glm)



broom::tidy(modelo_glm) %>% 
  mutate(oddsratio = exp(estimate), orp=exp(estimate)-1)


#8.R: la variable evaporacion es una variable significativa donde su Pvalue es menos que el nivel de significancia (0,05), ademas corresponde a un factor "protector"
#      esto dado que posee un Odds Ratio(OR)de 0,902 lo que es menor que 1.Esto implica que por cada aumento de la variable evaporacion existira una disminucion de la
#      probabilidad de ocurrencia o de la probabilidad de lluvia al dia siguiente.
#      al restarle 1 a OR de la variable evaporacion podemos concluir que frente a aumentos de los mm de evaporacion en las ultimas 24 hrs
#      tenemos una disminucion de la probabilidad de luvia del dia siguiente en un 9,79%


# Pregunta 9 --------------------------------------------------------------


#Utilizando un método automatizado, 
#ajuste un modelo de regresión logística, utilizando la metodología de dirección both (forward y backward a la vez).


modelo_glm2 <- glm(lluvia_man~., data = train,
                   family = binomial(link = "logit"))

modelo_both <- step(modelo_glm2, direction = "both")

modelo_both$formula
summary(modelo_both)

#9.R: ajustando el modelo y utilizando la metodologia de direccion both, se eliminan del modelo final las variables nub9am y temp3pm.

# Pregunta 10 -------------------------------------------------------------

#Considerando la base de entrenamiento, ajuste la curva ROC y KS asociada al modelo
#¿Qué puede concluir sobre la discriminación del modelo?. Con la información obtenida encuentre un punto de corte que tenga una sensibilidad 
#mínima del 80% y la máxima especificidad.

 #Kolmogorov Sminrnov


prob <- predict.glm(modelo_both, newdata = train, type = "response")

prob %>% head()

InformationValue::plotROC(train$lluvia_man, prob, returnSensitivityMat = TRUE)

#Punto de corte es el 0.20 

InformationValue::optimalCutoff(train$lluvia_man, prob, 
                                optimiseFor = "Both")

ks <- ksplot(rocit(score = prob, class = train$lluvia_man))

ks$'KS stat' #60.66% 

prob %>% head()

#10.R: el valor del area bajo la curva ROC (AUROC) es de un 88.61% lo cual lo deja dentro del rango de valores entre 75% y 90%,
#     lo que indica que el modelo logra tener una buena discriminación sobre los datos.
#     Por otro lado el valor obtenido del test KS, corresponde a un 60.66% lo cual se situa dentro de un indice KS muy bueno (entre un 60% y 75%).


# Pregunta 11 -------------------------------------------------------------


#Considerando la base de test, obtenga nuevamente la curva ROC y KS asociada al modelo, 
#¿cómo han variado los indicadores?, además, utilizando el punto de corte obtenido, obtenga la precisión.

prob2 <- predict.glm(modelo_both, newdata = test, type = "response")

prob2 %>% head()

InformationValue::plotROC(test$lluvia_man, prob2, returnSensitivityMat = TRUE)


ks <- ksplot(rocit(score = prob2, class = test$lluvia_man))

ks$'KS stat'

puntoc2 <- 0.20 #El punto de corte se calcula a través del resultado de la pregunta 10 

#Cálculo de precisión 
InformationValue::precision(test$lluvia_man, prob2, threshold = puntoc2)





#11.R: considerando la base test tenemos que el valor AUROC queda en un 88,71% y el valor del test de KS queda en 61,25%, con lo que podemos
#      ver que en ambos caso los indicadores suben un poco respecto a los valores obtenidos con la base train, pero siempre quedando dentro de los
#      mismos rangos, entre 75% y 90% para el valor de AUROC yentre 60% y 75% para el KS, lo cual esta clasificado como muy bueno.
#      
#      Finalmente al utilizar el punto de corte obtenido (0.20) tenemos que la presicion del modelo es de un 52.30%

# Pregunta 12 -------------------------------------------------------------


p12 <- tibble(MinTemp = 7,
       MaxTemp = 18,
       Lluvia  = 0,
       Evaporacion = 7,
       Sol = 12,
       VelRafaga = 72,
       Vel9am = 10,
       Vel3pm = 54,
       Hum9am = 65,
       Hum3pm = 77,
       Pres9am = 1001,
       Pre3pm = 1025,
       Nub9am = 3,
       Nub3pm = 2,
       Temp9am = 11.4,
       Temp3pm = 16.2,
       LluviaHoy = 'No',
       Koppen = 'Subtropical',
       Estacion = 'Primavera')


p12final <- p12 %>% janitor::clean_names()

predict(modelo_both, newdata = p12final , type ="response")


#Dada las cualidades indicadas, según nuestro modelo de regresión logística se estima que la probabilidad de lluvia para el día mañana es de 1.16%

