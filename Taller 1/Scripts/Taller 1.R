#===================================#
#### BIG DATA Y MACHINE LEARNING #### 
#           PROBLEM SET #1          #
#===================================#
# Creation Date: 02/09/2023
# Mod Date: XX/09/2023
# R version 4.3.1 

# AUTHORS:
# 1. Federico Camacho Amar
# 2. Andrés David Chaparro Florez
# 3. Juan David Perez Velez
# 4. Ricardo Andrés Silva Torres
#===================================#


#==================================#
#### [1.] Paquetes y entorno ####  
#==================================#
# Limpieza del entorno de trabajo e instalación de paquetes
rm(list=ls())
install.packages("pacman")
library(pacman)

p_load(rvest, tidyverse, skimr, jsonlite, stargazer, hrbrthemes)

getwd()
setwd("~/Documents/GitHub/BigData-MachineLearning202320")
list.files()

#==================================#
#### [2.] Web Scraping ####  
#==================================#
browseURL("https://ignaciomsarmiento.github.io/GEIH2018_sample/", getOption("browser"))
# PAG WEB: https://ignaciomsarmiento.github.io/GEIH2018_sample/
# PAG WEB DICT: https://ignaciomsarmiento.github.io/GEIH2018_sample/dictionary.html
# PAG WEB LABELS: https://ignaciomsarmiento.github.io/GEIH2018_sample/labels.html
# PAG de ayuda: https://www.projectpro.io/recipes/append-output-from-for-loop-dataframe-r

# Definición de dataframe vacio
df_final <- data.frame()

# For loop de web scraping
for (i in 1:10) {
  cat("-- Iteración = ", i, " --","\n")
  url <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", i, ".html")
  
  chunk_html <- read_html(url)
  # Mismo xpath para cada uno de los data chunks de PAG WEB. 
  # Clave: geih_html en network.
  # Xpath = "/html/body/table"
  
  # Creando tabla por web scraping
  df <- chunk_html %>% 
          html_nodes(xpath =  "/html/body/table") %>% 
          html_table() %>% 
          as.data.frame()
  
  # Añadir df a dataframe final y remover df del loop
  df_final <- rbind(df_final, df)

  # Finalizada iteración del loop
  cat("-- Completada iteración --", "\n", "\n")
}

format(object.size(df_final), units = "MB")
write_csv(df_final, "/Users/ricardoandressilvatorres/Documents/GitHub/
                      BigData-MachineLearning202320/Taller 1/stores/df_final.csv")



#===========================================#
#### [3.] Limpieza de los datos y graphs ####  
#===========================================#
df_final <- read.csv("~/Documents/GitHub/BigData-MachineLearning202320/Taller 1/stores/df_final.csv")
dim(df_final)
glimpse(df_final)

# Valores de dsi:	=1 if unemployed; =0 otherwise
c("dsi", "age") %in% names(df_final)
df_filtered <- subset(df_final, age >= 18 & dsi == 0)

# Media de salarios
mean(df_filtered$y_ingLab_m, na.rm = T)
mean(df_filtered$y_salary_m, na.rm = T)
mean(df_filtered$y_total_m, na.rm = T)

# Estad Descriptivas con Stargazer
stargazer(df_filtered, type = "text")

# Crea la nueva variable "salario_nuevo" y reemplaza los valores faltantes con la media
media_salario_hora <- mean(df_filtered$log_salario_hora, na.rm = TRUE)
df_filtered$log_salario_completo_hora <- ifelse(is.na(df_filtered$log_salario_hora), media_salario_hora, df_filtered$log_salario_hora)

# Variables de salario disponibles:
# ingtot -> ingreso total
# ingtotes -> Ingreso total imputado
# ingtotob -> Ingreso total observado
# y_ingLab_m -> Labor income salaried - Nominal Monthly
# y_ingLab_m_ha -> Labor income salaried - Nominal Hourly
# y_salary_m -> Salary - Nominal Monthly
# y_total_m -> Income Salaried + indep total + nominal monthly
# y_total_m_ha -> Income Salaried + indep total + nominal hourly


## Histograma de referencia - Con Comando de geom_histogram :)
# Referencia: https://r-graph-gallery.com/220-basic-ggplot2-histogram.html
ggplot(data=df_filtered, aes(x= y_total_m))+
  geom_histogram(bins = 25, fill="#69b3a2", color="#e9ecef") +
  theme_ipsum() +
  labs(x="Ingresos Mensuales") +
  ggtitle("Histograma de ingresos mensuales")


skim(df_filtered)
skim(df)
skim(db$age)


## Salario mensual y por hora
summary(df_filtered$age)
summary(df_filtered$y_ingLab_m_ha)
summary(df_filtered$y_ingLab_m)


# Create a boxplot to visualize the distribution and quartiles of income
## No se ve bien porque hay mucha varianza.
boxplot(df_filtered$y_ingLab_m_ha,
        main = "Income Distribution",
        ylab = "Income",
        col = "lightblue",
        border = "red",
        horizontal = TRUE)

boxplot(df_filtered$y_ingLab_m,
        main = "Income Distribution",
        ylab = "Income",
        col = "lightblue",
        border = "red",
        horizontal = TRUE)


# Crear una nueva variable de ingreso limitada a 1000 o menos ( se trunca en )
df_filtered$ingreso_limitado2 <- ifelse(df_filtered$y_ingLab_m <=12000 , 
                                        df_filtered$y_ingLab_m, 12000)

## Algo quedo mal, use arbitrariamente el 12000
df_filtered$ingreso_limitado2 <- ifelse(df_filtered$y_ingLab_m <=12000 , 
                                        df_filtered$y_ingLab_m, 12000)

## creo variables logaritmicas
df_filtered$log_salario_mensual <- log(df_filtered$y_ingLab_m)
df_filtered$log_salario_hora <- log(df_filtered$y_ingLab_m_ha)




boxplot(df_filtered$log_salario_mensual,
        main = "Income Distribution",
        ylab = "Income",
        col = "lightblue",
        border = "red",
        horizontal = TRUE)

# Create a histogram of the salary distribution
hist(df_filtered$log_salario_mensual,
     main = " Monthly Salary Distribution",
     xlab = "Salary in logs",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")
## se observa mejor la distribucion de salario


boxplot(df_filtered$log_salario_mensual,
        main = "Income Distribution",
        ylab = "Income",
        col = "lightblue",
        border = "red",
        horizontal = TRUE)


summary(df_filtered$p6426)
## cuanto lleva trabajando en esa empresa, me parece importante
## variable microempresa tambien me parece interesante



# Create a boxplot of income vs. college attendance
boxplot(log_salario_mensual ~ college, data = df_filtered,
        main = "Income vs. College Attendance",
        xlab = "College Attendance",
        ylab = "Income",
        col = c("lightblue", "lightgreen"))
## las dimensiones no son las mejores. ( curioso como se comporta el 1 que es
## educacion teriaria tiene menos varianza, pero se comporta en la media similarmente)


# Create a boxplot of income vs. Sex

boxplot(log_salario_mensual ~ sex, data = df_filtered,
        main = "Income vs. sex",
        xlab = "sexo (1=hombre, 0=mujer",
        ylab = "Income",
        col = c("lightblue", "lightgreen"))

## las dimensiones no son las mejores. ( curioso como se comporta el 1 que es
## educacion teriaria tiene menos varianza, pero se comporta en la media similarmente)

## si les gusta se puede hacer asi respecto a informal y tambien respecto a clase



# Create a vector representing the counts (0s and 1s)
counts <- table(df_filtered$microEmpresa)

# Create a pie chart
## libreria scales para que salga porcentaje en la grafica de torta
library(scales)

pie(counts, labels = c("Doesn't Work in Small Company", "Works in Small Company"),
    main = "Pie Chart of Employment in Small Companies",
    col = c("lightblue", "lightgreen"))

##
# Cargar librería si no está cargada
# install.packages("ggplot2")
library(ggplot2)

# Crear un gráfico de puntos con colores para la variable categórica

ggplot(df_filtered, aes(x = p6210, y =log_salario_mensual , color = educacion_anios)) +
  geom_point() +
  labs(title = "Gráfico de Puntos de Ingresos por Años de Educación",
       x = "Años de Educación",
       y = "Ingreso"
  )


ggplot(data=df_filtered, aes(x= p6210, y= log_salario_mensual ))+
  labs(x="edad", y="ingresos mensuales")


#===========================================#
#### [4.]  Ejercicio: Age-wage profile ####  
#===========================================#
## El ejercicio de regresion 

df_filtered$age2 <- df_filtered$age^2
summary(df_filtered$age2)
summary(df_filtered$age)

model1 <- lm(log_salario_hora ~ age + age2 , data=df_filtered)
summary (model1)
stargazer(reg,type="text")

## todas dan muy significativas, efectivamente el cuadrado es negativo y corrobora
## que la relacion es concava. Se cumple lo que la teoria menciona del descenso de ingreso
## a partir de edades altas (50 años)
## se interpreta log-lin ( un cambio B*100 es un cambio de y%)

## le podemos añadir edad al cubo a ver si es mas pronunciada la relacion de disminucion
## de ingreso en la vejez


library(ggplot2)
# Crear un gráfico de dispersión
ggplot(df_filtered, aes(x = age2, y = log_salario_hora)) +
  geom_point() +
  labs(
    title = "Gráfico de Dispersión\nEdad al Cuadrado vs Ingreso hora",
    x = "Edad al Cuadrado",
    y = "Ingreso"
  )

## en este scatter la relacion no es tan clara
## se remueven 1286 por missing value (warning)


## Tengo problema con el boostrap para hallar maximo

## creo que al derivar la solucion se encuentra en:
##  ------- Edad_maxima<-b1+2*b2*age2 --------




## Para hacer el FWL 4(b) El problema de mujer

## El punto a

 df_filtered$female <- 1 - df_filtered$sex

regmujer<- lm(log_salario_hora ~ female, data=df_filtered)
summary(regmujer)

reghombre<- lm(log_salario_hora ~ sex, data=df_filtered)
summary(reghombre)
## raro revisar con los demas datos (hombre gana menos que mujer)

regmujer_controles<- lm(log_salario_hora ~ female + sizeFirm + p6870 + oficio + depto + college+ cotPension, data=df_filtered)
summary(regmujer_controles)


## deberia funcionar, pero hay un problema de lenght y match
pic1<- lm(female ~ sizeFirm + p6870 + oficio + depto + college+ cotPension, df_filtered)
residuals1 <- residuals(pic1)

pic2<- lm(log_salario_hora ~ sizeFirm + p6870 + oficio + depto + college+ cotPension, df_filtered)
residuals2 <- residuals(pic2)

reg_mujer_controles2 <- lm (residuals2 ~ residuals1 )

stargazer(reg1,reg2,type="text",digits=7) 

## ------ i)FWL----------------------------------------------##
##intento con la media 

df_filtered2 <- data.frame(df_filtered)

media_variable <- mean(df_filtered2$log_salario_hora, na.rm = TRUE)
df_filtered2$log_salario_hora[is.na(df_filtered2$log_salario_hora)] <- media_variable


pic1<- lm(female ~ sizeFirm + p6870 + oficio + college + cotPension ,df_filtered2 )
residuals1 <- residuals(pic1)
pic2<- lm(log_salario_hora ~ sizeFirm + p6870 + oficio+ college + cotPension, df_filtered2)
residuals2 <- residuals(pic2)
pic3<- lm(log_salario_hora ~ female + sizeFirm + p6870 + oficio+ college+ cotPension, df_filtered2)

length(pic1)
length(pic2)

reg_mujer_controles2 <- lm (residuals2 ~ residuals1 )

stargazer(reg_mujer_controles2, type="text", digits=7)
stargazer(pic3, type="text", digits=7)

## ------- BOOTSRAP DE LA REGRESION(COEFICIENTE DE FEMALE)-------##
mod1<- lm(log_salario_completo_hora ~ female +sizeFirm + p6870 + oficio + depto + college+ cotPension, df_filtered)
stargazer(mod1,type="text", omit.stat=c("ser","f","adj.rsq"))

str(mod1)

mod1$coefficients


set.seed(123)

B<-1000 # Number of Repetions()


eta_mod1<-rep(0,B)#this is an empty vector where we are going to save our elasticity estimates


for(i in 1:B){
  
  db_sample<- sample_frac(df_filtered,size=1,replace=TRUE) #takes a sample with replacement of the same size of the original sample (1 or 100%)
  
  f<-lm(log_salario_completo_hora ~ female +sizeFirm + p6870 + oficio + depto + college+ cotPension,db_sample)# estimates the models
  
  coefs<-f$coefficients[2] # gets the coefficient of interest that coincides with the elasticity of demand
  
  eta_mod1[i]<-coefs #saves it in the above vector
}

length(eta_mod1)

plot(hist(eta_mod1))

## este es el valor
mean(eta_mod1)

sqrt(var(eta_mod1))

print(coefs)



##-------------------- SI SITRVE ( SAMPLE AND TRAIN)-----

## Como en clase
sample <- sample(c(TRUE, FALSE), nrow(df_filtered), replace=TRUE, prob=c(0.7,0.3))
head(sample)

train  <- df_filtered[sample, ] #train sample those that are TRUE in the sample index
test   <- df_filtered[!sample, ] #test sample those that are FALSE in the sample index
dim(train)



model4<-lm(log_salario_hora~poly(age,8,raw=TRUE):poly(maxEducLevel,3,raw=TRUE)
           + poly(fweight,3,raw=TRUE),data=train)
model4

coef (model4)
paste("Coef:", mean(train$log_salario_hora,na.rm=TRUE))
## aqui genero la prediccion de mis datos que entrene sobre el tes
test$model4<-predict(model4,newdata = test)

with(test,mean((log_salario_hora-model4)^2,na.rm=TRUE))

## aqui el error estandar bajo bastante mas. Sigue siendo alto
## El error es de 0,336

## intentar con distintas especificaciones



model6 <- lm(log_salario_hora ~ poly(age, 8, raw = TRUE) + poly(maxEducLevel, 3, raw = TRUE) + poly(fweight, 3, raw = TRUE) + poly(experiencia_ajustada2, 3, raw = TRUE), data = train)


## CREAR EXPERIENCIA (la que usamos es experiencia_ajustada2)


df_filtered$resta_edad <- ifelse(df_filtered$age > 65, df_filtered$age - 65, 0)

df_filtered$experiencia_ajustada2 <- ifelse(df_filtered$age > 65, df_filtered$age - df_filtered$resta_edad - df_filtered$maxEducLevel, df_filtered$age - df_filtered$maxEducLevel)





model5 <- lm(log_salario_hora ~ poly(age, 3, raw = TRUE) + 
               poly(maxEducLevel, 2, raw = TRUE) + 
               poly(fweight, 3, raw = TRUE) + 
               sizeFirm + 
               regSalud + 
               clase, 
             data = train)
model5

coef (model5)
paste("Coef:", mean(train$log_salario_hora,na.rm=TRUE))
## aqui genero la prediccion de mis datos que entrene sobre el tes
test$model5<-predict(model5,newdata = test)

with(test,mean((log_salario_hora-model4)^2,na.rm=TRUE))


model9 <- lm(log_salario_hora ~ poly(age, 2, raw = TRUE) + 
               poly(maxEducLevel, 2, raw = TRUE) + 
               clase, 
             data = train)
model9

coef (model9)
paste("Coef:", mean(train$log_salario_hora,na.rm=TRUE))
## aqui genero la prediccion de mis datos que entrene sobre el tes
test$model9<-predict(model9,newdata = test)

with(test,mean((log_salario_hora-model9)^2,na.rm=TRUE))

## MSE de 0,3625 es mas alto

##--------------   hhs --------##

model20 <- lm(log_salario_hora ~ poly(age, 3, raw = TRUE):
                poly(maxEducLevel, 2, raw = TRUE) + 
                poly(experiencia_ajustada,3,raw=TRUE) +
                sizeFirm + 
                regSalud + 
                clase, 
              data = train)
model20

coef (model20)
paste("Coef:", mean(train$log_salario_hora,na.rm=TRUE))
## aqui genero la prediccion de mis datos que entrene sobre el tes
test$model20<-predict(model20,newdata = test)

with(test,mean((log_salario_hora-model20)^2,na.rm=TRUE))

## Bajo a 0,3192 hasta el momento de los mejores


model21 <- lm(log_salario_hora ~ poly(age, 3, raw = TRUE) +
                poly(maxEducLevel, 2, raw = TRUE):
                poly(experiencia_ajustada,3,raw=TRUE) +
                sizeFirm + 
                regSalud + 
                formal +
                p6620 +
                clase, 
              data = train)
model21

coef (model21)
paste("Coef:", mean(train$log_salario_hora,na.rm=TRUE))
## aqui genero la prediccion de mis datos que entrene sobre el tes
test$model21<-predict(model21,newdata = test)

with(test,mean((log_salario_hora-model21)^2,na.rm=TRUE))

## Volvio a bajar a 0,3144

## ----------- ULTIMA----------##

model2 <- lm(log_salario_hora ~ poly(age, 3, raw = TRUE) +
               data = train)

model2 <- lm(log_salario_hora ~ poly(age, 3, raw = TRUE), data = train)

model2

coef (model22)
paste("Coef:", mean(train$log_salario_hora,na.rm=TRUE))
## aqui genero la prediccion de mis datos que entrene sobre el tes
test$model2<-predict(model2,newdata = test)

with(test,mean((log_salario_hora-model2)^2,na.rm=TRUE))

## MSE ALTO DE 0,49

## no es necesario controlar por dearatmento todos son de bogota.
##depto microEmpresa regSalud sizeFirm ( algunas variables de interes)

##------- JUNTAR TODO EN UNO--------#



mse2 <- with(test,mean((log_salario_hora-model4)^2,na.rm=TRUE))
mse4 <- with(test,mean((log_salario_hora-model4)^2,na.rm=TRUE))
mse9 <- with(test,mean((log_salario_hora-model9)^2,na.rm=TRUE))
mse20 <- with(test,mean((log_salario_hora-model20)^2,na.rm=TRUE))
mse21 <- with(test,mean((log_salario_hora-model21)^2,na.rm=TRUE))


#put them in a vector
mse<-c(mse2,mse4,mse9,mse20,mse21)

#create a data frame
db<-data.frame(model=factor(c("model2","model4","model9","model20","model21"),ordered=TRUE),
               MSE=mse)
db

## se esocge el modelo 21 (5) tiene el MSE MAS BAJO. (0,31447)
## el segundo modelo con el MSE mas bajo es el 20  (0,3192)




