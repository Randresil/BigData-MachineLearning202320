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



#==================================#
#### [3.] Limpieza de los datos ####  
#==================================#
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


# Variables de salario disponibles:
# ingtot -> ingreso total
# ingtotes -> Ingreso total imputado
# ingtotob -> Ingreso total observado
# y_ingLab_m -> Labor income salaried - Nominal Monthly
# y_ingLab_m_ha -> Labor income salaried - Monthly nominal
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

##
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
##
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



##________  3- Age-wage profile-_________

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




##---------  Train and sample (validation) Punto 5 ------------##

## Como en clase
sample <- sample(c(TRUE, FALSE), nrow(df_filtered), replace=TRUE, prob=c(0.7,0.3))
head(sample)

train  <- completa[sample, ] #train sample those that are TRUE in the sample index
test   <- completa[!sample, ] #test sample those that are FALSE in the sample index
dim(train)


reg1<-lm(log_salario_hora ~ age + age2 + age3 ,data=train)
summary(reg1)

coef (reg1)

paste("Coef:", mean(train$log_salario_hora,na.rm=TRUE))

test$reg1<-predict(reg1,newdata = test)

with(test,mean((log_salario_hora-reg1)^2,na.rm=TRUE))
## sale el MSE queremos que sea lo mas bajo posible. Intentamos modelos mas complejos


model4<-lm(log_salario_hora~poly(age,8,raw=TRUE):poly(maxEducLevel,3,raw=TRUE)
           + poly(fweight,3,raw=TRUE),data=train)
model4

coef (model4)
paste("Coef:", mean(train$log_salario_hora,na.rm=TRUE))
## aqui genero la prediccion de mis datos que entrene sobre el tes
test$model4<-predict(model4,newdata = test)

with(test,mean((log_salario_hora-model4)^2,na.rm=TRUE))

## aqui el MSE bajo bastante mas. Sigue siendo alto

## intentar con distintas especificaciones, polinomios, interacciones y variables







