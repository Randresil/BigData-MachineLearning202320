## Limpieza de los datos


df_filtered <- subset(df_final, age >= 18 & dsi == 0)
## dsi 0 es solo personas que no esten desempleadas

## Descargamos para hacer ggplot y estadisticas descriptivas
install.packages("tidyr")
installed.packages("ggplot2")
library(tidyr)
library(ggplot2)

 ## No se observa porque toca volver logaritmo los ingresos
  ggplot(data=df_filtered, aes(x=age, y= y_ingLab_m))+
  labs(x="edad", y="ingresos mensuales")

## Salario mensual y por hora
summary(df_filtered$age)
summary(df_filtered$y_ingLab_m_ha)
summary(df_filtered$y_ingLab_m)

df_filtered$log_salario_mensual <- log(df_filtered$y_ingLab_m)
df_filtered$log_salario_hora <- log(df_filtered$y_ingLab_m_ha)

# Crear un histograma of the salary distribution
hist(df_filtered$log_salario_mensual,
     main = " Monthly Salary Distribution",
     xlab = "Salary in logs",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")
## se observa mejor la distribucion de salario

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
        xlab = "sexo (1=hombre, 0=mujer"),
        ylab = "Income",
        col = c("lightblue", "lightgreen"))
## si les gusta se puede hacer asi respecto a informal y tambien respecto a clase (urbano y rural)







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
