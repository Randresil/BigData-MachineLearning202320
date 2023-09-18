
---
 'Problem Set #1'
author: 'Elaborado por: Ricardo Silva, Fedrico Camacho, Andrés Chavarro & Juan David
  Vélez P.'
date: "2023-09-18"
output:
  html_document: default
  word_document: default
  pdf_document: default
subtitle: Predicting income
---

## Datos empleados

*"Medición de Pobreza Monetaria y Desigualdad Report"* que toma datos de la GEIH.

## Introducción

Para analizar los factores que influencian los salarios por hora de los individuos en Colombia, se utilizó la Gran Encuesta Integrada de Hogares (GEIH). Se enfocó en variables estrechamente relacionadas con el poder adquisitivo de los hogares, buscando patrones de comportamiento en dichas variables.

La base de datos contiene una amplia gama de información sobre ingresos extras percibidos por el hogar, ya sea por arriendo, remesas, subsidios o programas de ayuda. También aborda preguntas sobre el empleo, horas extras, disposición a trabajar más o menos, entre otras. Al finas se escogieron **18** de las 180 variables disponibles. *Nota:* Preferimos trabajar con variables que fueran dummies o que tuvieran un alto nivel de observaciones completas para asegurar la robustez de nuestros modelos.

## Obtención de los datos

-   Los datos fueron obtenidos mediante Web-scraping, particularmente por medio de un loop que recopila la información de los 10 *data chunks*.

-   Posteriormente, se procedió a hacer la respectiva limpieza de datos, debido a que multiples variables presentan datos faltantes, problemas de medición o se encuentran correlacionadas. Por tanto, nos quedamos con **18** de las 180 variables disponible. Entre estas se encuentran: educación, edad, género, condiciones laborales e ingresos (trabajados en forma logarítmica y por hora).

### Estadisticas Descritivas

```{r, echo=FALSE, results = 'hide'}
df_final <- read.csv('database.csv')

# Valores de dsi:	=1 if unemployed; =0 otherwise
c("dsi", "age") %in% names(df_final)
df_filtered <- subset(df_final, age >= 18 & dsi == 0)

options(repos = c(CRAN = "https://cran.rstudio.com"))
install.packages("pacman")

library(pacman)
p_load(stargazer, tidyverse, ggplot2, tinytex)


df_filtered <- df_filtered %>% select(c("directorio", "secuencia_p", "orden", "clase", "dominio", "mes", "estrato1", "sex", "age", "y_ingLab_m_ha", "y_ingLab_m", "y_total_m","y_total_m_ha", "college", "college", "depto", "dsi", "formal", "maxEducLevel","p6050", "p6426", "regSalud", "sizeFirm", "totalHoursWorked","microEmpresa", "fweight", "p6620", "cotPension") )  
```

```{r, echo=FALSE}
stargazer(df_filtered, title = "Estadísticas Descriptivas",
          type = "text", digits = 2)

```

## Relación *Wage-Age*

Buscando replicar lo que sugiere la disciplina economica, el momento en el que mayor ingreso se perciven en promedio, es sobre los 50 años. Hemos corrido el siguiente modelo:

#### *log(w) = β1 + β2Age + β3Age2 + u*

```{r, echo=FALSE, results = 'hide'}
df_filtered$log_salario_hora <- log(df_filtered$y_ingLab_m_ha)
df_filtered$age2 <- df_filtered$age^2
model1 <- lm(log_salario_hora ~ age + age2 , data=df_filtered)
summary (model1)
```

```{r, echo=FALSE}
stargazer(model1,type="text")
```

### Interpretación

\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_-\_---

```{r, echo=FALSE}
ggplot(df_filtered, aes(x = age2, y = log_salario_hora)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(
    title = "Wage Vs Age^2",
    x = "Edad al Cuadrado",
    y = "Ingreso"
  )
```

## Relación *The gender earning GAP*

Con la intención de analizar la brecha salarial condicional al genero se estimó la siguiente regresión, donde *Female* es una dummy igual a 1 si el individuo es mujer.

#### *log(w) = β1 + β2Female + u*

```{r, echo=FALSE, results = 'hide'}
 df_filtered$female <- 1 - df_filtered$sex
regmujer<- lm(log_salario_hora ~ female, data=df_filtered)
```

```{r, echo=FALSE}
stargazer(regmujer,type="text")
```
