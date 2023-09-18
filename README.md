
---
 # Problem Set #1: Predicting Income

## Elaborado por: Ricardo Silva, Fedrico Camacho, Andrés Chavarro & Juan David Vélez P.

## date: "2023-09-18"
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
stargazer(df_filtered, title = "EstadC-sticas Descriptivas",
          type = "text", digits = 2, covariate.labels = c("Directorio", "Secuencia-p", "Orden", "Clase", "Dominio", 
                                                  "Mes", "Estrato1", "Sexo", "Edad", "Ingresos laborales Hora", "Ingresos laborales Mensual", 
                                                  "Ingreso total Mensual", "Ingreso total Hora", "College", "Depto", "Empleado", "Formal", "EducaciC3n mC!xima", 
                                                  "Parentesco Jefe Hogar", "Tiempo trabajando", "Seguridad Salud", "TamaC1o Firma", "Total horas trabajadas",
                                                  "Micro Empresa", "Pesos frecuencia", "Otros ingresos", "Pensiones", "NC:mero personas en Firma", "OcupaciC3n"))


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

Al estimar la regresión que incluye únicamente las variables de edad y edad al cuadrado, nuestro objetivo es validar la hipótesis relacionada con la forma funcional y especificación adecuada del modelo. Si el estimador (beta) asociado al término que contiene la edad al cuadrado es negativo, esto indica una relación cóncava entre el ingreso y la edad. Este hallazgo respalda la hipótesis de que el ingreso por edad alcanza un punto máximo y, a partir de ahí, disminuye.
Al correr el modelo, observamos un primer estimador con un signo positivo, lo que sugiere un efecto de aumento del salario por hora en función de la edad. Específicamente, un incremento de un año en la edad conlleva un aumento positivo del 6,702% en el salario por hora en términos logarítmicos. También podemos interpretar que hay aumentos salariales cercanos al 70% por cada década adicional de edad.
Por otro lado, el signo negativo del segundo estimador, que explora la relación cuadrática, nos indica la presencia de concavidad en la relación. El valor de este estimador es -0,0007394, ilustrando que, al ser la relación cuadrática y penalizar los valores altos de edad, cada incremento en edad se traduce en aumentos salariales menores debido a esta penalización inherente a la naturaleza cuadrática de la relación.

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

#### *Modelo no condicional Vs Modelo condicional*

                *Dependent variable:*
                log_salario_hora
|         |   Modelo Incondicional | Modelo Condicional |
|---------------|:-------------:|---------:|
|    Mujer      | 	-0.045***   | -0.020** |
|               |    (0.015)    | (0.009)  |
|---------------|:-------------:|---------:|
| Ovservations  |     9,892     |  16,542  |
|     R2        |     0.001     |  0.324   |
|  Ajusted  R2  |     0.001     |  0.324   |
|   Residual Std. Error |   0.727 (df = 9890) |0.463 (df = 16450)|
|  F Statistic  |    9.317*** (df = 1; 9890)    | 87.960*** (df = 91; 16450)   |
          Note:	*p<0.1; **p<0.05; ***p<0.01

### Interpretación
Realizaremos una comparación de los salarios sin controles, otras estimaciones con controles, con el objetivo de clarificar si esta brecha se genera a raíz de la diferenciación entre trabajadores, y cómo última cuestión compararemos la edad “peak” de los hombres con el de las mujeres.

Como una primera aproximación ante el problema se realizó la siguiente regresión *log⁡(salario) = β_(0 )+ β_(1 ) Female + *u; en esta podemos ver que se da un coeficiente de – 4%, pensando en que nuestra variable base es hombre tenemos que en promedio las mujeres están recibiendo 4% menos salario que los hombres. 
 