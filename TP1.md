# TP1

## 0. Seteo de la herramienta

Seteo de paquetes y de opciones para que en los gráficos no se disponga la notación científica.

```{r setup}
library("tidyverse")
library("GGally")
options(scipen = 50, digits = 10)
```

## 1. Preparacion de los datos

### 1.a Selección del dataset

```{r}
Data <- read.csv(file="C:\\Users\\l0339237\\Desktop\\ar_properties.csv", header=TRUE, sep=",")
str(Data)
```

### 1.b Filtro de registros

Los filtros podrían haberse unificado en el mismo código, pero para mantener el orden y asegurarse el cumplimiento de cada condición, se separan en cláusulas distintas

#### b. i. Pertenecen a Argentina y Capital Federal

```{r}
Data = Data %>% filter(l1 == 'Argentina' & l2 == 'Capital Federal')
```

#### b. ii. Cuyo precio esta en dolares (USD)

```{r}
Data = Data %>% filter(currency == 'USD')
```

#### b. iii. El tipo de propiedad sea: Departamento, PH o Casa

```{r}
Data = Data %>% filter(property_type == "Departamento"|property_type == "PH"|property_type == "Casa")
```

#### b. iv. El tipo de operacion sea Venta

```{r}
Data = Data %>% filter(operation_type == 'Venta')
```

### c. Filtro de columnas

```{r}
Data = Data %>% select(id,l3,rooms,bedrooms,bathrooms,surface_total,surface_covered,price,property_type)

dim(Data) # Verifico llegar a un dataset con 61905 observaciones y 9 variables

```

## 2. Analisis exploratorios (I)

### a. Obtener la cantidad de valores unicos y de valores faltantes (NAs) para cada una de estas variables

#### Valores únicos

```{r}
sapply(Data, function(x) length(unique(x)))
```

```{r}
sapply(Data, function(x) length(unique(x)))
```

#### Valores faltantes

```{r}
for (i in 1:9){
  print(paste0("NAs en ", colnames(Data[i]), ": ", (sum(is.na(Data[i])))))
  }
```

Los valores únicos de la variable ID coinciden con la cantidad de filas del data frame, lo que da cuenta de que el id puede funcionar como clave primaria de la tabla. Se puede ver que al no existir nulls en price y en property_type, además, todas las propiedades tienen registro de su id, price y property_type. Por otra parte, la variable bedrooms tiene una gran cantidad de observaciones faltantes. La abundancia de valores nulos en la variable bedrooms indica que la variable puede no ser útil, y al existir otras dos variables con menos valores nulos (rooms y bathrooms) es posible que estas puedan funcionar en su lugar.

### b. Obtener la matriz de correlacion para las variables numericas.

```{r}
round(cor(Data[3:8], use="complete.obs", method="pearson"),4)
```

Por los niveles de las correlaciones, pueden advertirse tres niveles diferentes de correlación, lo que redunda en tres formas en las que estas variables se asocian entre sí: de forma nula o muy debil en el caso de surface_total o surface_covered con todo el resto de las variables, incluyendo en forma llamativa el precio; de forma media en el caso de las variables de los ambientes de la casa (ambientes, habitaciones, baños) con el precio; y de forma importante en el caso de bedrooms y rooms. Con esto se verifica la sospecha del punto anterior: es posible usar rooms para explicar bedrooms.

## 3. Preparacion de los datos (II)

#### Elimino variable bedrooms por presentar una alta proporción de nulos, y por estar contenida en rooms

```{r}
Data = subset(Data, select = -c(4))
```

#### Elimino registros con valores faltantes

```{r}
Data = na.omit(Data)

dim(Data) # Verifico llegar a un dataset con 51210 observaciones y 8 variables
```

## 4. Analisis exploratorios (II)

### a. Obtener estadísticas de la variable precio y realizar un histograma por tipo de propiedad.

#### Resumen de la variable precio

```{r}
summary(Data$price)
```

Rapidamente se visualizan dos elementos propios de las variables expresadas en dinero o en valor monetario: como están acotadas por abajo (nunca pueden ser menores a 0) pero no por arriba, los valores extremos hacia arriba generan efectos sobre las medidas que no consideran este efecto. La manifestación más típica de esto está en que la media excede mucho a la mediana, que se vuelve un buen reflejo del valor típico de las viviendas, en este caso.

#### Histograma de la variable precio, fijando ejes para hacerlo útil a la interpretación

```{r}
hist(Data$price,
           main="Histograma de la variable precio",
           xlab="Precio",
           ylab="Frecuencia",
           xlim=c(6000,1500000),
           col="chocolate",
           border="brown",
           breaks=500
      )
```

El histograma refleja lo dicho en el punto anterior: los valores típicos están muy a la izquierda, por lo que los valores de la derecha (el gráfico está acotado) resultan outliers positivos, sin que existan equivalentes negativos. Esto hace que la media esté en torno a los 250K, lo que tiene una frecuencia de 1000 (la mediana en cambio estaría en 17000 con una frecuencia casi de 3000).

### b. Obtener estadísticas de la variable precio y realizar un histograma por tipo de propiedad.

```{r}
Data%>%group_by(property_type)%>%count()%>%arrange(n)
```

En primer lugar se verifica una diferencia en la cantidad de valores en cada grupo: hay muchos más departamentos que Casas o PHs.

```{r}
Data$property_type = as.character(Data$property_type) # Convierto a character para que no considere grupos innecesarios
tapply(Data$price, Data$property_type, summary)
```

La diferencia explicada entre media y mediana, producto de la presencia de valores atípicos hacia arriba, se visualiza fuertemente en casas y en departamentos, pero no en PHs. Esto hace que la media sea bastante parecida a la mediana (solo un 10% superior) en este caso. Además sucede que el primer cuartil de los PHs está por arriba que su equivalente en el Departamento, pese a que la media está por debajo: los valores bajos del PH no son tan bajos como los del departamento. Respecto del valor típico, la mediana, el valor típico es prácticamente el doble en casas que en departamentos.

### c. Realizar un grafico de boxplot de la variable precio por tipo de propiedad.

```{r}
boxplot(price~property_type,data=Data,main="Precio por tipo de propiedad",
  xlab="Tipo de propiedad", ylab="Precios",
  col=(c("gold")))
```

Se ven muchos datos atípicos en los extremos superiores, mayores a la distancia equivalente al 1.5 Q3. Esto induce la idea de que los outliers vistos de esa forma no sean una correcta medida de los valores atípicos. Se verifica también, de otra forma, la cola a derecha del histograma anterior.

### d. Realizar un correlagrama usando GGAlly.

```{r}
ggpairs(Data[3:8], title="Correlograma", mapping = ggplot2::aes(colour= Data$property_type)) 
```

La tabla inferior de la derecha deja visualizar la diferencia entre las propiedades por tipo, mucho más concentradas en los departamentos.

## 5. Remover outliers.

Criterio utilizado: se remueven valores que se encuentran a más de 1.5 distancias intercuartiles del Q1 (hacia abajo) y del Q3 (hacia arriba)

```{r}
DataSinOutliers = Data%>%
  filter(price<=(quantile(Data$price,0.75)+IQR(Data$price)*1.5)&price>=(quantile(Data$price,0.25)-IQR(Data$price)*1.5))

dim(DataSinOutliers)
```

Se removieron 4.258 observaciones.

## 6. Repito análisis pero para dataset sin outliers.

Para identificar outliers, se realiza un scatterplot para entender en dónde la variable crece

```{r}
sort(Data$price, decreasing = FALSE) %>% 
  plot(main = "Scatterplot de precios",
       xlab = "Índice",
       ylab = "Precios")
```

Se verifica que el crecimiento es desde el índice 50.000 (precios 2.500.000), por lo que se realizará un recorte en ese nivel.


```{r}
DataSinOutliers =
  Data %>%
  filter(price < 2500000)
```

Criterio utilizado: se remueven valores que se encuentran a más de 1.5 distancias intercuartiles del Q1 (hacia abajo) y del Q3 (hacia arriba)

```{r}
summary(DataSinOutliers$price)
hist(DataSinOutliers$price,
     main="Histograma de la variable precio - Sin outliers",
     xlab="Precio",
     ylab='Frecuencia',
     xlim=c(6000,500000),
     col="chocolate",
     border="brown",
     breaks=500
)
tapply(DataSinOutliers$price, DataSinOutliers$property_type, summary)
boxplot(price~property_type,data=DataSinOutliers,main="Precio por tipo de propiedad",
        xlab="Tipo de propiedad", ylab="Cantidad de propiedades",
        col=(c("gold")))
ggpairs(DataSinOutliers[3:8], title="Correlograma", mapping = ggplot2::aes(colour= DataSinOutliers$property_type)) 
```

Al volver a realizar los análisis exploratorios, se encuentra que el boxplot resulta más rico en la medida que los outliers ocupaban una proporción importante. Los valores del eje x en el histograma también deben acomodarse, y la asimetría a la izquierda se mantiene pese a que se vuelve algo menos intensa. Como era de esperarse, la diferencia entre la media y la mediana se redujo con fuerza, logrando una medida de posición definitiva de los datos en torno a los 17K.

## 7. Modelo lineal

### a. Realizar un modelo lineal simple para explicar el precio en función de las habitaciones y otro modelo que explique el precio en función de la superficie total.

```{r}
ML_Room <- lm(price~rooms, data = DataSinOutliers)
ML_Surface <- lm(price~surface_total, data = DataSinOutliers)
```

### b. Usar la función summary() para obtener informacion de ambos modelos. Explicar los valores de los coeficientes estimados.

```{r}
summary(ML_Room)
summary(ML_Surface)
```

La predicción del valor de los inmuebles crece en 89K por cada habitación que se agrega, y en el caso (teórico) de inmueble sin habitaciones tendríamos un valor predicho negativo. En el segundo modelo el intercepto es de 236K, y la predicción del valor aumenta 16K por cada metro cuadrado de superficie. El primer modelo recoge mucha más variabilidad (39%) que el segundo (0.2%).

### c. ¿Cuál modelo usarían para predecir el precio? ¿Por qué?

Entre los dos modelos, la diferencia en la variabilidad explicada es mucho mayor en el primer caso que en el segundo, lo que lo vuelve un mejor modelo para explicar el precio.
