# TP3

## Preparación de los datos y análisis descriptivo

### Paquetes

```{r setup}
library(purrr)
library(e1071)
library(tidyverse)
library(funModeling)
library(GGally)
library(caret)
library(broom)
library(modelr)
library(pROC)
```

### Leer el archivo y mostrar su estructura

```{r}
titanic = read.csv("C:/Users/Federico/Downloads/titanic_complete_train.csv")
status(titanic)
```

Rápidamente vemos la estructura de la tabla y de algunas de sus columnas: ID está bien construida (no hay ceros ni NAs, y tantos únicos como observaciones), lo mismo en nombres, en sexo y en el target (sobrevivió o no) dos únicos. En las variables que solo algunos de los pasajeros tenían (hermanos, pareja) hay muchos ceros, y en la cabina hay muchos NAs, lo que da a entender que de muchos pasajeros no se conoce la cabina.


### Selección y transformación de variables

You can also embed plots, for example:

```{r}
titanic = titanic %>%
  select(PassengerId, Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked) %>%
  mutate(
    Survived = as.factor(Survived),
    Pclass = as.factor(Pclass),
    Embarked = as.factor(Embarked)
    )
```

La condición de supervivencia o no, igual que la clase y la de embarcar o no, se interpretaban como números contínuos y en realidad eran variables categóricas. De esta forma se asigna su correcta función.


### Realizar un gráfico de ggpairs.

```{r}
titanic %>%
  select(Survived, Pclass, Sex, Age, Fare) %>%
  ggpairs(., mapping = aes(color = Survived)) + 
  theme_bw() +
  labs(title = "Relación entre variables")
```

La mayor parte de los pasajeros no sobrevive, variable target que correlaciona distinto con cada variable: en las clases es muy dependiente en la medida que la mayoría de los que sobrevive está en la misma clase (tercera), cosa que de todas formas mantiene una tendencia con la población total que está más en la tercera. En cuanto al sexo ocurre algo similar: la mayor parte de los que sobreviven son mujeres, pero también la mayor parte de los pasajeros totales son mujeres. Las otras dos variables (edad y tarifa) no parecen tener demasiada relación con la supervivencia o no. Las variables explicativas, además, en algunos casos aportan información por su asociación, pero es información de carácter descriptivo y, en ocasiones, algo redundante: la mayor parte de los pasajeros son mujeres de entre 20 y 40 años, y la mayor parte de las tarifas bajas se asocian a una clase baja en el barco.


### Distribución de clases

```{r}
titanic %>%
  group_by(Survived) %>%
  summarise(count = n(), perc = round(n() / nrow(titanic) *100,2))

ggplot(data = titanic, aes(x = Survived, fill = Survived)) +
  geom_bar() +
  labs(title = "Distribución de los sobrevivientes")
```

Como se dijo, la mayor parte de las personas no sobreviven. La proporción es de 61.6 a 38.4, y las odds son 1.60 aproximadamente: por cada persona que no muere hay 1.60 que sí lo hacen.

### Partir el dataset

```{r}
idx = createDataPartition(titanic$Survived, p = .70, list = FALSE, times=1) # Con esto aseguro de mantener las proporciones del target
head(idx)
train = titanic[idx,]
validation  = titanic[-idx,]
```

## Modelos y predicción

### Modelo inicial

```{r}
modelo = glm(Survived ~ Pclass + Sex + Age, family = 'binomial', data = titanic)
tidy(modelo)
```

Al ser basales las variables clase y género, la ausencia de activación de variables indicará que el caso del intercepto es un caso de una mujer (sin Sexmale activo) de primera clase (sin PClass2 ni PClass3 activo). Sin embargo, al incluir la variable edad ese caso será el de una mujer de 0 años en primera clase. Los casos en los que se activan las otras variables (es segunda o tercera clase en vez de primera, es hombre en vez de mujer) disminuyen la probabilidad de supervivencia. El hecho de tener más años también lo hace. En todos los casos se trata de coeficiente significativos. Con respecto a la edad, una salvedad: será de esperar que la supervivencia según la edad no sea una relación lineal, ya que podrán sobrevivir más los niños pero también los ancianos. Entonces, un modelo incorporando la edad como coeficiente cuadrático tendría que tener, para esa variable, el signo opuesto, positivo:

```{r}
modelo2 = glm(Survived ~ Pclass + Sex + Age + I(Age^2), family = 'binomial', data = titanic)
tidy(modelo2)
```

Se ve que la tendencia existe aunque muy poco significativa.

### Predicción Rose y Jack

```{r}
roseYJack = data.frame(
  Pclass = as.factor(c(1, 3)),
  Sex = c('female', 'male'),
  Age = c(17.0, 20.0)
)

predict(modelo, newdata = roseYJack, type = 'response') # Mediante el tipo 'response' evitamos las log-odds como salida y lo convertimos a probabilidad.
```

De esta manera, la probabilidad de supervivencia de Rose es mucho mayor que la de Jack.

### Comparación de modelos

Se realizan cuatro modelos, además del modelo original (1) para analizar la posible unidimensionalidad del problema, es decir para analizar cuánto incorporan los nuevos coeficientes: dos univariados (que miran únicamente Clase y Sexo) y dos multivariados (uno que mira clase+sexo, y otro que mira todas las variables)

```{r}
formulas = formulas(.response = ~Survived,
                           modelo1 = ~ Pclass + Sex + Age,
                           modeloClasista = ~Pclass,
                           modeloSexista = ~Sex,
                           modeloSexistayClasista = ~Pclass + Sex,
                           modeloMultivariado = ~Pclass + Sex + Age + Parch + SibSp + Embarked + Fare)

models = tibble(formulas) %>%
  mutate(models = names(formulas), expression = paste(formulas),
         mod = map(formulas, ~glm(.,family = 'binomial', data = titanic)))

models %>%
  mutate(tidy = map(mod, tidy)) %>%
  unnest(tidy) %>%
  mutate(estimate = round(estimate, 5), p.value = round(p.value, 4)) %>%
  select(models, term, estimate, std.error, statistic, p.value)
```

Las variables que se buscaron analizar (sexo y edad) no tienen dos problemas que podrían tener: posibles signos contrario por omisión de variables relevantes, ni colinealidad. Esto está explicado porque la incorporación de otras variables (la complementaria entre sexo y edad en el SyC, y todo el resto en el multivariado) no hacen que el signo de ninguno de los coeficientes cambie, es decir, pertenecer a clases inferiores sigue reduciendo las chances de supervivencia, al igual que ser hombre. Respecto de la multicolinealidad, ni siquiera la incorporación de muchas variables hace que sus p-valores pasen a evidenciar no significatividad, cosa que sí ocurre en las variables Parch y Fare, por ejemplo.


```{r}
models = models %>% 
  mutate(glance = map(mod,glance))

models %>%
  unnest(glance) %>%
  mutate(percExplainedDev = 1-deviance/null.deviance) %>%
  select(models, percExplainedDev, null.deviance, deviance, logLik, expression) %>%
  arrange(deviance)
```

El modelo que mira todas las variables es el que minimiza el deviance (maximiza la proporción explicada). La remoción de variables va reduciendo, pero es de notar que reducir todas las variables menos sexo y edad (modelo clasista y sexista) conserva el 88% de la deviance que explica el modelo en sí. Se selecciona el modelo que minimiza el deviance para los siguientes puntos.

### Evaluación y curva ROC

```{r}
Eval = (
  models %>%
    filter(models == 'modelo1') %>%
    mutate(pred = map(mod, augment, type.predict = 'response')) %>%
    select(pred)
)[[1]][[1]] 

rocAll = roc(response = Eval$Survived, predictor = Eval$.fitted)

ggroc(list(all = rocAll), size = 1) +
  geom_abline(slope = 1, intercept = 1, linetype='dashed') +
  theme_bw() + labs(title='Curva ROC en entrenamiento', color='Modelo')

rocAll$auc
```

Cuando la especificidad es alta (eje de las X cercano a 1) muchos de los no sobrevivientes serán clasificados como tales, pero en el gráfico se ve que eso será a costa de una sensitividad muy baja, es decir una proporción baja de sobrevivientes clasificados como tales. Conceptualmente las situaciones son combinaciones entre estos dos 'costos-beneficios', pero un modelo que tiene una curva cercana al extremo superior izquierdo es una buena, porque es capaz de tener una alta especificidad sin perder sensitividad. Este es el caso de nuestro modelo, que llega a 0.84 de área bajo la curva.

```{r}
ggplot(Eval, aes(x = Survived, y = .fitted, group = Survived, fill = Survived)) +
  geom_violin() +
  theme_bw() +
  guides(fill = FALSE) +
  labs(title = 'Violin plot', y = 'Predicted probability')
```

De esta manera se visualiza que el modelo funciona bastante bien para predecir los que no sobrevivieron (con un corte de 0.25 se predice a la mayoría), pero no es tan bueno para predecir a los que sí sobreviven: excepto prediciendo que todo el mundo sobrevive, asumiremos el costo de predecir como fallecidos a muchos que sobreviven.

### Gráfico de métricas de performance

Se introduce el significado de cada métrica, por medio de la pregunta que responde:

- Accuracy: Cuántos precedidos fueron correctos?
- Precision: Cuántos de los que predijimos como fallecidos lo eran?
- Sensivity: De los fallecidos, a cuántos predijimos bien?
- Especificidad: De los que no son fallecidos, a cuántos predijimos bien?
El score F1 es la media entre la precisión y sensitividad

```{r}
models_val = models %>%
  filter(models == 'modeloMultivariado') %>%
  mutate(pred = map(mod, augment, newdata = validation, type.predict = "response"))

prediction_validation_total =  models_val %>%
  filter(models == "modeloMultivariado") %>%
  unnest(pred, .drop = TRUE)

prediction_metrics =
  function(cutoff, predictions = prediction_validation_total) {
    table_res = predictions %>%
      mutate(
        predicted_class = if_else(.fitted > cutoff, 1, 0) %>% as.factor(),
        Survived = factor(Survived)
      )
    confusionMatrix(table_res$predicted_class, table_res$Survived, positive = "1") %>%
      tidy() %>%
      select(term, estimate) %>%
      filter(term %in% c('accuracy', 'sensitivity', 'specificity',
                         'precision', 'f1')) %>%
      mutate(cutoff = cutoff)
  }

cutoffs = seq(
  min(prediction_validation_total$.fitted),
  max(prediction_validation_total$.fitted),
  0.001
) # Vector de puntos de corte

logit_pred = map_dfr(cutoffs, prediction_metrics) %>% mutate(term = as.factor(term))

logit_pred %>%
  ggplot(., aes(cutoff, estimate, group = term, color = term)) +
  geom_line(size = 1) +
  labs(title = 'Evaluación conjunta de variables',
       color = "") + 
  scale_x_continuous(breaks = round(seq(0, 1, by = 0.1), 2)) +
  scale_y_continuous(breaks = round(seq(0, 1, by = 0.1), 2))+
  theme(legend.position = "bottom")
```

Conceptualmente, cuando el punto de corte es más cercano a 0 estamos considerando que todos son sobrevivientes, mientras que cuando es más cercano a 1 diremos que nadie sobrevivirá. A medida que el cutoff va aumentando, crece el accuracy, la precisión y la specificity. Es lógico que esto suceda, ya que crecen al predecir más fallecidos: la única que podría no crecer es la precision, porque al predecir fallecidos mal decrece (solo decrece muy cerca del 1, así que ese no parece ser un problema de este modelo). La que sí decrece en forma monótona es la especificidad, que inversamente a la sensitividad mira de los no fallecidos cuántos se clasificaron bien. 

Uno podría considerar que el objetivo de este problema es la maximización del accuracy (0.6), dado que ambas malas predicciones podrían ser igual de complejas.


```{r}
logit_pred %>%
  filter(term == "accuracy") %>%
  arrange(desc(estimate))

best_accuracy = logit_pred %>%
  filter(term == "accuracy") %>%
  arrange(desc(estimate)) %>%
  head(1)

sel_cutoff = best_accuracy$cutoff
sel_accuracy = best_accuracy$estimate

table = prediction_validation_total %>% 
    mutate(predicted_class=if_else(.fitted>sel_cutoff, 1, 0) %>% as.factor(),
           Survived= factor(Survived))

confusionMatrix(table(table$Survived, table$predicted_class), positive = "1")
```

Así se ve que el modelo clasificó correctamente a 71 pasajeros como víctimas fatales y a 148 como sobrevivientes, mientras que clasificó mal a 47 en total, entre los dos grupos.

## Test fuera de la muestra

```{r}
titanic_test = read.csv("C:/Users/Federico/Downloads/titanic_complete_test (1).csv")
titanic_test = titanic_test %>%
  select(PassengerId, Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked) %>%
  mutate(
    Survived = as.factor(Survived),
    Pclass = as.factor(Pclass),
    Embarked = as.factor(Embarked)
    )

titanic_test$predict = predict(modelo, titanic_test, type="response")
table1 = titanic_test %>% 
    mutate(predicted_class=if_else(predict>sel_cutoff, 1, 0) %>% as.factor(),
           Survived= factor(Survived))
confusionMatrix(table(table1$Survived, table1$predicted_class), positive = "1")
```

Fuera de la muestra, el modelo predijo con exactitud a 326 personas, 220 por la supervivencia y 106 por la no supervivencia. En cambio, clasificó mal a 92. La accuracy, métrica que estabamos usando como indicador de la evaluación, bajo de 0.82 a 0.77.
