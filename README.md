# project_regression
---
title: "Untitled"
author: "Sergeeva Anastasia"
date: "2023-09-09"
output:
  html_document: default
  pdf_document: default
  word_document: default
---
#файл 

library(ggplot2) 
library(car)
library(DescTools)
library(broom)
library(glmnet)
library(dplyr)
library(readxl)
library(tidyverse)
library(caTools)  # or library(caret)
library(GGally)
library(lmtest)
# разделяем выборку на тестовую и обучающую (70/20)
```{r}
z <- read_excel("cardio2.xlsx")
view(z)
set.seed(1821)
split <- sample.split(z$gluc, SplitRatio = 0.7)
train <- subset(z, split == TRUE)
test <- subset(z, split == FALSE)
train_y <- train$gluc
train_x <- select(train, -gluc) %>% data.matrix()
test_x <- select(test, -gluc) %>% data.matrix()
```

# модели линейной регрессии(МНК)
```{r}

m_lm <- lm(gluc ~ ., data = train)
summary(m_lm)
```

`

```{r}
m_lm1 <- lm(gluc ~ age+gender+weight+ap_hi+ap_lo+cholesterol+smoke+alco+active, data = train)
summary(m_lm1)
```
```{r}
m_lm2 <- lm(gluc ~ age+gender+weight+ap_hi+ap_lo+cholesterol+smoke+alco, data = train)
summary(m_lm2)
```

```{r}
m_lm3 <- lm(gluc ~ age+gender+weight+ap_hi+ap_lo+cholesterol+smoke, data = train)
summary(m_lm3)
```

```{r}
m_lm4 <- lm(gluc ~ age+gender+weight+ap_hi+ap_lo+cholesterol, data = train)
summary(m_lm4)
```
```{r}
m_lm5 <- lm(gluc ~ age+gender+weight+ap_hi+cholesterol, data = train)
summary(m_lm5)
```

```{r}
m_lm6 <- lm(gluc ~ age+gender+weight+cholesterol, data = train)
summary(m_lm6)
```
```{r}
m_lm7 <- lm(gluc ~ age+weight+cholesterol, data = train)
summary(m_lm7)
```
```{r}
m_lm8 <- lm(gluc ~ weight+cholesterol, data = train)
summary(m_lm8)
```

```{r}
test$predict <- predict(m_lm8, test)
ggplot(data = test, aes(x = gluc, y = predict)) +
geom_point() +geom_abline() + scale_x_continuous(limits = c(0, 5), expand = c(0, 0)) +
scale_y_continuous(limits = c(0, 5), expand = c(0, 0))
```

```{r}
# вычисление значений коэффициентов увеличения дисперсии = мультиколлинеарности нет
car::vif(m_lm8)

```

# некоторый диапазон значений λ
```{r}
lambdas <- seq(0, 2000, by = 0.025)
set.seed(1877)
cv_ridge <- cv.glmnet(train_x, train_y, alpha = 0, lambda = lambdas)
plot(cv_ridge)
```
cv_ridge$lambda.min
```{r}
# метод Ридж

set.seed(1877)
m_ridge <- glmnet(train_x, train_y, alpha = 0, lambda = cv_ridge$lambda.min)
coef(m_ridge)
```
```{r}
#прогноз на тестовой выборке
test$ridge<-predict(m_ridge,s=cv_ridge$lambda.min,newx=test_x)
View(test)
```

```{r}
#Метод Лассо
set.seed(1886)
cv_lasso<-cv.glmnet(train_x,train_y,alpha=1,lambda = lambdas)
plot(cv_lasso)
#значения лямбда исходя из минимума ошибок

```
cv_lasso$lambda.min


```{r}
#модель_линеной_регресии_метод_Лассо 
set.seed(1886)
m_lasso<-glmnet(train_x,train_y,alpha =1,lambda = cv_lasso$lambda.min )
coef(m_lasso)
```
```{r}
#прогноз полученной модели на тестовой выборке
test$lasso<-predict(m_lasso,s=cv_lasso$lambda.min,newx=test_x)
```

```{r}
#метрики для моделей с использованием различных методов для оценки параметров
mape1 <- MAPE(test$predict, test$gluc)
rmse1 <- RMSE( test$predict,  test$gluc)
mae1 <- MAE (test$predict,  test$gluc)
mape2 <- MAPE( test$ridge,  test$gluc)
rmse2 <- RMSE( test$ridge,  test$gluc)
mae2 <- MAE( test$ridge,  test$gluc)
mape3 <- MAPE( test$lasso,  test$gluc)
rmse3 <- RMSE( test$lasso, test$gluc)
mae3 <- MAE(test$lasso, test$gluc)
MNK<-c(mape1, rmse1, mae1)
Ridge<-c(mape2, rmse2, mae2)
Lasso<-c(mape3, rmse3, mae3)
res_norm<-cbind(MNK,Ridge,Lasso)
rownames(res_norm)<-c('MAPE','RMSE','MAE')
res_norm

```

```{r}
mn_summary = summary(m_lm8)
mn_summary
```

##Оценка качества подгонки
```{r}
summary(lm(data = train, gluc ~ weight + cholesterol))$r.squared 
summary(lm(data = train, gluc ~ weight + cholesterol))$adj.r.squared
##Чем ближе коэффициент детерминации к единице, тем наблюдаемые значения теснее примыкают к линии регрессии, а уравнение регрессии лучше описывает зависимость переменных
summary(lm(data = train, gluc ~ weight + cholesterol))$sigma^2
##При нулевой случайной(остаточной) дисперсии наблюдается функциональная зависимость между величинами (каждому значению одной величины соответствует единственное значение другой).
sigma(m_lm8)*100/mean(z$gluc)
```
##F-критерий и t-критерий. 
```{r}
qf(.95, 2, 4546)
qf(.99, 3, 4546)
qt(0.975, 4546)
qt(0.995, 4546)
plot(m_lm8$fitted.values, m_lm8$residuals)
qplot(y = m_lm8$fitted.values, x = m_lm8$residuals)
```

##Проверка условия М(Еi)= 0
```{r}
mean(m_lm8$residuals)
a<-mean(m_lm8$residuals) 
b<-sd(m_lm8$residuals, na.rm = FALSE) 
n <- sqrt(6499) 
tm <- a/b*n
str(tm)
qt(0.975,6498) ##df=6499-1

```
##Проверка условия cov(ei, ej) = 0.
```{r}
bgtest(m_lm8)
dwtest(m_lm8)
```
##Проверка условия согласуются ли остатки регрессии с нормальным законом
```{r}
qqnorm(m_lm8$residuals)
library(sm)
Z<- m_lm8$residuals
hist(Z)
sm.density(m_lm8$residuals, model = "Normal",xlab = "Resudual", ylab = "Функция плотности распределения", xlim=c(0,20000000))
```

##параметрические тесты
```{r}
library(nortest) 
lillie.test(m_lm8$residuals) 
sh2 <- m_lm8$residuals[1:1000]
shapiro.test(sh2)

```
