install.packages("glmnet")
library(glmnet)
Vessel_X <- read.csv("Vessel_X.txt", header=FALSE)
Vessel_Y <- read.csv("Vessel_Y_Variables_quimicas.txt", header=FALSE)


# Detectando valores nulos
sum(is.na(Vessel_X))
sum(is.na(Vessel_Y))
Vessel__X<- cbind(Vessel_Y[11],Vessel_X)
colnames(Vessel__X)[1] <- "compuesto_11"


#Dividimos los datos de entrenamiento y test
#datos de test no son tomados en el ajuste del mejor modelo sino
#hasta la evaluacion final
set.seed(1)
train <- sample(1:nrow(Vessel__X),nrow(Vessel__X)*2/3)
datos.train <- Vessel__X[train,]
test <- -train
datos.test <- Vessel__X[test,]


#Convertimos los datos a matriz modelo (aqui por defecto se normalizan
#los datos) los datos de entrenamiento y test
datos.train.mat <- model.matrix(compuesto_11~., data = datos.train)[,-1]
datos.test.mat <-  model.matrix(compuesto_11~., data = datos.test)[,-1]
plot(datos.train$compuesto_11)
plot(datos.test$compuesto_11)


# selecionamos por validacion cruzada K-fold el mejor Valor ??
# en los datos de entrenamiento
#cv.glmnet tiene k=10 por defecto , alpha =0 indica que usamos metodo Rigde
set.seed(1)
cv.ridge <- cv.glmnet(x = datos.train.mat, y = datos.train$compuesto_11, alpha = 0)


# selecionamos por validacion cruzada K-fold el mejor Valor ??
# en los datos de entrenamiento
#cv.glmnet tiene k=10 por defecto , alpha =0 indica que usamos metodo Rigde
set.seed(1)
cv.ridge <- cv.glmnet(x = datos.train.mat, y = datos.train$compuesto_11, alpha = 0)


#grafica de distintos valores ?? en Ridge
plot(cv.ridge)

#Mejo valor de ?? (nos da el menor error de validacion)
bestlam_R <- cv.ridge$lambda.min
bestlam_R


#Modelo Ridge con los datos de entrenamiento
#Asocia un vector de coeficientes  para cada valor de ??
modelo.ridge.train <- glmnet(x= datos.train.mat , y= datos.train$compuesto_11,
                             alpha = 0)

# intercepto +301 vatiables, 100 vectores de coeficientes
# (uno para cada valor de ?? )
dim(coef(modelo.ridge.train))
plot(modelo.ridge.train, xvar = "lambda", label = TRUE)
abline (v = log(bestlam_R),  col = 'coral2', lwd = 2)
plot(modelo.ridge.train, xvar = "norm", label = TRUE)
abline (v = bestlam_R,  col = 'coral2', lwd = 2)


# Predicciones del modelo con los datos de test y el mejor ??
pred.modelo.ridge <- predict(modelo.ridge.train, s =bestlam_R, newx = datos.test.mat)


#Error de test
test.MSE.ridge <- mean((pred.modelo.ridge - datos.test$compuesto_11)^2)
test.MSE.ridge
sqrt(test.MSE.ridge)
modelo.ridge.train_f <- glmnet(x= datos.train.mat , y= datos.train$compuesto_11,
                               alpha = 0,lambda = bestlam_R)


#coeficientes del modelo Ridge
coef.Ridge <- predict(modelo.ridge.train_f, type = "coefficients", s =bestlam_R)
coef.Ridge
set.seed(1)


# selecionamos por validacion cruzada K-fold el mejor Valor ??
# en los datos de entrenamiento
#cv.glmnet tiene k=10 por defecto , alpha =1 indica que usamos metodo LAsso
cv.lasso <- cv.glmnet(x = datos.train.mat, y = datos.train$compuesto_11, alpha = 1)

#grafica de distintos valores ?? en Ridge
plot(cv.lasso)


#Mejo valor de ?? (nos da el menor error de validacion)
bestlam_L <- cv.lasso$lambda.min
bestlam_L


#Modelo Lsaso con los datos de entrenamiento
#Asocia un vector de coeficientes  para cada valor de ??
modelo.lasso.train <- glmnet(x= datos.train.mat , y= datos.train$compuesto_11,
                             alpha = 1)

# intercepto +301 vatiables, 100 vectores de coeficientes
# (uno para cada valor de ?? )
dim(coef(modelo.lasso.train))


#Grafica: de distintas cuervas de coeficientes para distintos valore ?? en Lasso
#Comportamiento de los coeficientes segun la norma l_1
plot(modelo.lasso.train, xvar = "lambda", label = TRUE)
abline (v = log(bestlam_L),  col = 'coral2', lwd = 2)
plot(modelo.lasso.train, xvar = "norm", label = TRUE)


# Predicciones del modelo con los datos de test y el mejor ??
pred.modelo.lasso <- predict(modelo.lasso.train, s =bestlam_L, newx = datos.test.mat)


#Error de test
test.MSE.lasso <- mean((pred.modelo.lasso - datos.test$compuesto_11)^2)
test.MSE.lasso
sqrt(test.MSE.lasso)
modelo.lasso.train_f <- glmnet(x= datos.train.mat , y= datos.train$compuesto_11,
                               alpha = 1, lambda = bestlam_L)
modelo.lasso.train_f <- glmnet(x= datos.train.mat , y= datos.train$compuesto_11,
                               alpha = 1, lambda = bestlam_L)


#coeficientes del modelo Ridge
coef_lasso <- predict(modelo.lasso.train_f, type = "coefficients", s =bestlam_L)


#Vemos el numero de coeficientes selecionados por el metodo Lasso los restantes son cero
sum(coef_lasso!=0)
vector_coef_laso <- coef_lasso@i
v_lasso <- coef_lasso[,1]!=0
train_2<-datos.train[v_lasso]


#--------------------ajuste de modelo solos con los predictores propuestos por lasso--------------
modelo_1 <- lm(compuesto_11~., data =  train_2)
summary(modelo_1)
predic_model_1<- predict(modelo_1,datos.test)
plot(predic_model_1)
MSE_M1<- mean((predic_model_1 - datos.test$compuesto_11)^2)
MSE_M1


#Ajuste de nuevo modelo con los predictores significativos del modelo_1
modelo_2 <- lm(compuesto_11 ~ V26+ V27 + V32 + V34 +V40 + V54 + V92 + V115+ V118 + V124+
                 V134 + V149 + V150 + V159 + V167 + V168+ V171 + V173 + V174
               + V194 + V243 + V271 + V272 + V287, data = datos.train)
summary(modelo_2)
predic_model_2<- predict(modelo_2,datos.test)
plot(predic_model_2)
MSE_M2<- mean((predic_model_2 - datos.test$compuesto_11)^2)
MSE_M2
anova(modelo_2,modelo_1)
