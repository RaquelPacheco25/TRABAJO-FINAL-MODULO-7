
## TRABAJO FINAL MODULO 7
## RAQUEL PACHECO 

## VARIABLE: CREDITO PANORAMA FINANCIERO

## Librerias
library(foreign)
library(urca)
library(openxlsx)
library(highcharter)

file.choose()
## Carga de la base

data<-read.xlsx("C:\\Users\\CompuStore\\Desktop\\CURSOS\\CIENCIA DE DATOS\\MODULO 7\\TRABAJO FINAL MODULO 7\\BASE TRABAJO FINAL.xlsx")

tsdata <- ts(as.vector(as.matrix(data)),
             start=c(2017,1),
             end=c(2021,7),
             frequency = 12)
ts.plot(tsdata)

## Contraste Aumented Dickey Fuller 

adftes<-ur.df(tsdata, type=c("trend"), 
              selectlags=c("BIC"))
summary(adftes)

## 4 ES > A 4, 3, 3? SI por lo tanto RH0

#REGLA 

# si el valor calculado es > al valor critico RH0
# H0: Raiz unitaria
# Ha: Raiz no unitaria 

# En este caso raiz unitaria= no estacionariedad 
# Usted desea que no sea estacionaria es decir que no sea raiz unitaria 

## Contraste Phillip y perron

pptes<- ur.pp(tsdata, type=c("Z-tau"),
              model=c("trend"),
              lags = c("short"))
summary(pptes)

## 5 ES > A 4, 3, 3? SI por lo tanto se RH0

#REGLA 

# si el valor calculado es > al valor critico RH0
# H0: Raiz unitaria
# Ha: Raiz no unitaria 

# En este caso raiz unitaria= no estacionariedad 

### Contraste KPSS

kpsstes<-ur.kpss(tsdata, type = c("tau"),
                 lag=c("short"))
summary(kpsstes)


## 0.0849 ES > A 0.119 no por lo tanto no RH0

#REGLA 

# si el valor calculado es > al valor critico RH0
# H0: Estacionariedad
# Ha: No estacionariedad

## Contraste Elliot, Rothenberg & Stock Unit Root Test
## Contraste de alta potencia (menos sensible a valores atipicos)

erstest<-ur.ers(tsdata, type = c("DF-GLS"),
                model = c("trend"), 
                lag.max = 4)
summary(erstest)

# -2.7134 es > a 3,3,2? no por lo tanto no RH0

# si el valor calculado es > al valor critico RH0
# H0: Raiz unitaria
# Ha: Raiz no unitaria 

# En este caso raiz unitaria= no estacionariedad 

### Complemento para ver de cuanto se necesita para convertir en estacionaria

ndiffs(tsdata, test = c("kpss"))

ndiffs(tsdata, test = c("adf"))

ndiffs(tsdata, test = c("pp"))

# Es necesaerio diferenciar la serie a 1 de tal manera que se convierta en una serie estacionaria aunque claramente ya lo es 

## Primeras diferencias

par(mfrow=c(1,2))
plot(tsdata)
plot(diff(tsdata,1))

## ¿que tipo de modelo arima es? Identificar 

acf(diff(tsdata,1), xlim=c(0.2,4))
pacf(diff(tsdata,1))


## Sera necesario incorporar una media movil 
## Sera necesario incorporar 1 autorregresivo

## CONSTRUCCION DEL MODELO 
## forma compacta
# Arima (p,d,q)

modelo1 <- Arima(tsdata, order = c(0, 1, 1))
modelo1
#Ho: coeficiente=0
# Ha: coeficiente!0

# Si no se rechaza la Ho significa que el coeficiente no ayuda a replicar la serie original 
# Se busca RHO

# Prueba t
0.9573/0.0511

# Regla 
# Si tiene mas de 36 datos 
# Si el t calculado es >2 RH0 ## 18.73386 es >2 ?  SI por lo tanto RH0 y el coficiente SI aporta areplicar la serie

## Evaluando el modelo CAPACIDAD PREDICTIVA DEL MODELO ACCURACY

accuracy(modelo1)

### El MAPE nos dice que el error de pronostico va a hacer de 2.01 es decir el 


### Evaluar los residuos

par(mfrow=c(1,1))

plot(modelo1$residuals)
abline(h=0)

# Los residuos estan centrados, presenta de una o dos tendencia, se comportan de manera aleatoria=ruido blanco
# se espera que el modelo utilizado sea un buen modelo pero como la vista es subjetiva


Box.test(modelo1$residuals, lag=1, type = "Ljung-Box")

# Ho: residuos independientes
# Ha: residuos dependientes

# si pvalor es <0.05 RH0
#No RHO

par(mfrow=c(1,2))
Acf(modelo1$residuals)
Pacf(modelo1$residuals)

## PRONOSTICO

f1<-forecast(modelo1, h=4, level=c(95))
plot(f1)

f1

hchart(f1)

## MODELO AUTOARIMA
modelo1<-auto.arima(tsdata)
accuracy(modelo1)
plot(modelo1)

Box.test(modelo1$residuals, type = c("Ljung-Box"))

# Ho: residuos independientes
# No RHo

Acf(modelo1$residuals)
Pacf(modelo1$residuals)

#Proyeccion 

f1 <- forecast(modelo1,h=4,level=c(95))
f1
hchart(f1)


## COMPARAR CUAL ES LA SERIE ORIGINAL VS LA SERIE EN ESTUDIO

ts.plot(tsdata, f1$fitted,f1$mean,
        col=c("red", "blue","green"),
        main="Proyeccion del Credito Panorama Financiero")

