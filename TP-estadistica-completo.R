###################################################################
################# Funciones Utiles para el Trabajo ################
###################################################################

#generamos una función para la  distribución Poisson
muestra_estimadores <- function(m,n,lambda){
  muestra<-vector(length = m)
  for(i in 1:m){
    ma <- rpois(n , lambda = lambda)
    x_raya <- mean(ma)
    moneda <- rbinom(1,1,1/n)
    muestra[i]<-(1-moneda)*x_raya + moneda*n
  }
  return(muestra)  
}
#generamos una función para la  distribución Gamma
muestra_estimadoresgamma <- function(m,n,alpha, beta){
  muestra<-vector(length = m)
  for(i in 1:m){
    ma <- rgamma(n , shape = alpha, scale = beta)
    x_raya <- mean(ma)
    moneda <- rbinom(1,1,1/n)
    muestra[i]<-(1-moneda)*x_raya + moneda*n
  }
  return(muestra)  
}

#generamos una función para la  distribución Normal
muestra_estimadoresnormal <- function(m,n,mu,desvio){
  muestra<-vector(length = m)
  for(i in 1:m){
    ma <- rnorm(n , mean = mu, sd = desvio)
    x_raya <- mean(ma)
    moneda <- rbinom(1,1,1/n)
    muestra[i]<-(1-moneda)*x_raya + moneda*n
  }
  return(muestra)  
}
#generamos una función para la  distribución Cauchy
muestra_estimadorescauchy <- function(m,n,xcero, gamma){
  muestra<-vector(length = m)
  for(i in 1:m){
    ma <- rcauchy(n , location = xcero, scale = gamma)
    x_raya <- mean(ma)
    moneda <- rbinom(1,1,1/n)
    muestra[i]<-(1-moneda)*x_raya + moneda*n
  }
  return(muestra)  
}

####################################################################################
##### Simulacion del estimador, generamos un Dataframe con la informacion   ########
##### de la media, la varianza y el sesgo del estimador para distintos      ########
##### valores de n y de lambda.                                             ########
####################################################################################

datos<-data.frame("n"=integer(),"lambda"=double(),"Es_barra"=double(),"Es_var"=double())

#Rangos de muestreo, para lambda y para n.
rlamb<-c(1/100,1/10,1,10,20,25,50,75,100,150,200)
rn<-c(1:99,seq(100,300,25))

for(i in 1:length(rlamb)){
  lamb<-rlamb[i]
  
  for(j in 1:length(rn)){
    n<-rn[j]
    #El codigo puede demorar algunos minutos, segun el rango de n y el tamaño de
    #las muestras, por eso se incorpora el siguiente contador de avance para
    #tener un estimativo del tiempo restante.(sale por la consola)
    print(paste( "porcentaje de avance: ",round(((i-1)*length(rn) + j)*100/(length(rlamb)*length(rn)),digits=2),"%",sep=" "))
    #Tomamoos un muestra de estimadores y guardamos la informacion
    m<-muestra_estimadores(10000,n,lamb)
    datos<-rbind(datos,data.frame("n"=n,"lambda"=lamb,"Es_barra"=mean(m),"Es_var"=var(m)))
  }
}
#agregamos la columna del sesgo como la diferencia entre la media y lambda
datos<-cbind(datos,"ses"=datos[,3]-datos[,2])

####################################################################################
#####                             Graficos del Sesgo                           #####
####################################################################################
#####        Sesgo como Funcion de lambda (requiere el dataframe anterior)     #####
####################################################################################
## Iniciamos un grafico nuevo vacio
x11()
x<-vector()
rngraf<-seq(1,100,3)
## Definimos parametros de colores para el grafico
par(bg="gray0",col.axis="white",col.main="white",col.lab="white",col.sub="white",col="white")
#dibujamos el area de trabajo en blanco (un plot vacio) y agregamos el titulo
plot(x,type = "n",xlim = c(0,140),ylim = c(-120,2),xlab = "lambda",ylab =" ses")
title(main = expression(Sesgo~Vs~lambda~para~distintos~n))

##armamos un vector con colores para graficar los distintos n (cada n de un color)
colores<-rainbow(length(rngraf))

for(i in 1:length(rngraf)){
  n<-rngraf[i]
  ## Agregamos los puntos al grafico generado para cada n
  points(datos[datos$n ==n,2],datos[datos$n ==n,5],col=colores[i])
  ## superponemos el modelo teorico a los puntos
  curve(1-x/n,0,100,add=T,col=colores[i])
}
#Finalmente agregamos una leyenda a los graficos
legend(105,-10, legend = rngraf, fill = colores, title="Valores de n",border = "white",ncol = 2)

#############################################################################################
###            Sesgo como sucesion de n (requiere el dataframe anterior)                  ###
#############################################################################################
## Iniciamos un grafico nuevo vacio
x11()
x<-vector()
## Definimos parametros de colores para el grafico
par(bg="gray0",col.axis="white",col.main="white",col.lab="white",col.sub="white",col="white")
#dibujamos el area de trabajo en blanco (un plot vacio) y agregamos el titulo
#Obs: regulamos el zoom del grafico con xlim...
#(elegimos ver hasta n=25 para observar las curvas con mayor presicion. pero hay datos hasta n=100)
plot(x,type = "n",xlim = c(0,25),ylim = c(-200,2),xlab = "n",ylab =" ses")
title(main = expression(Sesgo~Vs~n~para~distintos~lambda))

##armamos un vector con colores para graficar los distintos n (cada n de un color)
colores<-rainbow(length(rlamb))

for(i in 1:length(rngraf)){
  lambda<-rlamb[i]
  ## Agregamos los puntos al grafico generado para cada n
  points(datos[datos$lambda == lambda,1],datos[datos$lambda ==lambda,5],col=colores[i])
  ## superponemos el modelo teorico a los puntos
  curve(1-lambda/x,0,25,add=T,col=colores[i])
}
#Finalmente agregamos una leyenda a los graficos
legend(15,-75, legend = rlamb, fill = colores, title=expression(valores~de~lambda),border = "white",ncol = 2)
###############################################################################
####              Graficos Varianza (Requiere Data frame)                  ####
###############################################################################
####              Varianza como sucesion de n (Requiere Data frame)        ####
###############################################################################

x11()
x<-vector()
lg<-rlamb[1:(length(rlamb))]

par(bg="gray0",col.axis="white",col.main="white",col.lab="white",col.sub="white",col="white")
plot(x,type = "n",xlim = c(0,60),ylim = c(0,10000),xlab = "n",ylab ="Varianza")
title(main = expression(Var(delta)~Vs~n~para~distintos~lambda))

colores<-rainbow(length(lg))

for(i in 1:length(lg)){
  lambda<-lg[i]
  points(datos[datos$lambda == lambda,1],datos[datos$lambda ==lambda,4],col=colores[i])
  curve((lambda^2)*(-1/x^2+1/x)+lambda*(3/x-1/x^2-2)-1+x,1,60,n=1001,add=T,col=colores[i])
}
legend(25,7500, legend = lg, fill = colores, title=expression(valores~de~lambda),border = "white",ncol = 2)

###############################################################################
####              Varianza como funcion de lambda (Requiere Data frame)    ####
###############################################################################

x11()
x<-vector()
lg<-c(seq(1,100,3))

par(bg="gray0",col.axis="white",col.main="white",col.lab="white",col.sub="white",col="white")
plot(x,type = "n",xlim = c(0,350),ylim = c(0,1200),xlab = "n",ylab ="Varianza")
title(main = expression(Var(delta)~Vs~lambda~para~distintos~n))

colores<-rainbow(length(lg))

for(i in 1:length(lg)){
  n<-lg[i]
  points(datos[datos$n == n,2],datos[datos$n ==n,4],col=colores[i])
  curve((x^2)*(-1/n^2+1/n)+x*(3/n-1/n^2-2)-1+n,1,200,n=1001,add=T,col=colores[i])
}
legend(230,1100, legend = lg, fill = colores, title=expression(valores~de~n),border = "white",ncol = 2)

####################################################################################
#####       Consistencia del estimador para lambda con x_i~Pois(lambda)        #####
####################################################################################

### Definimos algunas variables que nescesitaremos
m<-1000
rn<-1:100
rk<-c(0.5,1:10)
vfrec<-vector()
vestim<-vector()

### Definicion de parametros para armar el grafico
x<-vector()
x11()
par(bg="gray0",col.axis="white",col.main="white",col.lab="white",col.sub="white",col="white")
plot(x,type = "n",xlim = c(0,140),ylim = c(0,1),xlab = "n",ylab ="frec")
title(main = expression(Distribucion~Pois(lambda)~para~lambda==10))
### creamos un vector de colores para diferenciar cada valor de k
colores<-rainbow(length(rk))

### cilco principal:
for(i in 1:length(rn)){
  ## tomamos un n...
  n<-rn[i]
  ## tomamos una muestra de estimadores para ese n....
  vestim<-muestra_estimadores(m,n,10)
  ## y para cada valor de k elegido para el analisis:
  for(j in 1:length(rk)){
    k<-rk[j]
    ## Calculamos la Frecuencia relativa de la muestra y armarmos un vector con los valores para ese n
    vfrec[j]<-sum(abs(vestim-10)<k)/m
  }
## luego de calcular las frecuencias para cada k, agregamos en el grafico los puntos
points(rep(n,length(rk)), vfrec, col = colores)
## volvemos a repetir para cada n....
}
## Finalmente agregamos una leyenda al grafico  
legend(110,0.8, legend = rk, fill = colores, title="Valores de k",border = "white")

####################################################################################
#####       Consistencia del estimador para x_i~N(mu,sigma^2)                  #####
####################################################################################

### Definimos algunas variables que nescesitaremos
m<-1000
rn<-1:100
rk<-c(0.5,1:10)
vfrec<-vector()
vestim<-vector()

### Definicion de parametros para armar el grafico
x<-vector()
x11()
par(bg="gray0",col.axis="white",col.main="white",col.lab="white",col.sub="white",col="white")
plot(x,type = "n",xlim = c(0,140),ylim = c(0,1),xlab = "n",ylab ="frec")
title(main = expression(Distribucion~N(50,sigma^2)~para~sigma==10))
### creamos un vector de colores para diferenciar cada valor de k
colores<-rainbow(length(rk))

### cilco principal:
for(i in 1:length(rn)){
  ## tomamos un n...
  n<-rn[i]
  ## tomamos una muestra de estimadores para ese n....
  vestim<-muestra_estimadoresnormal(m,n,50,10)
  ## y para cada valor de k elegido para el analisis:
  for(j in 1:length(rk)){
    k<-rk[j]
    ## Calculamos la Frecuencia relativa de la muestra y armarmos un vector con los valores para ese n
    vfrec[j]<-sum(abs(vestim-10)<k)/m
  }
  ## luego de calcular las frecuencias para cada k, agregamos en el grafico los puntos
  points(rep(n,length(rk)), vfrec, col = colores)
  ## volvemos a repetir para cada n....
}
## Finalmente agregamos una leyenda al grafico  
legend(110,0.8, legend = rk, fill = colores, title="Valores de k",border = "white")

####################################################################################
#####       Consistencia del estimador para x_i~gamma(alpha,beta)              #####
####################################################################################

### Definimos algunas variables que nescesitaremos
m<-1000
rn<-1:100
rk<-c(0.5,1:10)
vfrec<-vector()
vestim<-vector()

### Definicion de parametros para armar el grafico
x<-vector()
x11()
par(bg="gray0",col.axis="white",col.main="white",col.lab="white",col.sub="white",col="white")
plot(x,type = "n",xlim = c(0,140),ylim = c(0,1),xlab = "n",ylab ="frec")
title(main = expression(Distribucion~gamma(alfa,5)~para~alfa==50))
### creamos un vector de colores para diferenciar cada valor de k
colores<-rainbow(length(rk))

### cilco principal:
for(i in 1:length(rn)){
  ## tomamos un n...
  n<-rn[i]
  ## tomamos una muestra de estimadores para ese n....
  vestim<-muestra_estimadoresgamma(m,n,50,5)
  ## y para cada valor de k elegido para el analisis:
  for(j in 1:length(rk)){
    k<-rk[j]
    ## Calculamos la Frecuencia relativa de la muestra y armarmos un vector con los valores para ese n
    vfrec[j]<-sum(abs(vestim-50)<k)/m
  }
  ## luego de calcular las frecuencias para cada k, agregamos en el grafico los puntos
  points(rep(n,length(rk)), vfrec, col = colores)
  ## volvemos a repetir para cada n....
}
## Finalmente agregamos una leyenda al grafico  
legend(110,0.8, legend = rk, fill = colores, title="Valores de k",border = "white")

####################################################################################
#####       Consistencia del estimador para x_i~exp(lambda)                    #####
####################################################################################
#####       Usaremos una distribucion gamma(1, 1/lambda)                       #####
####################################################################################
### Definimos algunas variables que nescesitaremos
m<-1000
rn<-1:100
rk<-c(0.5,1:10)
vfrec<-vector()
vestim<-vector()

### Definicion de parametros para armar el grafico
x<-vector()
x11()
par(bg="gray0",col.axis="white",col.main="white",col.lab="white",col.sub="white",col="white")
plot(x,type = "n",xlim = c(0,140),ylim = c(0,1),xlab = "n",ylab ="frec")
title(main = expression(Distribucion~exp(lambda)~para~lambda==10))
### creamos un vector de colores para diferenciar cada valor de k
colores<-rainbow(length(rk))

### cilco principal:
for(i in 1:length(rn)){
  ## tomamos un n...
  n<-rn[i]
  ## tomamos una muestra de estimadores para ese n....
  vestim<-muestra_estimadoresgamma(m,n,1,1/10)
  ## y para cada valor de k elegido para el analisis:
  for(j in 1:length(rk)){
    k<-rk[j]
    ## Calculamos la Frecuencia relativa de la muestra y armarmos un vector con los valores para ese n
    vfrec[j]<-sum(abs(vestim-10)<k)/m
  }
  ## luego de calcular las frecuencias para cada k, agregamos en el grafico los puntos
  points(rep(n,length(rk)), vfrec, col = colores)
  ## volvemos a repetir para cada n....
}
## Finalmente agregamos una leyenda al grafico  
legend(110,0.8, legend = rk, fill = colores, title="Valores de k",border = "white")

####################################################################################
#####       Consistencia del estimador para x_i~C(x_0,gamma)                   #####
####################################################################################

### Definimos algunas variables que nescesitaremos
m<-1000
rn<-1:100
rk<-c(0.5,1:10)
vfrec<-vector()
vestim<-vector()

### Definicion de parametros para armar el grafico
x<-vector()
x11()
par(bg="gray0",col.axis="white",col.main="white",col.lab="white",col.sub="white",col="white")
plot(x,type = "n",xlim = c(0,140),ylim = c(0,1),xlab = "n",ylab ="frec")
title(main = expression(Distribucion~C(x_0,5)~para~gamma==30))
### creamos un vector de colores para diferenciar cada valor de k
colores<-rainbow(length(rk))

### cilco principal:
for(i in 1:length(rn)){
  ## tomamos un n...
  n<-rn[i]
  ## tomamos una muestra de estimadores para ese n....
  vestim<-muestra_estimadorescauchy(m,n,30,5)
  ## y para cada valor de k elegido para el analisis:
  for(j in 1:length(rk)){
    k<-rk[j]
    ## Calculamos la Frecuencia relativa de la muestra y armarmos un vector con los valores para ese n
    vfrec[j]<-sum(abs(vestim-30)<k)/m
  }
  ## luego de calcular las frecuencias para cada k, agregamos en el grafico los puntos
  points(rep(n,length(rk)), vfrec, col = colores)
  ## volvemos a repetir para cada n....
}
## Finalmente agregamos una leyenda al grafico  
legend(110,0.8, legend = rk, fill = colores, title="Valores de k",border = "white")
