
iris
mis_dades <- iris #longitud sepalo/anchura sepalo/longitud petalo/anchura petalo/tipo de flor

#1. sacamos los datos que queremos --> creamos variables

y <- mis_dades$Sepal.Length   #con el dolar escogemos esa linea
y

x <- mis_dades$Petal.Length
x

#2. preguntarnos: si los petalos son grandes los sepalos también???#creamos un histograma para ver los datos

plot(x,y)   #ha medida que aumento el petalo (x) también aumento el sepalo (y)
            #para encontrar la recta que más se acerque hacemos una regresión lineal

#3. tenemos que encontrar m y b
   #3.1 buscamos la media de x y la media de y

xbar <- mean(x)
xbar

ybar<- mean(y)
ybar
  #3.2 escribimos la fórmula para encontrar m --> (la tengo escrita en el ipad)
m <- sum((x-xbar)*(y-ybar))/sum((x-xbar)^2)
m #m = 0.4089223

  #3.3 escribimos la fórmula para encontrar b --> (la tengo escrita en el ipad)
b <- ybar-m*xbar
b #b = 4.306603

#m = 0.4089223
#b = 4.306603

#-----------------------------------------------------------------------------

#4. tenemos que encontrar la predicción cuando petal.length = 1.5
m*1.5+b

mod <- lm(y~x)
mod
data.frame(x=x) #base de datos con una sola columna

ypredicted <- predict(mod, data.frame(x=x)) #predecir en el modelo en cada uno de los puntos
ypredicted

plot(x, ypredicted)


plot(x,y, pch=16,col='red') #volvemos a poner la grafica
lines(x,ypredicted)#le hacemos una linea para q pase por todos los puntos

Rsq <- sum((ypredicted-ybar)^2/sum((y-ybar)^2)) #fórmula en el ipad
Rsq # Rsq=0.7599546 !!!!!

summary(mod) #multiple R-squared!!!!!
sqrt(Rsq) #coeficiente de correlacion es R sin cuadrados
cor.test(x,y)
#R=0.8717538

