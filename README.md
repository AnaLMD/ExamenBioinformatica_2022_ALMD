# ExamenBioinformatica_2022_ALMD

# 1. Comandos Linux: Tengo un fichero llamado genes.txt con 3 columnas: Nombre_ de_ gen, cromosoma y posición. Separados por tabulados. ¿Qué comando utilizarías para saber?:
cat genes.txt
# 1.a) ¿Cuántas palabras hay en el fichero?
wc genes.txt

# 1.b) ¿Cómo cambiarías todos los tabulados por guiones?
Cat genes.txt | sed ‘s/\t/-/g’ 

# 1.c) ¿Cómo cambiarías solo la primera aparición?
Cat genes.txt | sed ‘s/\t/-/1’  | awk ‘{print$1}

# 1.d. ¿Cómo buscarías todos los genes excepto el gen llamado DSTYK?
Cat genes.txt | wc | grep -v DSTYK


# 2)
install.packages("nycflights13")
library("nycflights13")
install.packages("dplyr")
library("dplyr")

weather <- nycflights13::weather
delete.na <- function(df, n=0) {
  df[rowSums(is.na(df)) <= n,]
}

weather_NA <- delete.na(weather)

# 2.a. (0.5 puntos) ¿Cuántas columnas y cuántos registros tiene este dataset?
ncol(weather) #15
nrow(weather) #26115

# 2.b. (0.5 puntos) ¿Cuántos “origin” diferentes existen y para cada uno de ellos cuántos registros hay?
table(weather$origin)
EWR_ <- weather[weather$origin == "EWR", ]
JFK_ <- weather[weather$origin == "JFK", ]
LGA_ <- weather[weather$origin == "LGA", ]
nrow(EWR_) #8703
nrow(JFK_) #8706
nrow(LGA_) #8706

# 2.c. (0.5 puntos) En LGA, ¿cuáles fueron la mediana del wind_speed y media de pressure?
LGA_NA <- delete.na(LGA_)
median(LGA_NA$wind_speed) #16.11
mean(LGA_NA$pressure) #1015

# 2.d. (0.5 puntos) Después de eliminar los NA de la columna wind_gust, calcula para cada mes la media de wind_speed y wind_gust, y el número de casos.
weather%>%
     filter(!is.na(wind_gust))%>%
     group_by(month)%>%
     summarise(
         speed=mean(wind_speed),
         gust=mean(wind_gust),
         num=n()
     )

# 3.a) Intenta hacer este plot 
par(mfrow = c(1, 3))
boxplot( data = EWR_,
temp~month,
main="EWR",
xlab="Month",
ylab="C",
col="red",
border="black"
)

boxplot (data = JFK_, 
temp~month, 
main = "JFK", 
xlab = "Month", 
ylab = "C", 
col = "green",
border = "black")

boxplot (data = LGA_, 
temp~month, 
main = "LGA", 
xlab = "Month", 
ylab = "C", 
col = "blue",
border = "black")

# 3.b) Crea una función que plotee el plot anterior.
# - Como argumentos se podrán introducir: el nombre de la columna de interés, el título y las unidades.
# - La función debe devolver un vector con 3 valores. Estos valores serán la media de cada uno de los lugares.
# - Por pantalla también tendrá que aparecer la media mediante un print.

plot_meteo <- function(dat = weather, meteo = "temp", titulo = "Punto de rocío", unidades = "F")
{
dat <- data.frame(dat)
boxplot (dat[,meteo], main = titulo, ylab = unidades, xlab = "hours", col = "blue" )
}

par(mfrow = c(1, 1))
plot_meteo(meteo = "humid", titulo = "Humedad", unidades = "Relative humidity")



# 4.a Qué correlación tuvieron la temperatura y humedad en cada uno de los origin? Plotealo mediante puntos con ggplot.
ggplot(data = weather) + 
geom_point(mapping = aes(x = temp, y = origin, col = humid))

# 4.b. Si comparas la temperatura en los origins JFK y LGA ¿son estadísticamente diferentes? ¿Qué p-valor consigues? Plotea los boxplots.

t.test(log(JFK_$temp), log(LGA_$temp))

##### El p-valor es mucho menor que 0.05 por lo que se podria concluir que las temperaturas entre los origenes si son estadisticamente diferentes. 

boxplot(log(JFK_$temp), log(LGA_$temp), col = 6:7, names = c("JFK", "LGA"), ylab = "logTemp", main = "Comparison between temperatures")

# 5.a) ¿Cuál es el punto con FC (Fold change) más grande en valores absolutos?
El punto con el fold change mas grande es en 5 

# 5.b) ¿Qué gen sobreexpresado es el más significativo?
El gen mas sobreexpresado es el RBP1

# 6. Sube el examen a github y escribe a continuación tu url aquí.

https://github.com/AnaLMD/ExamenBioinformatica_2022_ALMD.git


# 7. Acaba el documento con el comando sessionInfo()
sessionInfo()
