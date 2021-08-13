# Importar datos
library(readxl)
tabla_datos <- read_xlsx("datos/..")
View(tabla_datos)

## Analisis de Correspondencias
library(ca)
acm <- ca(tabla_datos)
summary(acs)
plot(summary(acs)$scree, type="l") #Gráfico de codo (tal vez sea así)

#Graficar
plot(acs)
plot(acs, lines = TRUE) #Se observa la dependencia
plot(acs, arrows = TRUE)
plot(acs, arrows = c(TRUE, FALSE))
plot(acs, arrows = c(FALSE, TRUE))
plot(acs, arrows = c(TRUE, TRUE))
plot(acs, arrows = c(FALSE, TRUE), lines = TRUE)

acm <- mjca(tabla_datos,lambda = "indicator",nd=NA)
plot(acm)
biplot(acm$rowpcoord,acm$colpcoord)
#------------------------------------------------------------------------------

## Analisis de Componentes
acp <- prcomp(tabla_datos)
plot(ACP, type="l") #Grafico de sedimentacion o de codo
plot(ACP$rotation)
biplot(ACP)
plot(ACP$x[,1], ACP$x[,2])
#PCA1 vs PCA2, diferenciando por especie
plot(ACP$x[,1], ACP$x[,2], col = ifelse(crabs$sp=="O", "orange", "blue"), pch=16, xlab="PCA,1", ylab="PCA,2")
library(scatterplot3d)
scatterplot3d(ACP$x[,1], ACP$x[,2], ACP$x[,3], color=ifelse(crabs$sp=="O", "orange", "blue"))

#Graficar ggfortify
library(ggfortify)
autoplot(ACP)
autoplot(ACP, colour = 'sex')
autoplot(ACP, data = crabs, colour = 'sex', label=TRUE,label.size=3)
autoplot(ACP, data = crabs, colour = 'sex', shape=FALSE,label.size=3)
autoplot(ACP, data = crabs, colour = 'sex', shape=FALSE,label.size=3, loadings=TRUE)
autoplot(ACP, data = crabs, colour = 'sex', shape=FALSE,label.size=3,
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3,
         loadings.label.colour='blue')
#Gráfico final con todo
autoplot(ACP, data = crabs, colour = ifelse(crabs$sex=="M"&crabs$sp=="O", "purple", ifelse(crabs$sex=="M"&crabs$sp=="B", "green", ifelse(crabs$sex=="F"&crabs$sp=="O", "orange", ifelse(crabs$sex=="F"&crabs$sp=="B", "blue", "black")))),
         shape=FALSE,label.size=3,
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 3,
         loadings.label.colour='red')

#------------------------------------------------------------------------------

## Analisis de conglomerados
acl <- kmeans(datos,centers=3)
plot(datos, col = cl[["cluster"]])
text(datos,row.names(datos),pos=3)
