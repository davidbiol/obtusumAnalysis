# Importar datos
library(readxl)
tabla_datos <- read_xlsx("datos/..")
View(tabla_datos)

## Análisis de Correspondencias
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
