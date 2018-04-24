library(knit)
library(ggplot2)
library(RANN)
path <- "./Documento.Rnw"
data<- read.csv(file="data/Absenteeism_at_work.csv", header=TRUE, sep=";")
#Queremos encontrar los ID outliers de Edad y 
empleados = data[!duplicated(data$ID),]
#Ordenamos
empleados = empleados[order(empleados$ID),]
#Sacar número de ausencias.
empleados$NumAusencias = as.numeric(table(data$ID))
#se=False quita la "confidence" region

regresion_plot<-function(data,independent,dependent){
lm <- lm(dependent ~ independent, data = data)
alpha <- as.numeric(lm$coefficients[1])
beta <- as.numeric(lm$coefficients[2])
Rcuadrado <- summary(lm)$r.squared
dispersion <- ggplot(data, aes(x=independent,y=dependent)) + geom_point() + geom_smooth(method='lm',se=FALSE) +
annotate("rect", xmin = 52, xmax = 58, ymin = 40, ymax = 60, fill="white", colour="red") +
      annotate("text", x=55, y=55, label = paste("R^2 == ", Rcuadrado), parse=T) + annotate("text", x=55, y=50, label = paste("alpha == ",alpha), parse=T) +
      annotate("text", x=55, y=45, label = paste("beta == ",beta), parse=T)
dispersion
}
# lm <- lm(NumAusencias ~ Age, data = empleados)
# alpha <- as.numeric(lm$coefficients[1])
# beta <- as.numeric(lm$coefficients[2])
# Rcuadrado <- summary(lm)$r.squared
# dispersion <- ggplot(empleados, aes(x=Age,y=NumAusencias)) + geom_point() + geom_smooth(method='lm',se=FALSE) +
# annotate("rect", xmin = 52, xmax = 58, ymin = 40, ymax = 60, fill="white", colour="red") +
#       annotate("text", x=55, y=55, label = paste("R^2 == ", Rcuadrado), parse=T) + annotate("text", x=55, y=50, label = paste("alpha == ",alpha), parse=T) +
#       annotate("text", x=55, y=45, label = paste("beta == ",beta), parse=T)

AuxEmpleados = empleados[,c("Age","NumAusencias")]
#K=3 serían los dos vecinos más cercanos.
k=3
kVecinos = nn2(AuxEmpleados, k = k)
distancias = kVecinos$nn.dists
#Vamos a eliminar los outliers y crear un nuevo dataframe
empleadosK = empleados
empleadosK$ID
eliminar = c()
for(i in 1:length(distancias[,k])){
      if(distancias[i,k]>10){
            #Print paste para imprimir todo junto
            print(paste("Empleado: ",i," es un suceso anomalo"))
            eliminar <- append(eliminar,i)
      }
}
empleadosK=empleadosK[-eliminar,]
regresion_plot(empleadosK,empleadosK$Age,empleadosK$NumAusencias)




