
## TRABALLO FINAL DATOS FUNCIONALES ## 

# BASE DE DATOS

#DATOS LONBORG
datos_lonborg<-readRDS("Lonborg.rds")
# Filtramos base de datos
datos_lonborg<-na.omit(datos_lonborg)
# Filtramos también el DOC
# observamos distribución
library(dplyr)
hist(datos_lonborg$DOC)
quantile(datos_lonborg$DOC,0.95) # 95% = 494.8617
# ponemos como límite 500umol/kg: 95% de los datos
datos_lonborg <- datos_lonborg %>% filter (DOC<500)

# CONVERTIMOS A DATOS FUNCIONALES
summary(datos_lonborg$Location)
unique(datos_lonborg$Location)
lonborg <- datos_lonborg[,c('Location','LATITUDE','LONGITUDE','CTD.TEMPERATURE','CTD.SALINITY','DOC','MONTH')]
lonborg$MONTH<-as.numeric(lonborg$MONTH)

# Datos funcionales: Variable TEMPERATURA
# utilizamos para predecir variable DOC
lonborg.temp <- lonborg %>% group_by(Location,MONTH) %>% summarise(meanTemp = mean(CTD.TEMPERATURE))
summary(lonborg.temp)

library(tidyr)
matriz.lonborg<- lonborg.temp %>%
 pivot_wider(names_from = MONTH, values_from = meanTemp)
matriz.lonborg<-as.data.frame(matriz.lonborg) 
matriz.lonborg<-na.omit(matriz.lonborg); matriz.lonborg
rownames(matriz.lonborg)<- matriz.lonborg$Location
matriz.lonborg<-matriz.lonborg[,-1]
nrow(matriz.lonborg)
month<-as.vector(1:12)
matriz.lonborg

library(knitr)
kable(x = round(matriz.lonborg[1:5,1:5],1),
      caption = "Temperaturas medias (ºC) en cada estación de mostraxe por cada mes do ano",
      format = "latex", booktabs = TRUE,file = "tabla1.tex")

#fdata
library(fda.usc)
lonborg.fdata<-fdata(mdata=matriz.lonborg, argvals=month, rangeval = c(1,12))
plot(lonborg.fdata, main="Temperatura punto de muestreo", xlab="Tiempo (meses)",ylab="Media Temp (ºC)")
# saveRDS(lonborg.fdata,file='lonborg_fdata.RDS')
# incluímos otras variables para regresión para regresión: DOC (variable que vamos a predecir) y SAL (salinidad: covariable)
lonborg.var <- lonborg %>% group_by(Location)  %>% summarise(meanDOC = mean (DOC), meanSAL = mean (CTD.SALINITY))
lugares<-as.character(row.names(matriz.lonborg))
lonborg.var  <- lonborg.var  %>% filter(Location %in% lugares)
y<-as.matrix(lonborg.var [,2:3])
rownames(y)<-1:nrow(y)
lonborg.fdata.var<-list(lonborg.fdata,y=y)

# ------------------ # 

# VISUALIZACIÓN DE LOS DATOS

# MAPAS EXPLICATIVOS

library(ggplot2)
library(rgl)
library(mapview)
# Visualización datos
# versión 2d
# buscamos ubicaciones de muestreo
ubi<-as.vector(rownames(matriz.lonborg))
datos_lonborg[datos_lonborg$Location == ubi,]

# filtramos por ubicaciones y nos quedamos con LONG y LAT medias de cada estación
lonborg_puntos <- lonborg %>% group_by(Location) %>% summarise(LONG = mean (LONGITUDE),LAT = mean (LATITUDE))
lonborg_puntos <- as.data.frame(lonborg_puntos)
# ahora filtramos por aquellas que vamos a utilizar (sin NA's, datos para los 12 meses)
lonborg_puntos <- lonborg_puntos %>% filter(Location %in% ubi)
# Estaciones de muestreo seleccionadas


library(leaflet)
colors <- colorFactor(palette= "viridis", domain = lonborg_puntos$Location)

# Crear un mapa interactivo con fondo mundial
leaflet(lonborg_puntos) %>%
  addTiles() %>%  # Mapa base (OpenStreetMap)
  addCircleMarkers(
    ~LONG, ~LAT, 
    label = ~Location,                  # Mostrar nombres al pasar el cursor
    color = ~colors(Location),                   # Color de los puntos
    radius = 6,                       # Tamaño de los puntos
    fillOpacity = 0.8,                # Transparencia
    stroke = TRUE, weight = 1         # Bordes de los puntos
  ) %>%
  setView(lng = mean(lonborg_puntos$LONG), lat = mean(lonborg_puntos$LAT), zoom = 2) # Vista global


# ---------- #


library(leaflet)
colors <- colorFactor(palette= "viridis", domain = lonborg_puntos$Location)

# Crear un mapa interactivo sobre región concreta (Islas bermudas)
leaflet(lonborg_puntos) %>%
  addTiles() %>%  # Mapa base (OpenStreetMap)
  addCircleMarkers(
    ~LONG, ~LAT, 
    label = ~Location,                  # Mostrar nombres al pasar el cursor
    color = ~colors(Location),                   # Color de los puntos
    radius = 6,                       # Tamaño de los puntos
    fillOpacity = 0.8,                # Transparencia
    stroke = TRUE, weight = 1         # Bordes de los puntos
  ) %>%
  setView(lng = lonborg_puntos$LONG[50], lat = lonborg_puntos$LAT[50], zoom = 10) # Vista global



# ------------------------- # 

# TEMOS OS NOSOS DATOS PREPARADOS, AGORA COMEZAMOS CO TRABALLO

# ANÁLISIS DE DATOS FUNCIONALES


library(fda.usc)
lonborg.fdata<-fdata(mdata=matriz.lonborg, argvals=month, rangeval = c(1,12))
plot(lonborg.fdata, main="Temperatura puntos de muestreo", xlab="Tiempo (meses)", ylab="Media Temp (ºC)")

#GRÁFICAS MOSTRANDO AS RECTAS DE DATOS
layout(matrix(c(1, 2), nrow = 1), widths = c(7, 1))
gradiente.color <- colorRampPalette(c("lightgreen","red","darkred"))(100) 
color.DOC <- gradiente.color[as.numeric(cut(lonborg.fdata.var$y[,1], breaks = 100))]

plot(lonborg.fdata,
     main = " ",
     xlab="Tiempo (meses)",ylab="Media Temp (ºC)",
     col = color.DOC,
     lwd=2,)
# Agregar una leyenda de gradiente
par(mar = c(7, 1, 6, 3)) # Axustar marxes da lenda
# función redondear al siguiente múltiplo de 10
redond.cerc.10 <- function(x) {
  round(x / 10) * 10
}

legend_gradient <- seq(redond.cerc.10(min(lonborg.fdata.var$y[,1])), 
                       redond.cerc.10(max(lonborg.fdata.var$y[,1])), length.out = 100)

# Usar image para dibujar el gradiente
image(
  y = seq(redond.cerc.10(min(lonborg.fdata.var$y[,1])),
          redond.cerc.10(max(lonborg.fdata.var$y[,1])), length.out = 101), # Valores del eje Y
  z = matrix(legend_gradient, nrow = 1), 
  col = gradiente.color, 
  axes = FALSE,
)
axis(4, at = (c(redond.cerc.10(min(lonborg.fdata.var$y[,1])),150,250,350,
                redond.cerc.10(max(lonborg.fdata.var$y[,1])))), 
     labels = c(redond.cerc.10(min(lonborg.fdata.var$y[,1])),150,250,350,
                redond.cerc.10(max(lonborg.fdata.var$y[,1]))), cex= 0.8)

# Etiqueta para la leyenda
mtext("DOC", side = 3, line = 2, cex = 1)
par(mfrow=c(1,1))

# ---------------- # 


# SUAVIZACIÓN DE LAS CURVAS

# Base de Fourier
fou=create.fourier.basis(lonborg.fdata$rangeval,nbasis=7); fou
matplot(1:12, eval.basis(1:12,fou),type="l")  #cada una de las líneas se corresponden con los elementos de la base.
# base de fourier
lonborg.fd=Data2fd(argvals=month, t(lonborg.fdata$data),basisobj=fou)
plot(lonborg.fd,main="Temperatura puntos de muestreo", xlab="Tempo (meses)", ylab="Media Temp (ºC)")

# comparamos con el de antes
par(mfrow=c(1,2))
plot(lonborg.fdata[1:8],main="Datos iniciais",xlab = "Tempo(meses)",ylab="Media Temp (ºC)") 
plot(lonborg.fd[1:8],main="Base de Fourier",xlab="Tempo (meses)",ylab="Media Temp (ºC)") #dibujamos las 5 primeras
# A la derecha datos crudos como vienen y en la derecha las curvas de fourier.
par(mfrow=c(1,1))


# COMPONENTES PRINCIPALES
lon.pc=create.pc.basis(lonborg.fdata,l=1:3)
names(lon.pc)
lon.pc$coefs
plot(lon.pc$basis,xlab="Tempo (meses)")  
abline(h=0)

summary(lon.pc) #1 componente explica el 87.93% de la variabilidad. En total, PC3: 99.36%
pairs(lon.pc$coefs) #componentes principales
par(mfrow=c(1,1))


# ----------------- #

# ANÁLISIS EXPLORATORIO DE DATOS FUNCIONALES
# Media en Non hilbertian Spaces
D2 = metric.lp(lonborg.fdata) #Distance L2 between curves
crit2 = apply(D2^2, 1, sum)
which.min(crit2) #7

# Medidas de localización y profundidad
set.seed(987277)
# Fraiman-Muniz Depth / Integrated depth
fmd=depth.FM(lonborg.fdata)
# Modal depth 
md=depth.mode(lonborg.fdata,h="0.10") 
# random projection depth
RPd=depth.RP(lonborg.fdata,nproj=10) 
# Random Tukey depth
RTd=depth.RT(lonborg.fdata,nproj=15) 

print(cur <- c(fmd$lmed, md$lmed, RPd$lmed, RTd$lmed))
# resultados
# visualización 
plot(lonborg.fdata, type = "l",col="lightgrey",xlab="Tempo (meses)",ylab="Media Temp (ºC)",main=" ")
lines(lonborg.fdata[cur], lwd = 3, lty = 1:4, col = 1:4)
legend("topleft", c("FMD", "MD", "RPD", "RTD"), lwd = 2, lty = 1:4,col = 1:4)


# Outliers
md.out = depth.mode(lonborg.fdata)
which.min(md.out$dep)
plot(lonborg.fdata, type = "l", col = "gray", lty = 1)
lines(md.out$median, col = 2, lwd = 3,
      lty = 1)
lines(md.out$mtrim, col = 3, lwd = 3,
      lty = 2)
lines(lonborg.fdata[which.min(md.out$dep),],
      col = 4, lwd = 3, lty = 3)
legend("topright", c("Deepest", "T_25%", "Out"), lty = 1:3, col=2:4, lwd = 2)


# Dispersión
barx = func.mean(lonborg.fdata)
print(vsn <- mean(metric.lp(lonborg.fdata, barx)^2))

# Bandas de confianza por bootstrap
mean.boot=fdata.bootstrap(lonborg.fdata,statistic=func.mean,nb=1000,draw=TRUE)
mean.trim=fdata.bootstrap(lonborg.fdata,statistic=func.trim.FM,nb=1000,draw=TRUE)


#Outliers(Febrero-Bande, 2007)
out.FM = outliers.depth.trim(lonborg.fdata, nb = 1000, smo = 0.05, trim = 0.03,dfunc = depth.FM)
out.FM$outliers
which(out.FM$Dep<out.FM$quantile)
plot(lonborg.fdata, type = "l", col = "gray", lty = 1,ylab="Media Temp (ºC)", xlab = "Tempo (meses)", main =" ")
lines(lonborg.fdata[which(out.FM$Dep<out.FM$quantile),],
      col = 2, lwd = 3, lty = 2)



#-------------------------------#


# ------------------------- # 

## REGRESIÓN ## 


# Aplicar regresión coas variables: CTD.TEMPERATURE + CTD.SALINITY

temp=lonborg.fdata
length(temp);nrow(temp)
temp.pc=create.pc.basis(temp,1:3)
apply(temp.pc$coefs,2,range)
DOC<-y[,1]
ldatm=ldata(df=data.frame(DOC=DOC),Temp=temp)

# ----------- # 
# MODELO DE REGRESIÓN LINEAL
b.x=list(Temp=create.pc.basis(temp,1:3))
res.lm=fregre.lm(DOC~Temp,data=ldatm,basis.x=b.x)

par(mfrow=c(2,2))
summary(res.lm)
plot(res.lm)
par(mfrow=c(1,1))
# RMSE: sqrt(varianza)
sqrt(sum((res.lm$residuals)^2)/length(res.lm$residuals))

# ----------- # 
# MODELO LINEAL CON COVARIABLES
# SALINITY OBJETO FDATA
lonborg.sal<- lonborg %>% group_by(Location,MONTH) %>% summarise(meanSal = mean(CTD.SALINITY))
print(lonborg.sal,n=30)

matriz.lonborg.sal<- lonborg.sal %>%
  pivot_wider(names_from = MONTH, values_from = meanSal)
matriz.lonborg.sal<-as.data.frame(matriz.lonborg.sal) 
matriz.lonborg.sal<-na.omit(matriz.lonborg.sal); matriz.lonborg.sal
#matriz.lonborg<-as.matrix(matriz.lonborg); matriz.lonborg
rownames(matriz.lonborg.sal)<- matriz.lonborg.sal$Location
matriz.lonborg.sal<-matriz.lonborg.sal[,-1]
nrow(matriz.lonborg.sal)
month<-as.vector(1:12)
matriz.lonborg.sal
lonborg.sal.fdata<-fdata(mdata=matriz.lonborg.sal, argvals=month, rangeval = c(1,12))

Sal=lonborg.sal.fdata
ldatm=ldata(df=data.frame(DOC=DOC),Temp=temp,Sal=Sal)
sal.pc=create.pc.basis(Sal,1:3)
summary(sal.pc)

b.x=list(Temp=create.pc.basis(temp,1:3),Sal=create.pc.basis(Sal,1:3))

res.lmt=fregre.lm(DOC~Temp+Sal,data=ldatm,basis.x=b.x)

par(mfrow=c(2,2))
summary(res.lmt)
plot(res.lmt)
par(mfrow=c(1,1))

#RMSE modelo lmt
sqrt(sum((res.lmt$residuals)^2)/length(res.lmt$residuals))

# ------------------------- # 

# REGRESIÓN GLS: generalized least squares
acf(DOC)
pacf(DOC)

res.gls=fregre.gls(DOC~Temp,data=ldatm,correlation=corAR1(),basis.x=b.x)
summary(res.gls)
plot(res.gls)

plot(res.gls$beta.l[["Temp"]],main=" ", xlab="Tempo (meses)")
lines(res.lm$beta.l[["Temp"]],col="blue",lwd=2)
legend("topright",legend=c("Modelo GLS","Modelo LM"),lty=1,lwd=2,col=c("black","blue"))

res.gls$df.residual
res.lm$df.residual

#RMSE modelo gls
sqrt(sum((res.gls$residuals)^2)/length(res.gls$residuals))

# ------------- #

# MODELO PARCIALMENTE LINEAL
Sal<-y[,2]
ldatm=ldata(df=data.frame(DOC=DOC,Sal=Sal),Temp=temp)
# añadimos variable sal 
res.plm=fregre.plm(DOC~Sal+Temp,data=ldatm)
summary(res.plm)
# ojo que cogemos la ventana mínima

#RMSE modelo parcial lineal
sqrt(sum((res.plm$residuals)^2)/length(res.plm$residuals))

# ------------ #

# MODELO NO LINEAL
res.np=fregre.np.cv(temp,DOC)
names(res.np)
summary(res.np)

#RMSE no lineal
sqrt(sum((res.np$residuals)^2)/length(res.np$residuals))
# -------------- #

# MODELOS GLM
Sal=lonborg.sal.fdata
ldatm=ldata(df=data.frame(DOC=DOC),Temp=temp,Sal=Sal)
b.x=list(Temp=create.pc.basis(temp,1:3),Sal=create.pc.basis(Sal,1:3))

res.glm=fregre.glm(DOC~Temp+Sal,data=ldatm,family=gaussian(),basis.x=b.x)
summary(res.glm)

par(mfrow=c(2,2))
plot(res.glm)
par(mfrow=c(1,1))

pred.values<-data.frame("pred.DOC"=res.glm$fitted.values)
plot(ldatm$df$DOC,pred.values$pred.DOC,xlab="Valores DOC",ylab="Predicciones",ylim=c(50,200),xlim=c(50,200))
lines(x=seq(40,210,by=0.05),y=seq(40,210,by=0.05),col="blue")

par(mfrow=c(1,2))
plot(res.glm$beta.l[["Temp"]])
plot(res.glm$beta.l[["Sal"]])

#RMSE modelo GLM
sqrt(sum((res.glm$residuals)^2)/length(res.glm$residuals))

# ----------------- # 
# MODELO ADITIVO (GAM)
res.sam=fregre.gsam(DOC~s(Temp)+s(Sal),family=gaussian(),data=ldatm,basis.x=b.x)
summary(res.sam)

par(mfrow=c(3,2))
plot(res.sam,ylim=c(-100,100))
par(mfrow=c(1,1))
# RMSE mod GAM
sqrt(sum((res.sam$residuals)^2)/length(res.sam$residuals))

# ---------------- # 

# GKAM: Modelo generalizado aditivo kernel
res.gkam=fregre.gkam(DOC~Temp+Sal,data=ldatm) 
#por defecto utiliza la métrica del espacio de gilbert
# estimar la distancia entre variables y probar diversas ventanas
summary(res.gkam)

#RMSE mod GKAM
sqrt(sum((res.gkam$residuals)^2)/length(res.gkam$residuals))

# ------------------------ #

# REGRESIÓN FUNCIONAL CON RESPUESTA FUNCIONAL

# salinidade como fdata
Sal
# temperatura como fdata
temp

# ahora convertimos DOC como fdata
lonborg.doc<- lonborg %>% group_by(Location,MONTH) %>% summarise(meanDOC = mean(DOC))
print(lonborg.doc,n=30)

matriz.lonborg.doc<- lonborg.doc %>%
  pivot_wider(names_from = MONTH, values_from = meanDOC)
matriz.lonborg.doc<-as.data.frame(matriz.lonborg.doc) 
matriz.lonborg.doc<-na.omit(matriz.lonborg.doc); matriz.lonborg.doc
#matriz.lonborg<-as.matrix(matriz.lonborg); matriz.lonborg
rownames(matriz.lonborg.doc)<- matriz.lonborg.doc$Location
matriz.lonborg.doc<-matriz.lonborg.doc[,-1]
nrow(matriz.lonborg.doc)
month<-as.vector(1:12)
matriz.lonborg.doc
lonborg.doc.fdata<-fdata(mdata=matriz.lonborg.doc, argvals=month, rangeval = c(1,12))

doc=lonborg.doc.fdata
doc

# Crear una base funcional B-spline
rangeval <- range(temp$argvals)  # Rango de los datos funcionales
num_basis <- 8                 # Número de funciones base
b.x <- create.bspline.basis(rangeval, nbasis = num_basis)
num_basis <- 8
b.t <- create.bspline.basis(rangeval, nbasis = num_basis)

res.fr = fregre.basis.fr(temp,doc,basis.s = b.x, basis.t = b.t)
par(mfrow=c(1,2))
plot(doc[1:10],lty=1,lwd=2)
plot(res.fr$fitted.values[1:10],lty=2,lwd=2)

par(mfrow=c(1,1))
plot(res.fr$alpha.est)
plot(res.fr$beta.estbifd)

# ------------------------------- # 





# ------------------------------- # 

# CLASIFICACIÓN SUPERVISADA

# Variable DOC > 150umol/L 
# convertimos variable DOC a categórica (1,2)
DOC2 = data.frame(DOC=DOC,grupo=0)
for (i in 1:nrow(DOC2)) {
  if (DOC2$DOC[i] < 100) {
    DOC2$grupo[i] <- 1
  }
  else {
    DOC2$grupo[i] <- 2
  }
}
DOC2$grupo <- as.factor(DOC2$grupo)
# bases PC's
b.x=list(Temp=create.pc.basis(temp,1:3))

# Para hacer clasificación y luego predicción, vamos a seleccionar una muestra del 80% para entrenar el modelo y el 20% restante para testearlo
set.seed(1)
nobs <- nrow(ldatm$df)
itrain <- sample(nobs, 0.7 * nobs)

#datos train
ldatm2 = ldata(df= data.frame(grupo = DOC2$grupo[itrain]), temp=temp[itrain,])
#datos test
ldatmnew <- list(df = data.frame(gr = rep(NA, nrow(temp[-itrain,]))), temp = temp[-itrain,])

#gráfico
plot(ldatm2$temp,col=c(2:3)[ldatm2$df$grupo])
legend("topleft",legend=c("Grupo 1","Grupo 2"),lty=1,lwd=2,col=c(2:3),cex=0.7)

# ---------- # 

# CLASIFICACIÓN

# ESTIMACIÓN KERNEL
res.knn=classif.knn(DOC2$grupo[itrain],temp[itrain,])
table(res.knn$group,res.knn$group.est)

pr.knn=predict(res.knn,ldatmnew$temp,type="prob")
table(DOC2$grupo[-itrain],pr.knn$group.pred)

# ---- #
res.np=classif.np(DOC2$grupo[itrain],temp[itrain,])
table(res.np$group,res.np$group.est)

pr.np=predict(res.np,ldatmnew$temp,type="prob")
table(DOC2$grupo[-itrain],pr.np$group.pred)

# -------------------- # 
# clasificación modelos xeneralizados
res.glm=classif.glm(grupo~temp,data=ldatm2,basis.x=b.x)
pr.glm=predict(res.glm,ldatmnew,type="prob")
table(DOC2$grupo[-itrain],pr.glm$group.pred)

res.gsam=classif.gsam(grupo~s(temp),data=ldatm2,basis.x=b.x)
pr.gsam=predict(res.gsam,ldatmnew,type="prob")
table(DOC2$grupo[-itrain],pr.gsam$group.pred)

res.gkam=classif.gkam(grupo~temp,data=ldatm2,basis.x=b.x)
pr.gkam=predict(res.gkam,ldatmnew,type="prob")
table(DOC2$grupo[-itrain],pr.gkam$group.pred)


# --------------------- # 

# Clasificación nnet: redes neuronales
library(nnet)
set.seed(1)
res.nnet=classif.nnet(grupo~temp,data = ldatm2,basis.x = b.x ,type ="1vsall")
# lo importante es: group.est
table(res.nnet$group,res.nnet$group.est)

pr.nnet=predict(res.nnet,ldatmnew,type="prob")
table(DOC2$grupo[-itrain],pr.nnet$group.pred)


#clasificación rpart (non incluída no documento)
set.seed(1)
res.rpart = classif.rpart(grupo~temp, data = ldatm2, basis.x = b.x,type ="1vsall")
table(res.rpart$group,res.rpart$group.est)

pr.rpart=predict(res.rpart,ldatmnew,type="prob")
table(DOC2$grupo[-itrain],pr.rpart$group.pred)

# ---------------- # 

# Profundidade datos multivariantes
# DD-Plot
par(mfrow=c(1,2))
res.DD = classif.DD(group = ldatm2$df$grupo, fdataobj = ldatm2$temp, depth = "mode", classif = "glm")
res.DD1 = classif.DD(group = ldatm2$df$grupo, fdataobj = ldatm2$temp, depth = "mode", classif = "knn")
par(mfrow=c(1,1))

pr.DD=predict(res.DD,ldatmnew$temp,type="prob")
table(DOC2$grupo[-itrain],pr.DD$group.pred)

pr.DD1=predict(res.DD1,ldatmnew$temp,type="prob")
table(DOC2$grupo[-itrain],pr.DD1$group.pred)


# ---------------------- # 

# CLUSTERING
plot(temp)
set.seed(1)
res.km = kmeans.fd(temp,ncl=2)
table(DOC2$grupo,res.km$cluster)

# ---- # 

library(fda.usc)
source("fDBSCAN.R")
rmeanshift = fmeanshift(temp,h=-0.25)
table(rmeanshift$cluster)

plot(rmeanshift$centers)
table(DOC2$grupo,rmeanshift$cluster)


# --------------------------- # 

# CONTRASTES DE HIPÓTESIS EN FDA

bajoDOC<- temp[DOC2$grupo == 1]
altoDOC<- temp[DOC2$grupo == 2]

# Contraste medias 
set.seed(1)
(fmean <- fmean.test.fdata(bajoDOC,altoDOC))

# contraste covarianzas
set.seed(1)
(fcov <- cov.test.fdata(bajoDOC,altoDOC))

# Contraste distribuciones
set.seed(1)
(fdist<- fEqDistrib.test(bajoDOC,altoDOC))

# Contraste proyecciones
(fproy <-XYRP.test(bajoDOC,altoDOC,nproj=3,npc=5))
    ?XYRP.test

# Contraste anova
set.seed(1)
anovapho=fanova.onefactor(temp,factor(DOC2$grupo),nboot=1000,plot=TRUE)
anovapho


# test lineal
resflm=flm.test(temp,DOC,B=1000)
resflm


# ------------------------------------ # 

# FIN DO SCRIPT EMPREGADO PARA REALIZAR O TRABALLO FINAL DA MATERIA DE DATOS FUNCIONAIS

