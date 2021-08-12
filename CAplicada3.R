library(readxl)
acueducto_Vs_Vivienda <- read_excel("E:/Descargas/acueducto Vs Vivienda.xlsx")
#acueducto_Vs_Vivienda <- data.frame(acueducto_Vs_Vivienda)
pago<-acueducto_Vs_Vivienda$`Ultimo pago acueducto`
pago<-na.omit(pago)
filas <- which(pago > 4e+05)
pago <- pago[-filas]
filas1 <- which(pago < 10000)
pago <- pago[-filas1]

install.packages("StatMeasures")
library(StatMeasures)
atipicos <- outliers(pago)
atipicos
pago <- pago[-atipicos$idxOutliers]
hist(pago, breaks= 50, main="Ultimo pago acueducto", 
     xlab="Ultimo pago acueducto", ylab="Frencuencia absoluta", 
     col= "cyan", freq = FALSE) # freq = FALSE hace la gráfica de 0 y 1
lines(density(pago), col = "blue", lwd = 3)
boxplot(pago)

install.packages("BiocManager")
BiocManager::install(c("genefilter"))
install.packages("modeest")
library(modeest)
moda = mlv(pago, method = "mfv") # Para la moda
moda

abline(v = mean(pago), col = "red", lwd = 2)
abline(v = median(pago), col = "purple", lwd = 2)
abline(v = moda, col = "brown", lwd = 2)


n = length(pago)
sqrt((n-1)/n)*sd(pago)/mean(pago) #Desviacion estandar media
sqrt(mean((pago - median(pago))^2))/median(pago) #Desviacion estandar mediana 
sqrt(mean((pago - mean(pago))^2))/mean(pago) # Es mejor la media 
sd(pago)/mean(pago)

install.packages("nortest")
library(nortest)
ad.test(pago)

media <- function(data, indices){
  return(mean(data[indices]))
}
media(data = pago, indices = c(1, 2, 3))

install.packages("boot")
library(boot)
submuestras = boot(data= pago, statistic = media, R= 1000) 
submuestras
hist(submuestras$t, breaks = 50, freq = F, col = "red") # La distribución del bootstrap 
lines(seq(min(submuestras$t), max(submuestras$t), 0.5), 
      dnorm(seq(min(submuestras$t), max(submuestras$t), 0.5), mean = 64033.39 ,
            sd =  349.3944), col = "blue", lwd = 3)
shapiro.test(submuestras$t) # Para probar normalidad 

varianza <- function(data, indices){
  return(((n - 1)/n)*var(data[indices]))
}
varianza(pago, c(1, 2, 3))
replicas = boot(data=pago, statistic = varianza, R= 5000) # Tomamos las muestras para 
hist(replicas$t, breaks = 100, freq = F, main = "Varianza vìa Bootstrap")
c1 = quantile(replicas$t, 0.05/2)
c2 = quantile(replicas$t, 1- 0.05/2)
abline(v = c1, col = "red", lwd = 3)
abline(v = c2, col = "red", lwd = 3)
abline(v = n*349.3944*349.3944, col = "blue", lwd = 3)

boot.ci(submuestras, type = "perc")
boot.ci(submuestras, type = "norm")
boot.ci(submuestras, type = "bca") # No hay suficiente memoria 
submuestras

install.packages("BSDA") # Para el test del signo
library(BSDA)
SIGN.test(pago, conf.int = T)
library(stats)    # Para el test de Wilcoxon
wilcox.test(pago, conf.int = T)

install.packages("pwr")
library(pwr)
cohen.ES(test = "t", size = "medium")
potencia <- pwr.t.test(n = 200, d = 0.5, 
                       sig.level = 0.05, alternative = "two.sided") # Bajo normalidad 
plot(potencia)
