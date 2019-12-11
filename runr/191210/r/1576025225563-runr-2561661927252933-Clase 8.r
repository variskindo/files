 base=read.delim("clipboard")


#===========================================
#PH para datos provenientes  de una bernulli
#===========================================

#Pregunta 1∞
#=========
#Considerando los datos de los entrevistados del distrito de Comas, 
#pruebe si:
#1. La proporciÛn de personas que opinan que la atenciÛn dentro 
#del municipio es pÈsima es diferente al 10%.

table(base$Distrito,base$OpiniÛn)
prop.test(10,50,0.10,alternative = "t",correct =F)
#Correct= Metodo aproximado
#Valor X^2 = 5.556
sqrt(5.5556)
#=2.357 COINCIDE CON MINITAB: Estadisticas, estadisticas basicas>
#>1 PROPORCION
#RECORDAR QUE Z^2 =(X)^2:  
#Z: Distribucion normal
#X^2: Distribucion chi-cuadrado
#Hallando el Chi- critico
qnorm(0.025)
qchisq(0.025,9)

#Conclusion 
#==========
#A un \alpha = 0.05, se puede afirmat que la proporcion de 
#contribuyentes del distrito de comas que opinan que la atenciÛn 
#dentro del municipio es pesima es direnete del 10% 

#En la pracica si la muestra es grande es indistinto considerar 
#metodo aproximado o exacto

#la prueba exacta se optiene con:
binom.test(10,50,p=0.10, alternative = "t", conf.level = 0.95)
#Minitab no arroja el mismo p-value, si el mismo intervalo de confianza

#==========================================
#PH para datos provenientes  de una poisson
#==========================================
#Pregunta 2∞
#==========
#Considerando los datos de los entrevistados del distrito 
#de Comas, pruebe si:
#2. El n˙mero medio de delitos sufridos dentro del distrito 
#a miembro de la familia el aÒo 2017 es mayor a 3.


#Realizando conminitab
#>Poison de una muestra: COLUMNA ROBO: TASA HIPOTETIZADA=3
#Z=20.98
#0.000 #Pvalues <0.05  SE RECHAZA
comas= subset(base, base$Distrito=="Comas")
#RECUERDA QUE ESTE TEST NO DALA PARA LA MEDIA POR ELLO 
#ANTERIORMENTE LO DIVIDIAMOS ENTRE "N(50 PARA COMAS)"
#150= 50*3
poisson.test(sum(comas$Robos), r=150, alternative = "g")
qnorm(0.95)
#recordar que r da el valor exacto
#P-VALUE DICE QUE ES MENOR A UN NUMERO PEQUE√ëO
#CONCLUSI√ìN
#A un \alpha 0.05 se puede afirmar que el numero medio de 
#delitos satisface dentro del distrito a algun miembro de 
#la familia en el distrito de comas es mayor a 3

#==============================================
#PH para datos provenientes  de una Exponencial
#==============================================
#pREGUNTA 3¬∞
#Esta funcion solo sirve para PH unilateral izquierda
ph.datExp = function(datos, mu, alfa){
  n=length(datos)
  xbar=mean(datos)
  z= (xbar-mu)*sqrt(n)/mu
  pval=pnorm(z)
  zcrit=qnorm(alfa) #z critico
  return(list(z=z, pval=pval, zcrit=zcrit))
}

ph.datExp(comas$AtenciÛn,30,0.05)

#=======================================
#Pruebas con dos muestras independientes
#=======================================

#==========================
#ph para razon de varianzas
#==========================
#pREGUNTA 4¬∞
#2.1
#Pruebas de normalidad
shapiro.test(olivos$Pago.2017)
shapiro.test(sanmartin$Pago.2017)
#si se tine un pvalue cercano a uno es de esperarse que la distribucion
#se aproxime a una normal

olivos =subset(base, base$Distrito=="Los Olivos")
sanmartin= subset(base, base$Distrito=="San Mart√≠n de Porras")

var.test (olivos$Pago.2017, sanmartin$Pago.2017, alternative = "t")
qf(0.025,29,39)
qf(0.975,29,39)
#¬øC√≥mo calcular el pvalor de prueba F?
qf(0.5,29,39)
2*(1-pf(1.2263,29,39))

#Caso inverso
var.test (sanmartin$Pago.2017, olivos$Pago.2017, alternative = "t")
qf(0.025,39,29)
qf(0.975,39,29)

#Conclusion
#A un \alpha=0.05 no se puede afirmar que la varianza del pago 
#de impuestos en el a√±o 2017 en los distritos de los olivos
#y San Martin de Porras son heterogeneas

#===============================
#ph para la diferencia de medias
#===============================

t.test(olivos$Pago.2017, sanmartin$Pago.2017, mu=0, paired = F,
       var.equal = T, alternative = "g")
#Conclusion
#A un \alpha=0.05 se puede afirmar que el pago medio de impuestos
#en el a√±o 2017 fue superior en el distrito de los Olivos que en el
#Distrito de San martin de Porres
qt(0.95,68)


#Analizando el caso inverso
t.test(sanmartin$Pago.2017,olivos$Pago.2017, mu=0, paired = F,
       var.equal = T, alternative = "l")
qt(0.05,68)


#===============================
#ph para bernulli dos muestra
#===============================

#Pregunta 6 (Ver Minitab)
table(base$Distrito, base$Opini√≥n)
#olivos 9 de 50
#san maritin 6 de 30
#en minitab
#Estadisticas>Estadisticas basicas>2PROPORCIONES>DATOS RESUMIDOS
#CONCLUSION
#A UN \ALPHA =0.05, no se puede afirmar que la proporci√≥n de 
#contribuyentes que opinan que la atenci√≥n es excelente es mayor 
#en el distrito de comas que en los olivos

#EN minitab esta pendiente hablar de la prueba exacta de fisher
#note que es agrupado ya que la fierencia es cero

#===============================
#ph para poisson dos muestra
#===============================
table(base$Distrito)
by(base$Robos, base$Distrito,sum)

#Siguiente preguntas
#Estadistica >Estadistica b√°sica<POISSON 2 MUESTRAS>
#2OPCIONES
# Hacer un tapply con R
# MUESTRA1 MUESTRA2
# 30        40
# 111       249

#lA OTRA OPCION ES, DADO QUE SE QUIERE ANALIZAR OLIVOS Y SMP
#SUBSET DE OLIVOS EN MINITAB
#En el subset de los olivos pegar solo los robos de smp 
#en una columna aparte luego
#Estadisticas > Estadisticcas basicas > 2poison
#cada valor en su columna
#Robos de olivos y robo de smp

#Ambos deben salir lo mismo








