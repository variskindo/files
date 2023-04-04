
#Ej 20
duchas = read.table("duchas.txt", header=TRUE)  # Lee datos y los guarda con ese nombre, 
el archivo debe estar ubicado en la carpeta de trabajo, si no hay que agregarle la ubicaci�n
duchas <- read.table(file='C:/Users/mi_usuario/Desktop/base2.txt',
                    header=TRUE, sep='')
flujo <- duchas[,2]
# Crear estructura de tallo y hojas b�sico en R
# funci�n
#stem(x,           # Vector num�rico
     scale = 1,    # Altura del gr�fico
     width = 80,   # Ancho del gr�fico
     atom = 1e-08) # Par�metro de tolerancia
# Ejemplo:
stem(flujo) 