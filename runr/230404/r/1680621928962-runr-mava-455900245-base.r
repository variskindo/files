
#Ej 20
duchas = read.table("duchas.txt", header=TRUE)  # Lee datos y los guarda con ese nombre, 
el archivo debe estar ubicado en la carpeta de trabajo, si no hay que agregarle la ubicación
duchas <- read.table(file='C:/Users/mi_usuario/Desktop/base2.txt',
                    header=TRUE, sep='')
flujo <- duchas[,2]
# Crear estructura de tallo y hojas básico en R
# función
#stem(x,           # Vector numérico
     scale = 1,    # Altura del gráfico
     width = 80,   # Ancho del gráfico
     atom = 1e-08) # Parámetro de tolerancia
# Ejemplo:
stem(flujo) 