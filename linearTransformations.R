# linearTransformations.R
#### Linear Transformations
#	función que crea cuadrado base con centro en el origen O(0,0) 
#   y con longitud de lado igual a 2 –tan solo pareció ser un buen modelo base–
#	regresa una matríz con sus vértices
crearCuadrado <- function(){	# Actualizar a crearFigura con polimorfismo
	x <- c(1, - 1,- 1, 1)
	y <- c(1, 1, - 1, - 1)
	cuadrado <- cbind(x, y)
	return(cuadrado)
}
### para evitar la creación de una función constructora para cada figura
### se implementará un método con polimorfismo
crearTrianguloIsoceles <- function(){	# Actualizar a crearFigura con polimorfismo
	x <- c(0, - 1, 1)
	y <- c(1 , - 1, - 1)
	triangulo <- cbind(x, y)
	return(triangulo)
}

crearFigura <- function(iFig){
  if(iFig == 1) return(crearCuadrado())
  if(iFig == 2) return(crearCuadrado()[-4,])
  if(iFig == 3) return(crearTrianguloIsoceles())
}
# Función que traslada una figura horizontalmente
trasladarFiguraHorizontal <- function(nombreFigura, parametroT){
	x <- nombreFigura[,1]
	y <- nombreFigura[,2]
	x <- x + parametroT
	figuraTrasladada <- cbind(x,y)
	return(figuraTrasladada)
}

# Función que traslada una figura verticalmente
trasladarFiguraVertical <- function(nombreFigura, parametroT){
	x <- nombreFigura[,1]	
	y <- nombreFigura[,2]
	y <- y + parametroT
	figuraTrasladada <- cbind(x,y)
	return(figuraTrasladada)
}

# Función que rota una figura nombreFigura cierto numero de grados –positivos(CQW) o negativos(QW)–
rotarFigura <- function(nombreFigura, grados){
	#convierte parámetro grados a radianes para utilizar funciones trig
	angle <- grados * (pi / 180)
	# crea matriz de tranformación -rotación-
	M <- matrix( c(cos(angle), - sin(angle), sin(angle), cos(angle)), 2, 2 )
	figuraRotada <- nombreFigura %*% M # se aplica la transformación lineal
	return(figuraRotada) # regresa matriz con vertices afectados bajo la rotacion
}
