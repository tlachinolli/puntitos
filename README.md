# puntitos

## Instructions
1. Select a radio button to insert the correspondant figure vertex set (The first figure is centered at the origin)
2. Tranform the figure vertex set by using the sliders"
3. Use Insert button to fix the figure in place
4. Repeat 1-3 to insert as many figures as you want (subsequent figures share the same vertex as the last figure inserted;    just make a tranformation after inserting and you'll see. You can center the new figure by using the Origin button)
5. Export your graph by using the Print button

* Clear button will erase every inserted figure!
* The solution is added at the end of exported file!

## Hace falta

### Implementar figura de muestra.

Antes de la gráfica de vértices. mostrar la información de el número de figuras insertadas y el tipo. me imagino algo así

2 (figura de triangulo rectangulo)  4 (figura de triangulo acutangulo)  2 (figura de cuadrado)

donde lo que va entre parentesis se sustituya por una grafica real de la figura en cuestión.

![rtpsExampleFigures](https://user-images.githubusercontent.com/97901291/149855778-6569b4ee-f767-4500-ad55-49156f64c06c.png)

( esto va servir para indicar al estudiante como se deberían ver las figuras que va buscar en el diagrama de puntos)

### Bloque de código de botón print

El botón print deberá permitirte descargar un archivo con el la gráfica de los vértices y la solución (al final de la hoja o en otra).

### Sección de "modo infinito" (en otra pestaña, poodría ser)

En donde eliges cuantas gráficas generar. Las gráficas se generan automaticamente. 

#### Ejemplo

Eliges 9 gráficas. Se generan automáticamente y se muestran (3 columnas 3 renglones) listas para imprimir en una hoja así como su hoja de respuestas correspondiente.


## Otros viajes
### Plotly

Quería ver como se verían las respuestas en un diagrama de bolitas en plotly pero en 3d. Dandole una altura a cada pieza en correspondencia con su "type" (cuadrado = 1, trianguloA =2, trianguloB = 3). (En la versión antigua de app.R estaba un código que no generaba nada cuando lo quería utilizar. Puedes ver versiones antiguas?)

### Mas Figuras

Se pueden ir diseñando otros conjuntos de vértices base más complejos para agregar dificultad (en forma de flecha, estrella, etc)
