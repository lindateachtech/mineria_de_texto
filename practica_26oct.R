
# Capitulo 38 -------------------------------------------------------------

# este comando solo necesita ser ejecutado una vez
# si el paquete remotes no está instalado, descomentar para instalarlo

# install.packages("remotes")
library(remotes)
install_github("cdr-book/CDR")

# Para el error Using GitHub PAT from the git credential store. Error: Failed to install 'unknown package' from GitHub:
# gitcreds::gitcreds_delete()



# Ejemplo de aplicación ---------------------------------------------------


## Declaración Institucional del Estado de Alarma 2020 ---------------------

# Cargar objeto de análisis
# Declaración institucional del presidente del Gobierno anunciando el Estado de Alarma en la crisis del coronavirus
library("CDR")
data("declaracion")


## Segmentación en palabras y oraciones ------------------------------------

# Cargar el paquete 'tokenizers' para realizar la tokenización (segmentación en palabras)
library("tokenizers")

# Tokenizar el texto en palabras, dividiéndolo en secuencias de caracteres entre espacios en blanco
palabras <- tokenize_words(declaracion)

# Contar el número total de palabras en el texto
count_words(declaracion)

# Crear una tabla de frecuencias de las palabras tokenizadas
library("tidyverse")
tabla <- table(palabras[[1]])

# Convertir la tabla en un tibble y ordenar por recuento en orden descendente
tabla <- tibble(
    palabra = names(tabla),
    recuento = as.numeric(tabla)) %>% 
        arrange(desc(recuento))

tabla %>% view()

# Tokenizar el texto en oraciones
oraciones <- tokenize_sentences(declaracion)

# Contar el número total de oraciones en el texto
count_sentences(declaracion)

# Obtener las primeras tres oraciones y la última del texto
oraciones[[1]][1:3] # primeras 3 oraciones
oraciones[[1]][count_sentences(declaracion)] # última oración

# Tokenizar las palabras por oración
palabras_oracion <- tokenize_words(oraciones[[1]])

# Calcular el número de palabras en cada oración usando sapply
longitud_o <- sapply(palabras_oracion, length)

# Mostrar las primeras longitudes
head(longitud_o)

# Crear un data frame con el número de oración y el recuento de palabras
df_oraciones <- data.frame(oracion = 1:length(longitud_o), palabras = longitud_o)

# Visualizar el número de palabras en cada oración de la Declaración
ggplot(df_oraciones, aes(x = oracion, y = palabras)) +
    geom_line() +
    geom_smooth() +
    geom_point() +
    labs(x = "Número de oración", y = "Número de palabras", 
         title = "Número de palabras en cada oración de la Declaración") +
    theme_minimal()



## Análisis Exploratorio ---------------------------------------------------

### Eliminación de palabras vacías ------------------------------------------

# Cargar el paquete stopwords para eliminar las palabras vacías
library("stopwords")

# Crear una tabla con las palabras vacías en español
tabla_stopwords <- tibble(palabra = stopwords("es"))

# Eliminar las palabras vacías de la tabla original
tabla <- tabla %>% 
    anti_join(tabla_stopwords)
knitr::kable(tabla[1:10, ],
             caption = "Palabras más frecuentes (sin palabras vacías)"
)


### Nube de palabras --------------------------------------------------------

# Fijar la semilla para la reproducibilidad del gráfico
set.seed(12)

# Cargar el paquete 'wordcloud' para generar nubes de palabras
library("wordcloud")

# Generar una nube de palabras, mostrando las palabras más frecuentes
wordcloud(tabla$palabra, tabla$recuento, max.words = 50, colors = rainbow(3))
 

## Análisis de sentimientos y detección de emociones -----------------------

### Lexicón bing ------------------------------------------------------------

# Traducción automática al inglés de la Declaración.
data("EN_declaracion")

# Generamos el objeto tabla, y replicamos el procedimiento descrito 
# anteriormente de preparación, limpieza, segmentación en palabras y 
# eliminación de palabras vacías (obviamente, en idioma inglés).
tabla <- table(tokenize_words(EN_declaracion)[[1]])

tabla <- tibble(
    word = names(tabla),
    recuento = as.numeric(tabla)
)

tabla <- tabla  %>% 
    anti_join(tibble(word = stopwords("en"))) %>% 
    arrange(desc(recuento))


# Cargar el paquete tidytext para el análisis de sentimientos
library("tidytext")

# Obtener el lexicón de sentimientos positivos de bing
pos <- get_sentiments("bing") %>% 
    filter(sentiment == "positive")

# Filtrar las palabras positivas en el texto en inglés traducido automáticamente
pos_EN <- tabla %>% 
    semi_join(pos)

pos_EN %>% view()



### Lexicón NRC -------------------------------------------------------------

# Obtener el lexicón NRC
emo <- get_sentiments("nrc")

# Realizar un gráfico de barras para visualizar la frecuencia de las emociones en el texto
emo %>%  ggplot(aes(sentiment)) +
    geom_bar(aes(fill = sentiment), show.legend = FALSE)

# Tabla de frecuencias por emociones y sentimientos:
emo_tab <- tabla %>%  inner_join(emo)
head(emo_tab, n = 7)
emo_tab %>% view()

# Algunas palabras tienen asociados distintos sentimientos; por ejemplo, 
# resources
emo_tab %>% 
    count(sentiment) %>% 
    ggplot(aes(x = sentiment, y = n)) +
    geom_bar(stat = "identity", aes(fill = sentiment), show.legend = FALSE) +
    geom_text(aes(label = n), vjust = -0.25)

# Cargar la librería 'syuzhet', que se usa para realizar análisis de sentimientos
library("syuzhet")

# Tokenizar el texto en inglés (EN_declaracion) en palabras individuales
palabras_EN2 <- get_tokens(EN_declaracion)

# Utilizar la función 'get_nrc_sentiment' para analizar los sentimientos de cada palabra
# con base en el lexicón NRC (que incluye emociones como ira, anticipación, disgusto, etc.)
emo_tab2 <- get_nrc_sentiment(palabras_EN2, lang = "english")

# Crear un vector 'emo_vec' que contiene, para cada emoción seleccionada, las palabras
# correspondientes en forma de texto unido por espacios. Cada elemento del vector
# representa las palabras asociadas a una emoción específica: ira, anticipación y disgusto.
emo_vec <- c(
    paste(palabras_EN2[emo_tab2$anger > 0], collapse = " "),         # Palabras asociadas con 'ira'
    paste(palabras_EN2[emo_tab2$anticipation > 0], collapse = " "),  # Palabras asociadas con 'anticipación'
    paste(palabras_EN2[emo_tab2$disgust > 0], collapse = " ")        # Palabras asociadas con 'disgusto'
)

# Cargar la librería 'tm' para trabajar con matrices de términos y documentos
library("tm")

# Crear un corpus a partir del vector de emociones ('emo_vec'), donde cada documento es
# una lista de palabras asociadas a una emoción específica (ira, anticipación, disgusto)
corpus <- Corpus(VectorSource(emo_vec))

# Crear una matriz de términos y documentos (TDM), donde las filas son términos y
# las columnas son las emociones (ira, anticipación, disgusto)
TDM <- as.matrix(TermDocumentMatrix(corpus))

# Nombrar las columnas de la matriz para que correspondan con las emociones
colnames(TDM) <- c("anger", "anticipation", "disgust")

# Fijar la semilla para la reproducibilidad del gráfico
set.seed(1)

# Crear una nube de palabras comparativa utilizando 'comparison.cloud'
comparison.cloud(TDM,
                 random.order = FALSE,
                 colors = c("firebrick", "forestgreen", "orange3"),
                 title.size = 1.5, scale = c(3.5, 1), rot.per = 0
)


## N-gramas ----------------------------------------------------------------

# Tokenizar el texto en bigramas (pares de palabras consecutivas)
bigramas <- tokenize_ngrams(declaracion,
                            n = 2,
                            stopwords = tabla_stopwords$palabra
)

# Mostrar los primeros tres bigramas
head(bigramas[[1]], n = 3)

# Tokenizar el texto en trigramas (trío de palabras consecutivas)
trigramas <- tokenize_ngrams(declaracion,
                             n = 3,
                             stopwords = tabla_stopwords$palabra
)

# Mostrar los primeros tres trigramas
head(trigramas[[1]], n = 3)

# Se procede ahora a obtener los bigramas con tidytext. 
# Para el resto de n-gramas el procedimiento es análogo, 
# haciendo las modificaciones oportunas. 
# En el último paso se ordenan por frecuencia (de mayor a menor):
declara2 <- tibble(texto = declaracion)

bigramas <- declara2 %>% 
    unnest_tokens(bigram, texto, token = "ngrams", n = 2) |>
    count(bigram, sort = TRUE)

bigramas[1:5, ]

# Una forma de eliminar las palabras vacías es:
# Separar los bigramas en dos columnas, eliminar las palabras vacías y unirlas de nuevo
bigramas_limpios <- bigramas  %>% 
    separate(bigram, c("word1", "word2"), sep = " ") %>% 
    filter(!word1 %in% tabla_stopwords$palabra) %>% 
    filter(!word2 %in% tabla_stopwords$palabra) %>% 
    unite(bigram, word1, word2, sep = " ")

bigramas_limpios[1:5, ]


### Significado y contexto --------------------------------------------------

# En este caso, se puede ver cómo la palabra atender cambia de sentido 
# cuando va precedida de no o sin. 
# A continuación, se filtran los bigramas cuya primera palabra es no:
bigramas_no <- bigramas %>% 
    separate(bigram, c("word1", "word2"), sep = " ") %>% 
    filter(word1 == "no") %>% 
    count(word1, word2, sort = TRUE)

bigramas_no



## Análisis de redes -------------------------------------------------------

# Cargar los paquetes igraph y ggraph para el análisis de redes
library("igraph")
library("ggraph")

set.seed(1)

# Crear un gráfico de bigramas sin palabras vacías
graf_bigramas_l <- bigramas_limpios %>% 
    separate(bigram, c("first", "second"), sep = " ") %>% 
    filter(n > 1) %>% 
    graph_from_data_frame()

# Crear el gráfico de redes con ggraph
g1 <- ggraph(graf_bigramas_l, layout = "fr") +
    geom_edge_link(arrow = arrow(length = unit(4, "mm"))) +
    geom_node_point(size = 0) +
    geom_node_text(aes(label = name))

# Comparar con un gráfico de redes que incluye palabras vacías
graf_bigramas <- bigramas %>% 
    tidyr::separate(bigram, c("first", "second"), sep = " ") %>% 
    dplyr::filter(n > 2) %>% 
    graph_from_data_frame()

g2 <- ggraph(graf_bigramas, layout = "fr") +
    geom_edge_link0() +
    geom_node_point(size = 0) +
    geom_node_label(aes(label = name))

# Utilizar el paquete patchwork para combinar los gráficos
library("patchwork")

# Mostrar ambos gráficos juntos para comparación
g1 + g2

