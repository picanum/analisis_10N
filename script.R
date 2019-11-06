#### PREPROCESAMIENTO ####

library(pdftools)
library(tm)
library(SnowballC)
library(stringr)
library(fastmatch)

#Los programas de cada partido fueron cargados usando pdftools y almacenados en los objetos
#psoe, pp, cs, pod, mp y vox. Se retiraron saltos de línea, indíces de contenidos, cabeceras y pies de página

#Con esta función quitamos paréntesis y corchetes (por si removePunctuation fallara)
quitarparentesis <- function(x) gsub("\\s*\\([^\\)]+\\)","",as.character(x))

#Con esta otra función quitamos números, signos de puntuación y stop words (palabras vacías).
#La función que quita las stop words no es perfecta y se deja demasiados adverbios sin filtrar.
preprocesamiento <- function(x) removeWords(removePunctuation(removeNumbers(tolower(x))),stopwords("spanish"))


#Función para el procesamiento del texto
preproc <- function(texto){

  #Aplicamos las dos funciones anteriores y quitamos otros símbolos que aparecen y que no elimina removePunctuation
  texto2 <- preprocesamiento(quitarparentesis(texto))
  texto2 <- str_remove_all(texto2, "¿")
  texto2 <- str_remove_all(texto2, "¡")
  texto2 <- str_remove_all(texto2, "—")
  texto2 <- str_remove_all(texto2, "•")
  texto2 <- str_remove_all(texto2, "‘")
  texto2 <- str_remove_all(texto2, "’")
  texto2 <- str_remove_all(texto2, "“")
  texto2 <- str_remove_all(texto2, "”")
  texto2 <- str_remove_all(texto2, "«")
  texto2 <- str_remove_all(texto2, "»")

  #Y configuramos el texto restante para que nos devuelva directamente la matriz de frecuencias de términos
  DTM1 <- DocumentTermMatrix(Corpus(VectorSource(t(texto2))))
  DTM1<-as.matrix(DTM1)
  DTM1<-as.data.frame(DTM1)
  return(DTM1)
}

#Visualizamos los 20 términos más frecuentes, junto a su frecuencia, de cada programa
d_psoe <- preproc(psoe)
tail(sort(apply(d_psoe,2,sum)),20)

d_pp <- preproc(pp)
tail(sort(apply(d_pp,2,sum)),20)

d_pod <- preproc(pod)
tail(sort(apply(d_pod,2,sum)),20)

d_cs <- preproc(cs)
tail(sort(apply(d_cs,2,sum)),20)

d_mp <- preproc(mp)
tail(sort(apply(d_mp,2,sum)),20)

d_vox <- preproc(vox)
tail(sort(apply(d_vox,2,sum)),20)

#En las siguientes líneas uniremos todas las frecuencias de términos en un solo dataframe
#Obtendremos todas las palabras que se mencionan en todos los programa en total
#Después obtendremos la frecuencia de cada una en cada programa (aunque sea 0)
#Finalmente las combinaremos en un solo dataframe
palabras <- c(colnames(d_psoe), colnames(d_pp), colnames(d_pod), colnames(d_cs), colnames(d_mp), colnames(d_vox))
palabras <- names(table(palabras))

palabrizador <- function(d, palabras = palabras){
  ret <- merge(data.frame(palabras = palabras), data.frame(palabras = colnames(d), freq = apply(d,2,sum)), all.x = T)
  return(ret)
}

d2psoe <- palabrizador(d_psoe, palabras)
d2pp <- palabrizador(d_pp, palabras)
d2pod <- palabrizador(d_pod, palabras)
d2cs <- palabrizador(d_cs, palabras)
d2mp <- palabrizador(d_mp, palabras)
d2vox <- palabrizador(d_vox, palabras)

d <- data.frame(palabras = d2psoe[,1], psoe = d2psoe[,2], pp = d2pp[,2], pod = d2pod[,2],
                cs = d2cs[,2], mp = d2mp[,2], vox = d2vox[,2])

#Con este bucle pasaremos los NA's (que aparecen cuando no sale el término en el programa) a 0
#También pasaremos las frecuencias de absolutas a relativas dividiendo entre el nº total de palabras de cada programa
#(Sé que usar bucles en R no es elegante, pero tampoco me apetecía comerme el coco con do.call)
for(i in 2:7){
  d[which(is.na(d[,i])),i] <- 0
  d[,i] <- d[,i]/sum(d[,i])
}

#### GRÁFICOS ####
library(patchwork)
library(dplyr)
library(ggplot2)
library(ggwordcloud)

#Vamos a obtener los wordclouds de cada programa para aquellas palabras por encima del percentil 1 - 20/(nº de términos distintos que aparecen)
#Lo que conseguimos con esto es sacar, como mínimo, los 20 términos más repetidos. 
#Si algún(os) término(s) empata(n) en frecuencia con el 20º más repetido, también aparecerá(n)

g1 <- d %>% filter(psoe >= quantile(d$psoe,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = psoe)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "PSOE") + theme(title = element_text(colour = "red"),
                               plot.title = element_text(hjust = 0.5))

g2 <- d %>% filter(pp >= quantile(d$pp,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = pp)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "PP") + theme(title = element_text(colour = "blue"),
                             plot.title = element_text(hjust = 0.5))

g3 <- d %>% filter(cs >= quantile(d$cs,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = cs)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "Ciudadanos") + theme(title = element_text(colour = "orange"),
                                     plot.title = element_text(hjust = 0.5))

g4 <- d %>% filter(pod >= quantile(d$pod,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = pod)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "Unidas Podemos") + theme(title = element_text(colour = "purple"),
                                         plot.title = element_text(hjust = 0.5))

g5 <- d %>% filter(mp >= quantile(d$mp,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = mp)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "Más País") + theme(title = element_text(colour = "turquoise"),
                                   plot.title = element_text(hjust = 0.5))

g6 <- d %>% filter(vox >= quantile(d$vox,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = vox)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "Vox") + theme(title = element_text(colour = "green"),
                              plot.title = element_text(hjust = 0.5))

#Juntamos todos los gráficos en uno (gracias al magnífico paquete patchwork) y añadimos título y pie de imagen

(g1 + g2) / (g3 + g4) / (g5 + g6) + plot_annotation(title = "Términos más repetidos en los programas electorales de las elecciones del 10N de los principales partidos nacionales",
                                                    caption = "Fuente: programas electorales de PSOE, PP, Ciudadanos, Unidas Podemos, Más País y Vox | @Picanumeros")


#Vamos ahora a sacar los términos más comunes de cada programa, en comparación con el resto
#Para empezar, creamos una nueva columna que saque la media de frecuencias relativas de cada término entre los 6 programas
d$media <- apply(d[,2:7],1,mean)

#Y obtenemos un data.frame con el 5% de términos que más aparecen, para que los hallazgos sean relevantes
d_comp <- d[which(d$media > quantile(d$media, 0.95)),]

#Ahora con gráficos de R base podemos sacar los diagramas de barras que nos muestran los 10 que más comunes son en cada programa.
#Lo hacemos jugando con las funciones tail, sort y order:
par(mfrow=c(3,2))

barplot(tail(sort(d_comp$psoe/d_comp$media),10),
        names.arg =  tail(d_comp$palabras[order(d_comp$psoe/d_comp$media)],10),
        horiz = TRUE,las=1, xlim = c(0,6),
        main = "Palabras más repetidas en el programa del PSOE\nen comparación con el resto de partidos",
        xlab = "Nº de veces que aparece dividido entre la media de los 6 programas")

barplot(tail(sort(d_comp$pp/d_comp$media),10),
        names.arg =  tail(d_comp$palabras[order(d_comp$pp/d_comp$media)],10),
        horiz = TRUE,las=1, xlim = c(0,6),
        main = "Palabras más repetidas en el programa del PP\nen comparación con el resto de partidos",
        xlab = "Nº de veces que aparece dividido entre la media de los 6 programas")

barplot(tail(sort(d_comp$cs/d_comp$media),10),
        names.arg =  tail(d_comp$palabras[order(d_comp$cs/d_comp$media)],10),
        horiz = TRUE,las=1, xlim = c(0,6),
        main = "Palabras más repetidas en el programa de Ciudadanos\nen comparación con el resto de partidos",
        xlab = "Nº de veces que aparece dividido entre la media de los 6 programas")

barplot(tail(sort(d_comp$pod/d_comp$media),10),
        names.arg =  tail(d_comp$palabras[order(d_comp$pod/d_comp$media)],10),
        horiz = TRUE,las=1, xlim = c(0,6),
        main = "Palabras más repetidas en el programa de Unidas Podemos\nen comparación con el resto de partidos",
        xlab = "Nº de veces que aparece dividido entre la media de los 6 programas")

barplot(tail(sort(d_comp$mp/d_comp$media),10),
        names.arg =  tail(d_comp$palabras[order(d_comp$mp/d_comp$media)],10),
        horiz = TRUE,las=1, xlim = c(0,6),
        main = "Palabras más repetidas en el programa de Más País\nen comparación con el resto de partidos",
        xlab = "Nº de veces que aparece dividido entre la media de los 6 programas")

barplot(tail(sort(d_comp$vox/d_comp$media),10),
        names.arg =  tail(d_comp$palabras[order(d_comp$vox/d_comp$media)],10),
        horiz = TRUE,las=1, xlim = c(0,6),
        main = "Palabras más repetidas en el programa de Vox\nen comparación con el resto de partidos",
        xlab = "Nº de veces que aparece dividido entre la media de los 6 programas")

#### ANÁLISIS DE COMPONENTES PRINCIPALES ####
library(factoextra)
library(FactoMineR)

#Antes de hacer el análisis, hay que nombrar las filas y las columnas adecuadamente:
rownames(d_comp) <- d_comp[,1]
colnames(d_comp)[2:7] <- c("PSOE","PP","Unidas Podemos","Ciudadanos","Más País","Vox")

#Aplicamos el PCA utilizando la función del paquete FactoMineR:
res.pca <- PCA(d_comp[,2:7])

#Visualizamos la posición de las variables en las dos componentes principales.
#Al hacerse sobre la base de ggplot, podemos añadir argumentos (por ejemplo labs, que uso para añadir los títulos)
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
) + labs(
  title = "Análisis de componentes principales sobre los programas electorales,\nsegún frecuencia relativa de palabras utilizadas",
  caption = "Fuente: programas electorales de PSOE, PP, Ciudadanos, Unidas Podemos, Más País y Vox | @Picanumeros",
  x = "Componente 1 (57.1% de varianza explicada)", y = "Componente 2 (15.5% de varianza explicada)"
)

#Después obtenemos el Biplot, esta vez sin repeler las etiquetas ya que con tantos puntos iría muy lento:
fviz_pca_biplot(res.pca, repel = F) + labs(
  title = "Análisis de componentes principales sobre los programas electorales,\nsegún frecuencia relativa de palabras utilizadas",
  caption = "Fuente: programas electorales de PSOE, PP, Ciudadanos, Unidas Podemos, Más País y Vox | @Picanumeros",
  x = "Componente 1 (57.1% de varianza explicada)", y = "Componente 2 (15.5% de varianza explicada)"
)

#Almacenamos en el objeto mds la matriz de distancias euclídeas entre los 6 programas utilizando la frecuencia de cada término como variables.
mds <- dist(t(d_comp[,2:7]))

#Aplicamos sobre esa matriz un escalamiento multidimensional clásico con cmdscale y representamos las dos principales dimensiones a las que se nos reduce.
fit <- cmdscale(mds,eig=TRUE, k=2)
puntos <- as.data.frame(fit$points)
ggplot(puntos, aes(x = V1, y = V2, label = row.names(fit$points))) + geom_point() + geom_label() +
  theme_classic(base_size = 15) + labs(x = "Dimensión 1 (44.5% suma autovalores)",
                                       y = "Dimensión 2 (20.6% suma autovalores)",
                                       title = "Representación bidimensional de los programas de cada partido mediante\nescalamiento multidimensional (MDS) según las distancias entre ellos",
                                       subtitle = "Distancias obtenidas a partir de la distancia euclídea entre frecuencias relativas de palabras",
                                       caption = "Fuente: programas electorales de PSOE, PP, Ciudadanos, Unidas Podemos, Más País y Vox | @Picanumeros")
