# Contemos cuántas palabras lee por noche

# LECTURA ---------------------------------------------------------------------------------------------------------

  library(data.table)
  library(dplyr)
  require(tidytext)
  require(readr)
  source('../cuentame_preprocess/utils.R')
  
  rutas <- seleccion.txts(getwd())  
  
  libros <- data.table()
  for(ruta in rutas$path){
    libros <- rbind(libros, get.libro.dt(ruta))
  }
  # libros %>% select(-texto)

# PROCESO ---------------------------------------------------------------------------------------------------------

buf <- libros %>% unnest_tokens(word, texto) %>% as.data.table
buf[, j := 1:.N]
# sample_n(buf, 10)

muestra.entorno <- function(i, ventana =10){
  # i <- 100
  paste(buf[(i-ventana):(i+ventana), word], collapse = ' ')
}

# buscamos los simbolos que acompañan al número de noch
candidatos <- buf[word %like% 'ª|º']
candidatos[, noche := as.numeric(gsub('º|ª','',word))]

candidatos2 <-
  merge(
    candidatos, 
    data.table(noche = 1:1001),
    all.y = T,
    by = 'noche')

# noches que me faltan: 108
candidatos2[is.na(word)]

muestra.entorno(sample(candidatos2$j, 1), 20)

# vemos qué pasa en la que falta
  candidatos2[word %like% '74']
  # 12:   746 mil_noches   git Users/halatm/Desktop/git/mil_noches 746ª  815721
  # 13:   749 mil_noches   git Users/halatm/Desktop/git/mil_noches 749ª  816764
  reduced <- buf[between(j,815721, 816764-10)]
  js <- reduced[word %like% 'discr']$j
  lapply(js, function(x) muestra.entorno(x, 40))
  
  
  setorder(candidatos2, noche)
  candidatos2[, delta := j - lag(j)]



setorder(candidatos2, noche)
candidatos2[, delta := j - lag(j)]

#el 515 está repetido, q

m <- candidatos2$delta %>% max(na.rm = T)
candidatos2 <- candidatos2[delta != m]
setorder(candidatos2, noche)
candidatos2[, delta := j - lag(j)]

# PLOTS -----------------------------------------------------------------------------------------------------------

library(ggplot2)

candidatos2$delta %>% summary
candidatos2$delta %>% hist(breaks = 30)
candidatos2[is.na(delta)]

# cada noche
ggplot(candidatos2, aes(noche, delta)) + geom_line()+ geom_point()


candidatos2[, grupo:= cut(noche, breaks = seq(1,1001, by=50))]

# bloques de 50, número de palabras
datas <- candidatos2[, .(med = median(delta, na.rm = T) / 125), grupo]
grupos <- datas[,.N, grupo]
ggplot(datas, aes(grupo, med,label = format(round(med, 1), big.mark = '.', decimal.mark = ','))) +
  geom_bar(stat = 'identity', alpha = 0.5) + 
  geom_bar(data = datas[grupo == grupos[1]$grupo], stat = 'identity', alpha = 0.7)+ 
  geom_bar(data = datas[grupo == grupos[2]$grupo], stat = 'identity', alpha = 0.7)+ 
  theme_minimal() + 
  geom_text(size = 3, nudge_y = .8) +
  labs(
    title = 'Duration was tremendously shortened after the first 50 nights',
    # title = 'Duration of the narration by night',
    subtitle = 'Duration of the narration by night. Median over 50 nights', y = '[mins]') +
  theme(axis.text.x = element_text(angle = 65, hjust = 1),
        axis.title.x = element_blank()) 

# bloques de 50, minutos
datas <- candidatos2[, .(med = median(delta, na.rm = T)), grupo]
ggplot(datas,
       aes(grupo, med, label = format(round(med), big.mark = '.', decimal.mark = ','))) +
  geom_bar(stat = 'identity', alpha = 0.5)+ 
  geom_bar(data = datas[grupo == grupos[1]$grupo], stat = 'identity', alpha = 0.7)+ 
  geom_bar(data = datas[grupo == grupos[2]$grupo], stat = 'identity', alpha = 0.7)+ 
  theme_minimal() + 
  labs(title = 'Duration of the Narration, by night',
       subtitle = 'Median in 50 nights', y = 'Number of words') +
  geom_text(size = 2, nudge_y = 50) +
  theme(axis.text.x = element_text(angle = 65, hjust = 1),
        axis.title.x = element_blank()) 


# Avance en el libro, por noche
candidatos2[, perc := j / max(j, na.rm = T)]
candidatos2[, perc.ref:=noche/1001]

ggplot(melt(candidatos2, 'noche', c('perc', 'perc.ref')), aes(noche, value, color = variable))+ geom_line() +
  theme_minimal()+
  labs(title = 'Some interesting points at night 500 and 750',
       subtitle = 'At night 70 (7% of the nights) you have read 16% of the book',
       x = 'night', y='% of the book') + theme(legend.position = 'none') +
  coord_fixed(1100)
  


