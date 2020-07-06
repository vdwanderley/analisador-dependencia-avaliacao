library(dplyr)

preenche_id = function(data){
  last = c()
  for (i in 1:nrow(data)){
    if ( !is.na(data[i,1]) ) {
      last[1] = data[i,1]
    }
    else{
      data[i,1] = last[1]
    }
  }  
  data
}
preenche_sentenca = function(data){
  last = c()
  for (i in 1:nrow(data)){
    if ( data[i,2] != "" ) {
      last[1] = data[i,2]
    }
    else{
      data[i,2] = last[1]
    }
  }  
  data
}

setwd('D:/Mestrado/Resultados Avaliação Parser')
d1 = read.csv('stanford/extractedFactsByDpOIE.csv', sep = ';',stringsAsFactors=FALSE)
d2 = read.csv('orange/extractedFactsByDpOIE.csv', sep = ';',stringsAsFactors=FALSE)
d3 = read.csv('mquni/extractedFactsByDpOIE.csv', sep = ';',stringsAsFactors=FALSE)
d4 = read.csv('lys/extractedFactsByDpOIE.csv', sep = ';',stringsAsFactors=FALSE)
d5 = read.csv('dptoie/extractedFactsByDpOIE.csv',encoding = 'UTF-8',stringsAsFactors=FALSE)

View(d5)

d1 = d1[1:11]
d2 = d2[1:11]
d3 = d3[1:11]
d4 = d4[1:11]

d1 = preenche_id(d1)
d2 = preenche_id(d2)
d3 = preenche_id(d3)
d4 = preenche_id(d4)
d5 = preenche_id(d5)

d1 = preenche_sentenca(d1)
d2 = preenche_sentenca(d2)
d3 = preenche_sentenca(d3)
d4 = preenche_sentenca(d4)
d5 = preenche_sentenca(d5)

View(d5)
is.na(d1[4,2])
d1[4,2]

d1$SISTEMA = 'stanford' 
d2$SISTEMA = 'orange' 
d3$SISTEMA = 'mquni' 
d4$SISTEMA = 'lys' 

View(d2)

setSubordinada = function(dt){
  sixs = which(dt$MÓDULO.ARG2 == '6')  
  
  dt[sixs, 'subordinada'] = paste(dt[sixs+1,'ARG1'], '/ ', d1[sixs+1,'REL'], '/ ',d1[sixs+1,'ARG2'])
  dt[-sixs, 'subordinada'] = ''
  dt
}

d1 = setSubordinada(d1)
d2 = setSubordinada(d2)
d3 = setSubordinada(d3)
d4 = setSubordinada(d4)
d5 = setSubordinada(d5)

View(d5)

data = rbind(d1,d2,d3,d4,d5)

data

grouped_data = data %>% group_by(ID.SENTENÇA,ARG1,REL,ARG2,subordinada,SENTENÇA) %>% summarise(sistemas = paste(SISTEMA, collapse=", ")) %>% select(ID.SENTENÇA,SENTENÇA,ARG1,REL,ARG2, COERÊNCIA,subordinada, sistemas) %>% arrange(ID.SENTENÇA, desc(SENTENÇA))

write.csv2(grouped_data, 'avaliacao_extracoes_com_sistema.csv')
View(grouped_data)

subordinada_data = filter(grouped_data, subordinada != '')
View(subordinada_data)

write.csv2(subordinada_data, 'avaliacao_subordinadas_com_sistema.csv')
