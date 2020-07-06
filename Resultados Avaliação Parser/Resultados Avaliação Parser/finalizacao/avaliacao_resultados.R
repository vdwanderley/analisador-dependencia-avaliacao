set.seed(345)
install.packages('irr')
library(dplyr)
library(caret)
library(irr)

setwd('D:/Mestrado/Resultados Avaliação Parser')

# Avaliacao final
af = read.csv('finalizacao/avaliacao_extracoes_avaliadores.csv', sep = ';',stringsAsFactors=FALSE)
afs = read.csv('finalizacao/avaliacao_subordinadas_avaliadores.csv', sep = ';',stringsAsFactors=FALSE)

af = filter(af, excluida != 'X')

# colunas com votos
cVotos = c('AVALIADOR.3','AVALIADOR.2','AVALIADOR.1')

afVotos = af[,cVotos]
afsVotos = afs[, cVotos]

todosVotos = rbind(afVotos,afsVotos)
summary(todosVotos)
# Fleiss Kappa
fkappa = kappam.fleiss(todosVotos, detail = TRUE)
fkappa$value

# Colunas com coerencia final
cCoerencia = c('linha', 'ID.SENTENÇA', 'COERENCIA.FINAL','dptOIE', 'stanford',	'orange',	'mquni',	'lys')
afCoerencia = af[, cCoerencia]
afsCoerencia = afs[, cCoerencia]
todosCoerencia = rbind(afCoerencia, afsCoerencia)

# funções  ####
obtemFatosExtraidos = function(dados, sistema){
  which(todosCoerencia[[sistema]] == 1)
  #filter(dados, !!as.name(sistema) == 1) %>% select('COERENCIA.FINAL')
}

obtemFatosCoerentes = function(dados, extracoes_sistema){
  if (!missing(extracoes_sistema)){
    dados = dados[extracoes_sistema,]
  }
  which(dados$COERENCIA.FINAL == 1)
  #filter(extracoes_sistema, COERENCIA.FINAL ==1)
}

obtemPrecisao = function(corretas, recuperadas){
  length(corretas) / length(recuperadas)
}

obtemRecall = function(coerentes_ad, coerentes){
  length(coerentes_ad) / length(coerentes)
}


# Matrix de confusao ####
mx.data = lapply(todosCoerencia,factor)
confusionMatrix(data = mx.data$dptOIE, reference = mx.data$COERENCIA.FINAL, positive = '1')

# 1 - quantidade dos fatos extraídos ####
dptoie.ext = obtemFatosExtraidos(todosCoerencia, 'dptOIE') #filter(todosCoerencia, dptOIE == 1) %>% select('COERENCIA.FINAL')
stanford.ext = obtemFatosExtraidos(todosCoerencia, 'stanford') #filter(todosCoerencia, stanford == 1) %>% select('COERENCIA.FINAL')
orange.ext = obtemFatosExtraidos(todosCoerencia, 'orange') #filter(todosCoerencia, orange == 1) %>% select('COERENCIA.FINAL')
mquni.ext = obtemFatosExtraidos(todosCoerencia, 'mquni') #filter(todosCoerencia, mquni == 1) %>% select('COERENCIA.FINAL')
lys.ext = obtemFatosExtraidos(todosCoerencia, 'lys') #filter(todosCoerencia, lys == 1) %>% select('COERENCIA.FINAL')

# 2 - c (quantidade de fatos coerentes extraídos) ####
dptoie.c = obtemFatosCoerentes(todosCoerencia, dptoie.ext)
stanford.c = obtemFatosCoerentes(todosCoerencia, stanford.ext)
orange.c = obtemFatosCoerentes(todosCoerencia, orange.ext)
mquni.c = obtemFatosCoerentes(todosCoerencia, mquni.ext)
lys.c = obtemFatosCoerentes(todosCoerencia, lys.ext)

# 3 - Precisão (A precisao e a fracao das triplas corretas extraidas entre todas as triplas recuperadas) #######
dptoie.precisao = obtemPrecisao( dptoie.c, dptoie.ext)
stanford.precisao = obtemPrecisao( stanford.c, stanford.ext)
orange.precisao = obtemPrecisao( orange.c, orange.ext)
mquni.precisao = obtemPrecisao( mquni.c, mquni.ext)
lys.precisao = obtemPrecisao( lys.c, lys.ext)

# 4 - Cobertura ####
coerentes = obtemFatosCoerentes(todosCoerencia)

dptoie.recall = obtemRecall( dptoie.c, coerentes)
stanford.recall = obtemRecall( stanford.c, coerentes)
orange.recall = obtemRecall( orange.c, coerentes)
mquni.recall = obtemRecall( mquni.c, coerentes)
lys.recall = obtemRecall( lys.c, coerentes)

# 5 Extrações x Coerentes (Gráfico) ####
exts = c(length(dptoie.ext), length(stanford.ext), length(orange.ext), length(mquni.ext), length(lys.ext))
c =  c(length(dptoie.c), length(stanford.c), length(orange.c), length(mquni.c), length(lys.c))
AD = c("dptOIE", "Stanford",  "Orange - Deskiñ", "MQuni",  "LyS-FASTPARSE")
gbar = NULL
gbar$AD = rep(AD,2)
gbar$extracoes = c(c, exts - c)
gbar$tipo_extracao = as.factor(c(rep("Extrações Coerentes",5), rep("Extrações Incoerentes",5)))
as.data.frame(gbar)

ggplot(as.data.frame(gbar),
       aes(AD, extracoes, fill = tipo_extracao)) +
  geom_bar(stat = "identity")+
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_grey() +
  ggtitle("Relações Extraídas") + 
  geom_text(aes(label=extracoes), position = position_stack(reverse = TRUE), vjust = 0, size=3.5) +
  labs(x = "Analisador de Dependência", y = "Extrações", fill = "Corência")



# 7 Sentenças sem extração ####
dptoie.sem_extracao = filter(af[dptoie.ext,], ARG1 == '') %>% select(ID.SENTENÇA)
stanford.sem_extracao = filter(af[stanford.ext,], ARG1 == '') %>% select(ID.SENTENÇA)
lys.sem_extracao = filter(af[lys.ext,], ARG1 == '') %>% select(ID.SENTENÇA)
mquni.sem_extracao = filter(af[mquni.ext,], ARG1 == '') %>% select(ID.SENTENÇA)
orange.sem_extracao = filter(af[orange.ext,], ARG1 == '') %>% select(ID.SENTENÇA)

# 8 Extrações distintas ####
dptoie.distintas = filter(todosCoerencia[dptoie.ext,], COERENCIA.FINAL == 1 & dptOIE == 1 & stanford == 0 & orange == 0 & mquni == 0 & lys == 0) %>% select(linha)
stanford.distintas = filter(todosCoerencia[stanford.ext,], COERENCIA.FINAL == 1 & dptOIE == 0 & stanford == 1 & orange == 0 & mquni == 0 & lys == 0) %>% select(linha)
lys.distintas = filter(todosCoerencia[lys.ext,], COERENCIA.FINAL == 1 & dptOIE == 0 & stanford == 0 & orange == 0 & mquni == 0 & lys == 1) %>% select(linha)
mquni.distintas = filter(todosCoerencia[mquni.ext,], COERENCIA.FINAL == 1 & dptOIE == 0 & stanford == 0 & orange == 0 & mquni == 1 & lys == 0) %>% select(linha)
orange.distintas = filter(todosCoerencia[orange.ext,], COERENCIA.FINAL == 1 & dptOIE == 0 & stanford == 0 & orange == 1 & mquni == 0 & lys == 0) %>% select(linha)


##