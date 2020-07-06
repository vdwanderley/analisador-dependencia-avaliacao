## Lista de pacotes que serao usados
list.of.packages <- c("ggplot2", "dplyr", "ggthemes", "maps", "ggExtra", "knitr", "stringi", "stringr", "reshape2", "sfsmisc")

## Retorna uma lista de pacotes que nao estao instalados ainda
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
##

## Instala os novos pacotes
if(length(new.packages)) 
  install.packages(new.packages)
##

#install.packages(ggplot2)
library(ggplot2)
#install.packages("dplyr")
library(dplyr)
#install.packages("ggthemes")
library(ggthemes)
#install.packages("maps")
library(maps)
#install.packages("ggExtra")
library(ggExtra)
library(knitr)
library(stringi)
library(stringr)
library(reshape2)
##Resultados gerados para a AUC
setwd("D:/Mestrado/Resultados Avaliação Parser/finalizacao/")
getwd()

fCetenArg = "resultadosArgCeten.csv"
fCetenPrag = "resultadosPragCeten.csv"

fStanford = "yield_precisao_stanford.csv"
fDptoie = "yield_precisao_dptoie.csv"
fOrange = "yield_precisao_orange.csv"
fLys = "yield_precisao_lys.csv"
fMquni = "yield_precisao_mquni.csv"

dfStanford <- read.table(fStanford, header = TRUE, sep = ";")
dfDptoie <- read.table(fDptoie, header = TRUE, sep = ";", encoding="UTF-8")
dfOrange <- read.table(fOrange, header = TRUE, sep = ";", encoding="UTF-8")
dfLys <- read.table(fLys, header = TRUE, sep = ";", encoding="UTF-8")
dfMquni <- read.table(fMquni, header = TRUE, sep = ";", encoding="UTF-8")

dfStanford = dfStanford[, 4:5]
dfDptoie = dfDptoie[, 4:5]
dfOrange = dfOrange[, 4:5]
dfLys = dfLys[, 4:5]
dfMquni = dfMquni[, 4:5]

divideIntervalos = function(x,n){
  intervalos = as.integer(split(x,rep(1:n, ceiling(length(x)/n),length.out = length(x)))[[1]])
  return (intervalos)
}

divideGrupos = function(qtdLinhasDf, grupos){
  valorInicial = trunc(qtdLinhasDf/grupos)
  valor = valorInicial
  vetorGrupos = NULL
  vetorGrupos[1] = valor
  for (i in 1:(grupos - 1)){
    valor = valor + valorInicial
    vetorGrupos[i + 1] = valor
  }
  return(vetorGrupos)
}

grupos = 4

divideGrupos(500, grupos)

#geral
p = rbind(dfDptoie, dfStanford, dfLys, dfMquni, dfOrange)
pPontos = rbind(dfDptoie[divideGrupos(nrow(dfDptoie), grupos),],  
                dfStanford[divideGrupos(nrow(dfStanford), grupos),],
                dfLys[divideGrupos(nrow(dfLys), grupos),],
                dfMquni[divideGrupos(nrow(dfMquni), grupos),],
                dfOrange[divideGrupos(nrow(dfOrange), grupos),]
                )
nomeMetodos = c("dptOIE", "Stanford", "LyS-FASTPARSE", "MQuni", "Orange - Deskiñ")

p$AD<-rep(c("dptOIE", "Stanford", "LyS-FASTPARSE", "MQuni", "Orange - Deskiñ"),
                   c(nrow(dfDptoie),nrow(dfStanford),nrow(dfLys),nrow(dfMquni),nrow(dfOrange)))

pPontos$AD<-rep(nomeMetodos,
                         c(nrow(dfDptoie[divideGrupos(nrow(dfDptoie), grupos),]), 
                           nrow(dfStanford[divideGrupos(nrow(dfStanford), grupos),]),
                           nrow(dfLys[divideGrupos(nrow(dfLys), grupos),]),
                           nrow(dfMquni[divideGrupos(nrow(dfMquni), grupos),]),
                           nrow(dfOrange[divideGrupos(nrow(dfOrange), grupos),])
                           )
                    )

p$AD = factor(p$AD)
ggplot(p,aes(x = yield, y = precisao, group = AD, color = AD, shape = AD)) +
  scale_shape_manual(values=1:nlevels(p$AD)) +
  theme(axis.title = element_text(size = 15)) +
  theme(axis.text = element_text(size = 10))  +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 12)) +
  ggtitle("Precisão x Yield") +
  labs(x = "Yield", y = "Precisão") +
  geom_line(size = 0.71) + 
  geom_point(data = pPontos, size = 4, aes(x = yield, y = precisao, group = AD, color = AD, shape = AD)) 
#####
pCeten$Método = factor(pCeten$Método)
ggplot(pCeten,aes(x = yield, y = precisao, group = Método, color = Método, shape = Método)) +
  scale_shape_manual(values=1:nlevels(pCeten$Método)) +
  theme(axis.title = element_text(size = 10)) +
  theme(axis.text = element_text(size = 10))  +
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 10)) +
  #theme(legend.position="top") +
  #  guides(shape = guide_legend(override.aes = list(size = 1))) +
  theme(legend.justification=c(1,1), legend.position=c(1,0.36),legend.title=element_blank()) +
  theme(legend.key.size = unit(0.35, "cm")) +
  #ggtitle("CETEN200 - Precisão x Yield") +
  labs(x = "Yield", y = "Precisão") +
  geom_line(size = 0.2) + 
  coord_cartesian(ylim=c(0.45, 0.77)) +
  geom_point(data = pCetenPontos, size = 3.5, aes(x = yield, y = precisao, group = Método, color = Método, shape = Método))

#Cálculo da AUC ####

require(sfsmisc)

#ADs
aucDptoie = integrate.xy(dfDptoie$yield, dfDptoie$precisao)
aucStanford = integrate.xy(dfStanford$yield, dfStanford$precisao)
aucOrange = integrate.xy(dfOrange$yield, dfOrange$precisao)
aucLys = integrate.xy(dfLys$yield, dfLys$precisao)
aucMquni = integrate.xy(dfMquni$yield, dfMquni$precisao)

aucCetenPrag = integrate.xy(dfCetenPrag$yield, dfCetenPrag$precisao)
#nomeMetodos = c("dptOIE", "Stanford", "LyS-FASTPARSE", "MQuni", "Orange - Deskiñ")
dfAuc = NULL
dfAuc$AD[1:5] = nomeMetodos[1:5]

dfAuc$auc[1:5] = c(aucDptoie,aucStanford, aucLys, aucMquni, aucOrange)
#dfAucCeten$auc[2] = aucCetenPrag

ggplot(as.data.frame(dfAuc), aes(AD, auc, fill = as.factor(AD))) +
  geom_text(aes(label=round(auc,digits=2)), vjust=-0.1, position = position_dodge(0.9), size=3.5) +
  geom_bar(stat="identity") +
  ggtitle("CETEN200 - AUC") + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size = 10)) +
  guides(fill = FALSE) +
  scale_fill_grey(start = .4, end = .8) +
  #coord_cartesian(ylim=c(0, 380)) +
  labs(x = "", y = "AUC-PY")

