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
setwd("C:/Users/Leandro/Documents/defesa_mestrado/mestradoufba/Backup geral/Avaliação completa/AVALIAÇÃO FULL DISSERTAÇÃO/calculo_AUC/backups resultados individuais")
getwd()

fCetenArg = "resultadosArgCeten.csv"
fCetenPrag = "resultadosPragCeten.csv"

dfCetenArg <- read.table(fCetenArg, header = TRUE, sep = ";", encoding="UTF-8")
dfCetenPrag <- read.table(fCetenPrag, header = TRUE, sep = ";", encoding="UTF-8")

dfCetenArg = dfCetenArg[,7:8]
dfCetenPrag = dfCetenPrag[,7:8]

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

#CETEN
pCeten = rbind(dfCetenArg, dfCetenPrag)
pCetenPontos = rbind(dfCetenArg[divideGrupos(nrow(dfCetenArg), grupos),],  dfCetenPrag[divideGrupos(nrow(dfCetenPrag), grupos),])

pCeten$Método<-rep(c("ArgOE", "PragmaticOIE"),
                   c(nrow(dfCetenArg),nrow(dfCetenPrag)))

pCetenPontos$Método<-rep(c("ArgOE", "PragmaticOIE"),
                         c(nrow(dfCetenArg[divideGrupos(nrow(dfCetenArg), grupos),]), nrow(dfCetenPrag[divideGrupos(nrow(dfCetenPrag), grupos),])))

pCeten$Método = factor(pCeten$Método)
ggplot(pCeten,aes(x = yield, y = precisao, group = Método, color = Método, shape = Método)) +
  scale_shape_manual(values=1:nlevels(pCeten$Método)) +
  theme(axis.title = element_text(size = 15)) +
  theme(axis.text = element_text(size = 10))  +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 18)) +
  ggtitle("CETEN200 - Precisão x Yield") +
  labs(x = "Yield", y = "Precisão") +
  geom_line(size = 0.71) + 
  geom_point(data = pCetenPontos, size = 4, aes(x = yield, y = precisao, group = Método, color = Método, shape = Método))

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

#Cálculo da AUC

require(sfsmisc)

#Ceten
aucCetenArg = integrate.xy(dfCetenArg$yield, dfCetenArg$precisao)
aucCetenPrag = integrate.xy(dfCetenPrag$yield, dfCetenPrag$precisao)

dfAucCeten = NULL
dfAucCeten$metodo[1] = "ArgOE"
dfAucCeten$metodo[2] = "PragmaticOIE"

dfAucCeten$auc[1] = aucCetenArg
dfAucCeten$auc[2] = aucCetenPrag

ggplot(as.data.frame(dfAucCeten), aes(metodo, auc, fill = as.factor(metodo))) +
  geom_text(aes(label=round(auc,digits=2)), vjust=-0.3, position = position_dodge(0.9), size=3.5) +
  geom_bar(stat="identity") +
  #ggtitle("CETEN200 - AUC") + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size = 10)) +
  guides(fill = FALSE) +
  scale_fill_grey(start = 0, end = .7) +
  #coord_cartesian(ylim=c(0, 380)) +
  labs(x = "", y = "AUC-PY")

