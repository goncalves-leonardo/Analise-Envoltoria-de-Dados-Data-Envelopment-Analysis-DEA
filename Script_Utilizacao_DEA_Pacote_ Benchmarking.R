rm(list=ls(all=TRUE)) #apagar todas listas e variáveis
getwd() #importante saber qual o diretório de trabalho em utilização
setwd('C:/Users/USUARIO/Documents/R/PASTA') #Definir uma pasta de trabalho
dir() # ver quais os arquivos existentes no diretório em uso
#==================== Instalar e carregar os pacotes necessários
if(!require(dplyr)) install.packages("dplyr")
library(dplyr) 
if(!require(Benchmarking)) install.packages("Benchmarking")
library(Benchmarking)
if(!require(corrplot)) install.packages("corrplot") 
library(corrplot)
if(!require(readxl)) install.packages("readxl")
library(readxl)

#==================== Leitura do arquivo ‘.csv’ que contém as variáveis
dados <- read.csv(choose.files(), header = TRUE, sep = ",")
head(dados)  
glimpse(dados) # mostrar o tipo de cada variável do data frame
# Criar matriz de correlação:
matriz <-  cor(dados[2:5], method = "spearman") # Valores dentro dos colchetes determinam as colunas da matriz a serem usadas na análise
# Resumidamente, Spearman ou kendall para não paramétrica e pearson para paramétrica
head(matriz)
# Arredondando para duas casas decimais:
matriz.correlacao.variaveis <- round(cor(dados[2:5], method = "spearman"), 2)
head(matriz.correlacao.variaveis)
## Criando a representaçãoo gráfica de correlação das variáveis
par(mfrow=c(1,1))
corrplot(matriz.correlacao.variaveis, method="pie", 
         type="upper", order="alphabet",add = FALSE, 
         addCoef.col = "black", # adiciona o coeficiente da matriz
         tl.col="black", tl.srt=25, # cor e rotação do nome das variáveis
         number.cex = 1.2, diag=FALSE) # não mostrar a diagonal principal
#,title = 'spearman' #caso queira título

matriz.correlacao.variaveis <- as.data.frame(matriz.correlacao.variaveis)

#==================== Outras representações por meio de matriz e gráficos
if(!require(Hmisc)) install.packages("Hmisc")
library(Hmisc) 
matriz.Hmisc <- dados[2:5]
head(matriz.Hmisc)
matriz.Hmisc<- rcorr(as.matrix(matriz.Hmisc), type ="spearman")
head(round(matriz.Hmisc$P,2))

matriz.Hmisc$r # matriz de coeficiente de correlação
matriz.Hmisc$P # matriz de valor p
matriz.Hmisc$n # matriz número de variáveis

#==================== plot
corrplot(matriz.Hmisc$r,p.mat=matriz.Hmisc$P,sig.level=0.05,method="pie",type="upper",diag=FALSE)

#==================== alternativa para matriz
if(!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics")
library(PerformanceAnalytics) 

md<- dados[,c(2:5)] # filtrando as colunas
md

chart.Correlation(md, histogram = TRUE,method ="spearman")

help(package="Benchmarking") #Para ler sobre os pacotes a serem utilizados
?dea 
?Benchmarking 

#==================== Definir as variáveis INSUMOs x
insumos <- as.matrix(with(dados, cbind(dados$input1)))

#==================== Definir as variáveis PRODUTOs y
produtos <- as.matrix(with(dados, cbind(dados$Output1, 
                                        dados$Output2,
                                        dados$Output3)))

#==================== Definir a variável que contém o nome das DMUs
dmu <- dados$NomeDMU 

#==================== Modelo BCC Orientado ao Input
bcc.in <- dea(insumos, produtos, RTS="vrs", ORIENTATION="in", SLACK=TRUE) # eff BCC insumo orientado com folgas
summary(bcc.in)
lambda(bcc.in)
lambda(bcc.in, KEEPREF = TRUE)
head(bcc.in$eff, digits=2)
which(bcc.in$eff == 1 & !bcc.in$slack)
dmus.pares.insumo <-  data.frame(DMU=dmu, round(lambda(bcc.in),2))
head(dmus.pares.insumo,10)
dt.eff.bcc.in.insumo<- data.frame("DMU"=dmu,"eff"=round(bcc.in$eff,3), "ins"=insumos, "rad"=bcc.in$eff * insumos, "fol"=bcc.in$sx, "alv"= insumos - bcc.in$sx)

dt.eff.bcc.in.produto<- data.frame("DMU"=dmu,"eff"=round(bcc.in$eff,3), "ins"=produtos, "rad"=bcc.in$eff * produtos,"fol"=bcc.in$sy, "alv"=produtos + bcc.in$sy)

#==================== Modelo BCC Orientado ao Output
bcc.out <- dea(insumos, produtos, RTS="vrs", ORIENTATION="out", SLACK=TRUE)
bcc.out
summary(bcc.out)
lambda(bcc.out)
dmus.pares.produto <-  data.frame(DMU=dmu, round(lambda(bcc.out),2))
head(dmus.pares.produto)

lambda(bcc.out, KEEPREF = TRUE)
head(1/bcc.out$eff, digits=2)
DMU.eff <- which(1/bcc.out$eff == 1 & !bcc.out$slack)# para mostrar apenas as DMU's eficientes e sem folga
DMU.eff 
somente.dmus.eff.bcc.out <- data.frame(DMU=dmu[DMU.eff],LINHA=DMU.eff)#mostrar nome das DMU's eficientes
head(somente.dmus.eff.bcc.out)

dt.eff.bcc.out.insumo <- data.frame("DMU"=dmu, "eff"=round(1/bcc.out$eff,3), "ins"=insumos, "rad"=round((1/bcc.out$eff)*insumos,2), "Poupar"=round(insumos-(1/bcc.out$eff*insumos),2), "fol"=round(bcc.out$sx*-1,2), "alv"=round(insumos - bcc.out$sx,2))
head(dt.eff.bcc.out.insumo)

dt.eff.bcc.out.produto <- data.frame("DMU"=dmu,"eff"=round(1/bcc.out$eff,3), "pro"=produtos, "rad"=round((bcc.out$eff * produtos)-produtos,2), "fol"=round(bcc.out$sy,2), "alv"=round((bcc.out$eff * produtos)+bcc.out$sy,2))

head(dt.eff.bcc.out.produto)

#====================  RENDIMENTOS DE ESCALA
ccr.out <- dea(insumos, produtos, RTS="crs", ORIENTATION="out", SLACK=TRUE)
bcc.out <- dea(insumos, produtos, RTS="vrs", ORIENTATION="out", SLACK=TRUE)
bcc.out.irs <- dea(insumos, produtos, RTS="irs", ORIENTATION="out", SLACK=TRUE) # eff BCC out orientado rend crescentes
bcc.out.drs <- dea(insumos, produtos, RTS="drs", ORIENTATION="out", SLACK=TRUE) # eff BCC out orientado rend decrescentes

head( data.frame("DMU"=dmu,"CRS"=round(1/ccr.out$eff,3),"VRS"=round(1/bcc.out$eff,3), "E_ESC"=round(ccr.out$eff/bcc.out$eff,3), "REND"=ifelse(ccr.out$eff == bcc.out$eff | bcc.out$eff == bcc.out.irs$eff & bcc.out$eff == bcc.out.drs$eff, "constante", ifelse(bcc.out$eff == bcc.out.irs$eff & bcc.out$eff != bcc.out.drs$eff,"crescente","decrescente"))))

#==================== Limitar o número de casa decimais com a função round(x, digits=n) para não inteferir na análise
head(cbind("CRS"=ccr.out$eff, "VRS"=bcc.out$eff, "IRS"=bcc.out.irs$eff,"DRS"=bcc.out.drs$eff), digits=4)

ee <- data.frame(round((cbind("CRS"=1/ccr.out$eff, "VRS"=1/bcc.out$eff, "IRS"=1/bcc.out.irs$eff,"DRS"=1/bcc.out.drs$eff)), digits=3))

rendimentos <- data.frame("DMU"=dmu,ee, "E_ESC"=round(ee$CRS/ee$VRS,3),"REND"=ifelse(ee$CRS == ee$VRS | ee$VRS == ee$IRS & ee$VRS == ee$DRS, "constante", ifelse(ee$VRS == ee$IRS & ee$VRS != ee$DRS,"crescente","decrescente")))

head(rendimentos)

#==================== Visualizar ordenado do maior para o menor pelo modelo VRS
head(rendimentos[order(rendimentos$VRS, decreasing=TRUE),])

#==================== Gráfico 1
#Selecionar as colunas que quer representar no gráfico
dados.grafico.rendimento1 <-rendimentos[,c("CRS", "VRS","E_ESC")]
#renomear coluna
dados.grafico.rendimento1 <-rename(dados.grafico.rendimento1,Escala=E_ESC) 

glimpse(dados.grafico.rendimento1)

head(dados.grafico.rendimento1)

barplot(t(dados.grafico.rendimento1), names.arg = dmu, cex.axis=1,cex.names=0.9, plot = TRUE, beside = TRUE, axis.lty = 1, offset = 0, col = c("#345082","#608AD4", "#C2D0EA"), horiz = FALSE,axisnames = TRUE, xlab = NULL, ylab = NULL)

title(main = "Rendimentos de Escala, CRS e VRS no modelo orientado ao output", font = 1)
text(5,.98, "Fronteira de eficiência", col = "black")
abline(h = 1, col = "gray60",lty="dotted")#dashed ou dotted

legend(x=110, y=0.95, xpd=TRUE, ncol=1, legend=c("RS", "VRS", "Escala"), fill=c("#345082","#608AD4", "#C2D0EA"), bty="n",cex=0.5)

#nota de rodapé no gráfico
mtext(side=4, at=par("usr")[3], adj=0, cex=0.8, col="gray40", line=0, text=paste("Pereira, Leonardo --", format(Sys.time(), "%d/%m/%Y %H:%M:%S --"), R.version.string))

#==================== Dados para o gráfico 2
#Selecionar as colunas que quer representar no gráfico
dados.grafico.rendimento2 <-rendimentos[,c("VRS", "CRS","E_ESC")]
#renomear coluna
dados.grafico.rendimento2 <- rename(dados.grafico.rendimento2,Escala=E_ESC) 
print(dados.grafico.rendimento2)
glimpse(dados.grafico.rendimento2)

barplot(t(dados.grafico.rendimento2), names.arg = dmu, cex.axis=1,cex.names=0.9, plot = TRUE, beside = FALSE, axis.lty = 1, offset = 0, col = c("#345082","#608AD4", "#C2D0EA"), horiz = FALSE,axisnames = TRUE, xlab = NULL, ylab = NULL)

title(main = "Rendimentos de Escala, CRS e VRS no modelo orientado ao output", font = 1)
text(-0.5,.95, "VRS", col = "black")
abline(h = 1, col = "black",lty="dashed")#dashed ou dotted
text(-0.5,1.95, "CRS", col = "black")
abline(h = 2, col = "black",lty="dashed") 
text(-0.1,2.95, "ESCALA", col = "black")
abline(h = 3, col = "black",lty="dashed")

legend(x=28, y=2.9, xpd=TRUE, ncol=1, legend=c("VRS", "CRS", "Escala"), fill=c("#345082","#608AD4", "#C2D0EA"), bty="t",cex=.9)

#==================== Nota de rodapé no gráfico
mtext(side=4, at=par("usr")[3], adj=0, cex=0.8, col="gray40", line=0, text=paste("Pereira, Leonardo -", format(Sys.time(), "%d/%m/%Y %H:%M:%S --"), R.version.string))

#==================== Representação Gráfica da isoquanta
x <- insumos
y <- produtos

dea.plot(x,y, RTS="vrs", ORIENTATION="in-out",txt=dmu, main="Fronteira DEA BCC-O", lty="dotted",lwd=1,col="BLACK", fex=0.7,GRID=FALSE)
dea.plot(x,y,RTS="crs",add=TRUE,lty="dotted",col="red")

#==================== Exportar para Excel: cada sheet (guia) será como na sequência

writexl::write_xlsx(list(dados, dt.eff.bcc.out.insumo, dt.eff.bcc.out.produto, rendimentos, dmus.pares.produto, somente.dmus.eff.bcc.out ), path = paste("Resultados_DEA_", format(Sys.time(),"%d_%m_%Y"),".xlsx", sep = "", collapse = NULL), col_names = TRUE, format_headers = TRUE, use_zip64 = FALSE)
