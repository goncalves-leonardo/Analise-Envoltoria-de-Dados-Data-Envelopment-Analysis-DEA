rm(list=ls(all=TRUE)) #apagar todas listas e variáveis
getwd() #importante saber qual o diretório de trabalho em utilização
Pasta.de.Trabalho <- 'C:/Users/leogp/Documents/R/Leonardo_Arquivos/DEA_2021'
setwd(Pasta.de.Trabalho) #Definir uma pasta de trabalho
dir() # ver quais os arquivos existentes no diretório em uso

dados <- read.csv(choose.files(), header = TRUE, sep = ",") # abrir o arquivo ‘.csv’ que contém as variáveis
head(dados)  
glimpse(dados) #mostrar o tipo de cada variável do data frame

#==================== Definir as variáveis INSUMOs x
insumos <- as.matrix(with(dados, cbind(dados$Input1)))

#==================== Definir as variáveis PRODUTOs y
produtos <- as.matrix(with(dados, cbind(dados$Output1, 
                                        dados$Output2,
                                        dados$Output3)))

#==================== Definir a variável que contém o nome das DMUs
dmu <- dados$ï..DMU 

# ATENÇÃO! Desta etapa para frente poderá selecionar tudo e clicar em RUN

#==================== Instalar e carregar os pacotes necessários
if(!require(dplyr)) install.packages("dplyr")
library(dplyr) 
if(!require(Benchmarking)) install.packages("Benchmarking")
library(Benchmarking)
if(!require(corrplot)) install.packages("corrplot") 
library(corrplot)
if(!require(readxl)) install.packages("readxl")
library(readxl)


# Início - criar matriz de correlação:
matriz <-  cor(dados[2:5], method = "spearman") #entre colchetes determina as colunas a serem usadas na análise
#spearman ou kendall para não paramétrica e pearson para paramétrica
head(matriz)
# Arredondando para duas casas decimais:
matriz.correlacao.variaveis <- round(cor(dados[2:5], method = "spearman"), 2)#spearman ou pearson
head(matriz.correlacao.variaveis)
# Criando a representaçãoo gráfica da correlação das variáveis
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
#A função rcorr no pacote Hmisc produz correlações/covariáveis e níveis de significância para correlações de pearson e spearman. No entanto, a entrada deve ser uma matriz e a exclusão em pares é usada.
matriz.Hmisc <- dados[2:5]
head(matriz.Hmisc)
matriz.Hmisc<- rcorr(as.matrix(matriz.Hmisc), type ="spearman")
head(round(matriz.Hmisc$P,2))

matriz.Hmisc$r # matriz de coeficiente de correlação
matriz.Hmisc$P # matriz de valor p
matriz.Hmisc$n # matriz número de variáveis

#==================== plot - marcar x para as que não tiverem relação
corrplot(matriz.Hmisc$r,p.mat=matriz.Hmisc$P,sig.level=0.05,method="pie",type="upper",diag=FALSE)

#==================== alternativa para matriz
if(!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics")
library(PerformanceAnalytics) 

md<- dados[,c(2:5)] # filtrando as colunas
md

chart.Correlation(md, histogram = TRUE, method ="spearman")

#plot padrão
if(!require(psych)) install.packages("psych")
library(psych)
describe.by(md, group=NULL,mat=FALSE,type=3,digits=15  )
pairs.panels(md,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman", # Correlation method (also "spearman" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals


#help(package="Benchmarking") # Início da aplicação DEA - Para ler sobre os pacotes a serem utilizados retire o '?' e clique em RUN
#?dea 
#?Benchmarking 
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
head(dt.eff.bcc.in.insumo)

dt.eff.bcc.in.produto<- data.frame("DMU"=dmu,"eff"=round(bcc.in$eff,3), "ins"=produtos, "rad"=bcc.in$eff * produtos,"fol"=bcc.in$sy, "alv"=produtos + bcc.in$sy)
head(dt.eff.bcc.in.produto)

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

dt.eff.bcc.out.insumo <- data.frame("DMU"=dmu, "eff"=round(1/bcc.out$eff,3), "ins"=insumos, "rad"=round((1/bcc.out$eff)*insumos,2), "fol"=round(bcc.out$sx,2), "alv"=round(insumos - bcc.out$sx,2))
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

#==================== Dados para o Gráfico 1
#Selecionar as colunas que quer representar no gráfico
dados.grafico.rendimento1 <-rendimentos[,c("CRS", "VRS","E_ESC")]
#renomear coluna
dados.grafico.rendimento1 <-rename(dados.grafico.rendimento1,Escala=E_ESC) 

glimpse(dados.grafico.rendimento1)

head(dados.grafico.rendimento1)

#==================== Plotando o Gráfico 1
barplot(t(dados.grafico.rendimento1), names.arg = dmu, cex.axis=1,cex.names=0.9, plot = TRUE, beside = TRUE, axis.lty = 1, offset = 0, col = c("#345082","#608AD4", "#C2D0EA"), horiz = FALSE,axisnames = TRUE, xlab = NULL, ylab = NULL)
title(main = "Rendimentos com retornos constantes, variáveis e de escala: orientação ao output", font = 1)
text(30,.98, "Fronteira de eficiência", col = "black")
abline(h = 1, col = "gray60",lty="dotted") #dashed ou dotted
legend(x=90, y=.2, xpd=TRUE, ncol=1, legend=c("CRS", "VRS", "Escala"), fill=c("#345082","#608AD4", "#C2D0EA"), bty="t",cex=0.5)
#==================== Nota de rodapé no gráfico 1
mtext(side=4, at=par("usr")[3], adj=0, cex=0.8, col="gray40", line=0, text=paste("Pereira, Leonardo --", format(Sys.time(), "%d/%m/%Y %H:%M:%S --"), R.version.string))


#==================== Dados para o Gráfico 2
#Selecionar as colunas que quer representar no gráfico
dados.grafico.rendimento2 <-rendimentos[,c("VRS", "CRS","E_ESC")]
#renomear coluna
dados.grafico.rendimento2 <- rename(dados.grafico.rendimento2,Escala=E_ESC) 
print(dados.grafico.rendimento2)
glimpse(dados.grafico.rendimento2)

#==================== Plotando o gráfico 2
barplot(t(dados.grafico.rendimento2), names.arg = dmu, cex.axis=1,cex.names=0.7, plot = TRUE, beside = FALSE, axis.lty = 1, offset = 0, col = c("#345082","#608AD4", "#C2D0EA"), horiz = FALSE,axisnames = TRUE, xlab = NULL, ylab = NULL)
title(main = "Rendimentos com retornos constantes, variáveis e de escala: orientação ao output", font = 1)
text(-0.5,.95, "VRS", col = "black", cex = .7)
abline(h = 1, col = "black",lty="dashed")#dashed ou dotted
text(-0.5,1.95, "CRS", col = "black", cex = .7)
abline(h = 2, col = "black",lty="dashed") 
text(-0.1,2.95, "ESCALA", col = "black", cex = .7)
abline(h = 3, col = "black",lty="dashed")
legend(x=28, y=2.9, xpd=TRUE, ncol=1, legend=c("VRS", "CRS", "Escala"), fill=c("#345082","#608AD4", "#C2D0EA"), bty="t",cex=.9)
#==================== Nota de rodapé no gráfico 2
mtext(side=4, at=par("usr")[3], adj=0, cex=0.8, col="gray40", line=0, text=paste("Pereira, Leonardo -", format(Sys.time(), "%d/%m/%Y %H:%M:%S --"), R.version.string))

#==================== Representação Gráfica da isoquanta
x <- insumos
y <- produtos

dea.plot(x,y, RTS="vrs", ORIENTATION="in-out",txt=dmu, main="Fronteira DEA BCC-O", lty="dotted",lwd=1,col="BLACK", fex=0.7,GRID=FALSE)
dea.plot(x,y,RTS="crs",add=TRUE,lty="dotted",col="red")

#==================== Pacote qcc - Gráfico de Pareto
if(!require(qcc)) install.packages("qcc")
library(qcc) 

pareto.chart(dados.frequencia, ylab = "Freq. Ineficiência Relativa", xlab = "DMUs", ylab2="Porcentagem Acumulada",las=2)

# Alterando a função do pacote qcc
func.grafico.pareto <- function (data, plot = TRUE, ...) 
{
  call <- match.call(expand.dots = TRUE)
  varname <- deparse(substitute(data))
  data <- as.table(data)
  if (length(dim(data)) > 1) 
    stop("only one-dimensional object (table, vector, etc.) may be provided")
  data <- sort(data, decreasing = TRUE, na.last = TRUE)
  csum <- cumsum(data)
  tab <- round(cbind(data, csum, data/max(csum, na.rm = TRUE) * 
                       100, csum/max(csum, na.rm = TRUE) * 100),3)
  colnames(tab) <- c("Freq.Absoluta", "Freq.Acumulada", "Perc.Individual", 
                     "Perc.Acumulado")
  names(dimnames(tab)) <- c("", paste("\nPareto chart analysis for", 
                                      varname))
  tab <- as.table(tab)
  class(tab) <- c("pareto.chart", class(tab))
  attr(tab, "call") <- call
  attr(tab, "varname") <- varname
  if (plot) 
    plot(tab, ...)
  return(tab)
}

# Utilizando a função com alterações
dados.frequencia <- (rendimentos$VRS) #definir os dados para analisar a distribuição de frequência
names(dados.frequencia) <- rendimentos$DMU
frequencia.pareto.VRS <- data.frame(cbind( func.grafico.pareto(dados.frequencia, ylab = "Freq. DMUs Ineficientes")))

# Analisar sob a ótica da Regra 80/20
frequencia.pareto.VRS


#==================== Exportar para Excel - primeira opção
writexl::write_xlsx(list(dados, dt.eff.bcc.out.insumo, dt.eff.bcc.out.produto, rendimentos,
                         dmus.pares.produto, somente.dmus.eff.bcc.out ), 
                    path = paste("Resultados_DEA_", 
                                 format(Sys.time(),"%d_%m_%Y"),".xlsx", 
                                 sep = "", collapse = NULL), col_names = TRUE, 
                    format_headers = TRUE, use_zip64 = FALSE)


#==================== Função para Exportar para Excel: cada guia terá o nome das variáveis
xlsx.SalvarNomeGuia <- function (file, ...)
{
  require(xlsx, quietly = TRUE)
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    if (i == 1)
      write.xlsx(objects[[i]], file, sheetName = objnames[i],col.names = TRUE, 
                 row.names = TRUE)
    else write.xlsx(objects[[i]], file, sheetName = objnames[i],
                    append = TRUE, col.names = TRUE, row.names = TRUE)
  }
}

#==================== Exportar para Excel - segunda opção
xlsx.SalvarNomeGuia("Resultados_DEA.xlsx",
                    dados, dt.eff.bcc.out.insumo, dt.eff.bcc.out.produto, rendimentos,
                    dmus.pares.produto, somente.dmus.eff.bcc.out, frequencia.pareto.VRS)

