install.packages("caret")
bb=BETABANK[ ,2:11]
colnames(bb)
bb$NATUREZA=bb$NATUREZA...5
bb$ESCOLARIDADE=bb$ESCOLARIDADE...3
#nao sei porque acontece 
bb=bb[ , -c(2,4)]
names(bb)
class(bb)
str(bb)

table(bb$STATUS) #bom = 1 e mau = 0
 
#analisar e tratar as vars
bb$ECIV <- as.factor(bb$ECIV)
bb$PROFISSAO <- as.factor(bb$PROFISSAO)
bb$REGIAO=as.factor(bb$REGIAO)
bb$NATUREZA=as.factor(bb$NATUREZA)
bb$ESCOLARIDADE=as.factor(bb$ESCOLARIDADE)
 
#por simplicidade (tempo!!!) vou analisar algumas vars
summary(bb)

summary(bb$IDADE)
boxplot(bb$IDADE~bb$STATUS, main="idade", col=11);grid(col=2)
which(bb$IDADE<18)
bb=subset(bb, bb$IDADE>=18) #subconjunto que só contem 18 anos ou mais
quantile(bb$IDADE, probs = seq(0,1,.05))
bb=subset(bb, bb$IDADE<100) #subconjunto que só contem 100 anos ou menos
boxplot(bb$IDADE~bb$STATUS, main="idade", col=11);grid(col=2)
hist(bb$IDADE, col = 18)

summary(bb$RENDA)
hist(bb$RENDA)
boxplot(bb$RENDA~bb$STATUS, main="renda", col=11); grid(col=1)
quantile(bb$RENDA, probs= seq(0,1, .005))
#analistas do betabank quem ganha acima de 250 sao muito ricos...
#alternativa seria eliminar quem ganha acima de 250 ou 300
sum(bb$RENDA>300)
bb$renda=ifelse(bb$RENDA >300, 300, bb$RENDA)
#recomendação seria analisar se quem ganha acima de 300 obedece algum padrao
par(mfrow=c(2,2)) #dois graficos por tela
boxplot(bb$renda~bb$STATUS, main="renda", col=11)
hist(bb$renda)
boxplot(log(bb$renda)~bb$STATUS, main="renda", col=11); grid(col=1)
bb$Srenda=sqrt(bb$renda)
hist(bb$Srenda)
#valeria a pena comparar o ajuste com renda e com srenda

table(bb$UF, useNA = "ifany")
bb$estado=ifelse(bb$UF!="SP","NOSP","SP") # entre nosp e sp é como se houvese um else
table(bb$estado)
m=table(bb$estado, bb$STATUS); m
round(prop.table(m,1),2)


m=table(bb$ESCOLARIDADE, bb$STATUS, useNA = "ifany"); m
#podemos excluir categoria 5  ou fundir com outra categoria
#que critério seguir para fundir? 
round(prop.table(m,1),3)
#vamos fundir categoria 5 com categoria 1
bb$escol= ifelse(bb$ESCOLARIDADE==1 | bb$ESCOLARIDADE== 5, "1&5",bb$ESCOLARIDADE)
bb$escol= ifelse(bb$ESCOLARIDADE==3 | bb$ESCOLARIDADE==4, "3&4", bb$ESCOLARIDADE)
m <- table(bb$escol, bb$STATUS, dnn = c('escol', 'status')); m
round(prop.table(m,1),2)

#limpar arquivo
names(bb)
bb=bb[ ,-c(6,7,10,11)]
colnames(bb)

library(caret)
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#vamos dividir a amostra em duas partes> lrn  e tst
set.seed(123)
flag=createDataPartition(bb$STATUS, p=.6, list=F)#gerar um vetor de num aleatorios flag
#flag=sample(1:35331,20000) 
lrn=bb[flag,]; dim(lrn) #arquivo feito para aprendizado da máquina
tst=bb[-flag,];dim(tst) #arquivo feito para o teste



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#cuidado: se a variavel alvo for quali ==> obrigatriamente transformar em quanti
 #rodar a reg log (lembrar que status=1 bom  ==> prob(bom))
fit=glm(data = lrn, STATUS~. ,family = binomial())
summary(fit)
fit2=step(fit, trace=0 ) #novo modelo
summary(fit2)
#Essa adição foi minha
fib=glm(data = tst, STATUS~. ,family = binomial())
summary(fib)
fib2=step(fib, trace=0)
summary(fib2)

################################################################
#    previsao das probabilidades
#################################################################
#calcular pbom para todos da amostra teste
tst$pbom=predict(fit2, newdata = tst, type = "response")
head(tst$pbom)


#previsao para um novo cliente
# magali  eciv=3 idade=38 profissao=6 regiao=5, natureza=5, renda=2000 (lembrar do 300)  
#estado=nosp
names(tst)
magali=data.frame(ECIV="3", IDADE=38, PROFISSAO="6", REGIAO="5", SEXO= "F",   NATUREZA="5",
                  Srenda=sqrt(300),estado="NOSP")
astrogildo= predict(fit2, newdata = magali, type="response");astrogildo


######################################################################################
# classificaçao
#####################################################################################
#SLIDES----->>>>>

#DETERMINAR UM PONTO DE CORTE
pc=.8  #pbom>PC  ==> classificar como bom
KLASS=ifelse(tst$pbom>pc, "aprova","recusa")
table(KLASS, tst$STATUS, dnn=c("classificaccao", "realidade")) #matriz de classificaccao
erro80=(2482+292)/15232; erro80 
erro50=(958+901)/15232;erro50

#vamos supor classificar : aprovar um mau custo $250  e recusar  um bom tem custo de $1000
#custo do erro com pc=,80
custo= 292*250+ 2482*1000;custo
#custo do erro com pc=,50
custo= 958*250+ 901*1000;custo.medio=custo/15232;custo.medio

######################################################################################

# como podemos gerar classes de risco (melhor que ponto de corte)
 #vamos supor que vou dividir em 4 classes  '0.0 - 0.1'   '.1-.50 '  '.50- .80'  '.80-1.00'
kk=c(0.0, 0.1,   0.5,  0.8,  1)

library(arules)
classe=discretize(tst$pbom, method = "fixed", breaks = kk)

m=table(classe, tst$STATUS, dnn = c("class","status"));m
mm=prop.table(m,1);mm
round(mm,3)

#####################################################################################
#testes para avaliar o ajuste do modelo logístico (calibraçao)
####################################################################################
#   >>>>>>>>>>>>>   SLIDES
#Hosmer  & Lemeshow
#Spiegelhalter <-- vcs estudam sozinhos

#avaliar o ajuste do modelo pelo teorema de Zoyowski --. NAO EH MUITO PRECISO
install.packages("arules")
library(arules)
kpbom=discretize(tst$pbom, method = "frequency", breaks=10)
FF=table(kpbom, tst$STATUS,dnn = c('kpbom','status'));FF
prop.table(FF,1)


#############################################################
#       analise da capacidade discriminatória
##############################################################
#SLIDES----->

install.packages("hmeasure")
library (hmeasure)
HMeasure(tst$STATUS, tst$pbom)$metric

library(pROC)
tatu=roc(tst$STATUS, tst$pbom);tatu
plot(tatu, col=3);grid(col=2)
