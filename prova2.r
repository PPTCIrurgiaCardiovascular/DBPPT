db <- liv
install.packages("rpart")
options(scipen=999)
par(mfrow=c(2,2)) 
set.seed(1234)
head (db)
names(db)
class(db)
str(db)
table(db$STATUS)
db$STATUS=ifelse(db$STATUS=="MAU",1,0)
db$RESID=ifelse(db$RESID == "PROP",1,0)
db$FONE=ifelse(db$FONE == "SIM",1,0)
db$CARTAO=ifelse(db$CARTAO == "SIM",1,0)
db$RESTR=ifelse(db$RESTR == "SIM",1,0)
db$NAO_FICCAO=ifelse(db$NAO_FICCAO == "SIM",1,0)
db$AUTO_AJUDA=ifelse(db$AUTO_AJUDA == "SIM",1,0)
db$CATEG=ifelse(db$CATEG == "SIM",1,0)
#Verificar os pressupostos
#VD (o que eu quero prever) -> Status
summary(db)
summary(db$STATUS)


#Regressão logistica
summary(db$IDADE)
boxplot(db$IDADE~db$STATUS, main="idade", col=11);grid(col=2)
quantile(db$IDADE, probs = seq(0,1,.05))
hist(db$IDADE, col = 18)
boxplot(db$RESID~db$STATUS, main="Residencia", col=2);grid(col=4)
table(db$UNIFED, useNA="ifany")
db$estado = ifelse(liv$UNIFED !="SP", "NOSP", "SP")
summary(db$estado)

library(caret)

set.seed(1234)
flag=createDataPartition(db$STATUS, p=.6, list=F)
lrn=db[flag,]; dim(lrn)
tst=db[-flag,]; dim(tst)
summary(tst)
fit=glm(data = lrn, STATUS~. ,family = binomial())
summary(fit)
fit2=step(fit, trace=0 ) #novo modelo
summary(fit2)
tst$pmau=predict(fit2, newdata = tst, type = "response")
head(tst$pmau)
#Questão 2A1


novo.indiv=data.frame(IDADE=30,UNIFED="RJ",RESID=1,FONE=1,INSTRU="SUP",CARTAO=1,RESTR=1, NAO_FICCAO=0,AUTO_AJUDA=1,CATEG=1)
novo.p=predict(fit2, novo.indiv, type="response");novo.p

#Questão 2A2

kk=c(0.0, 0.1,   0.5,  0.8,  1)

library(arules)
classe=discretize(tst$pbom, method = "fixed", breaks = kk)

m=table(classe, tst$STATUS, dnn = c("class","status"));m
mm=prop.table(m,1);mm
round(mm,3)

kpmau=discretize(tst$pmau, method = "frequency", breaks=10)
FF=table(kpmau, tst$STATUS,dnn = c('kpmau','status'));FF
prop.table(FF,1)

library (hmeasure)
HMeasure(tst$STATUS, tst$pmau)$metric

library(pROC)
tatu=roc(tst$STATUS, tst$pmau);tatu
plot(tatu, col=3);grid(col=2)


#Questão 2A3
pc=.4  #pmau>PC  ==> classificar como mau
KLASS=ifelse(tst$pmau>pc, "recusa","aprova")
table(KLASS, tst$STATUS, dnn=c("classificaccao", "realidade")) #matriz de classificaccao
erro40=(121+73)/3000; erro40 
#reprovar o bom $500, aprovar o mau $800
custo=73*500+121*800; custo

#Questão 2B1
set.seed(1234)
novo.indiv2b1=data.frame(IDADE=30,UNIFED="RJ",RESID=0,FONE=1,INSTRU="SUP",CARTAO=1,RESTR=1, NAO_FICCAO=0,AUTO_AJUDA=1,CATEG=1)
novo.p2b1=predict(fit2, novo.indiv2b1, type="response");novo.p2b1
round(novo.p2b1,3)


