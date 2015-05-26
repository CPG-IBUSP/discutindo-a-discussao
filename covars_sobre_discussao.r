library(zoo)
library(plyr)
library(dplyr)
library(tidyr)
library(lme4)
library(bbmle)
library(ggplot2)
library(grid)
library(MuMIn)
load("dados.RData")


## Selecao de modelos: outros indicadores na ficha correlacionam com probabilidade de boa discussão?

## Efeito de programa, titulo, tempo de deposito relativo ao maximo e nota em outro quesitos na avaliacao da discussao 
f.m0 <- glmer(discussao ~ 1|aluno, data=ficha, family=binomial)
f.m1 <- glmer(discussao ~ titulo + (1|aluno), data=ficha, family=binomial)
f.m2 <- glmer(discussao ~ programa + (1|aluno), data=ficha, family=binomial)
f.m3 <- glmer(discussao ~ atrasou + (1|aluno), data=ficha, family=binomial)
f.m4 <- glmer(discussao ~ nota8 + (1|aluno), data=ficha, family=binomial)
f.m5 <- glmer(discussao ~ titulo + programa + (1|aluno), data=ficha, family=binomial)
f.m6 <- glmer(discussao ~ titulo + atrasou + (1|aluno), data=ficha, family=binomial)
f.m7 <- glmer(discussao ~ titulo + nota8 + (1|aluno), data=ficha, family=binomial)
f.m7b <- glmer(discussao ~ titulo + titulo:atrasou + nota8 + (1|aluno), data=ficha, family=binomial)
f.m8 <- glmer(discussao ~ programa + atrasou + (1|aluno), data=ficha, family=binomial)
f.m9 <- glmer(discussao ~ programa + nota8 + (1|aluno), data=ficha, family=binomial)
f.m10 <- glmer(discussao ~ atrasou + nota8 + (1|aluno), data=ficha, family=binomial)
f.m11 <- glmer(discussao ~ titulo + programa + atrasou + (1|aluno), data=ficha, family=binomial)
f.m12 <- glmer(discussao ~ titulo + programa + nota8 + (1|aluno), data=ficha, family=binomial)
f.m13 <- glmer(discussao ~ titulo + nota8 + atrasou + (1|aluno), data=ficha, family=binomial)
f.m14 <- glmer(discussao ~ nota8 + programa + atrasou + (1|aluno), data=ficha, family=binomial)
f.m15 <- glmer(discussao ~ titulo + programa + atrasou + nota8 + (1|aluno), data=ficha, family=binomial) 
## Selecao
## lista de modelos
f.list <- list(f.m0, f.m1, f.m2, f.m3, f.m4, f.m5, f.m6, f.m7, f.m7b, f.m8, f.m9, f.m10, f.m11, f.m12, f.m13, f.m14, f.m15)
names(f.list) <- sapply(f.list, function(x) as.character(formula(x))[3])
names(f.list) <- sub(" . .1 . aluno.", "", names(f.list))
names(f.list) <- paste("~",names(f.list))
names(f.list)[1] <- "~ 1"

AICctab(f.m0, f.m1, f.m2, f.m3, f.m4, f.m5, f.m6, f.m7, f.m7b, f.m8, f.m9, f.m10, f.m11, f.m12, f.m13, f.m14, f.m15)
AICctab(f.list)

##  resumo dos modelos plausíveis
model.sel(list(f.m4, f.m7, f.m10))
## Verificando efeitos e seus desvios
summary(f.m7)
summary(f.m4)
summary(f.m10)

## Coeficiente de tempo muito fraco e com erro padrão 25 vezes maior que o coeficiente
## 
## Ainda assim usamos o modelo médio para fazer previsoes
f.avg <- model.avg(list(f.m7, f.m4, f.m10))
summary(f.avg)

## Graficos dos previstos x observados
## tema para os graficos
t1 <- theme_grey()+
    theme(axis.title.x = element_text(vjust=-2, size=23),
          axis.title.y = element_text(vjust=4, size=23),
          plot.margin=unit(c(1,1,2,2),"line"),
          #axis.line=element_line(size=1.1),
          axis.text=element_text(size=22))
t2 <- theme_bw()+
    theme(axis.title.x = element_text(vjust=-2, size=23),
          axis.title.y = element_text(vjust=4, size=23),
          plot.margin=unit(c(1,1,2,2),"line"),
          axis.text=element_text(size=22))

## Incluindo efeito do tempo
ficha$pred.avg <- predict(f.avg)
## Vemos que o efeito do tempo no modelo medio não é importante:
covar.fS1 <- ficha %>%
    mutate(nota.q=cut(nota8, c(-1,3,6,9))) %>%
        group_by(titulo, nota.q, atrasou) %>%
            summarize(discussao=mean(discussao), N=n(), se=sd(pred.avg)/sqrt(N), media=mean(pred.avg)) %>%
                mutate(p=logist(media), p.low = p.ci(p, N)[1,], p.up=p.ci(p, N)[2,]) %>%
                    ggplot(aes(y=discussao, x=nota.q, ymin=p.low, ymax=p.up, colour=atrasou))+
                        geom_point(size=5, alpha=0.5) + geom_linerange(alpha=0.5)+ facet_grid(. ~ titulo) +
                            scale_y_continuous(name="Proporção de indicações", labels=comma2) +
                                scale_x_discrete(name="N de indicações em 8 outros quesitos") +
                                    t1
print(covar.fS1)                                   

## Usando apenas titulo e nota em escala de 0 a 8
covar.fS2 <- ficha %>%
    group_by(titulo, nota8) %>%
        summarize(discussao=mean(discussao), N=n(), se=sd(pred.avg), media=mean(pred.avg)) %>%
            mutate(disc.logit=log(discussao/(1-discussao)),
                   lower=media-se*qt(0.975,df=N-1),
                   upper=media+se*qt(0.975,df=N-1),
                   p=logist(media), p.low = p.ci(p, N)[1,], p.up=p.ci(p, N)[2,]) %>%
                ggplot(aes(y=discussao, x=nota8, ymin=p.low, ymax=p.up))+
                    scale_y_continuous(name="Proporção de indicações", labels=comma2) +
                        scale_x_continuous(name="N de indicações em 8 outros quesitos") +
                            t1
## Vemos que o efeito de titulo tb é pequeno e incerto:
print(covar.fS2 + geom_point(size=5) + geom_linerange(col="darkgrey")+ facet_grid(. ~ titulo) +t2)
covar.fS2 + geom_point(size=5, aes(colour=titulo))
covar.fS2 + geom_point(size=5, aes(colour=titulo))+geom_line(aes(y=p, colour=titulo)) # com linha do previsto


## Em resumo, unico efeito certo é o da nota nos outros quesitos
pred.covar.final <- ficha %>%
    group_by(nota8) %>%
        summarize(discussao=mean(discussao), N=n(), media=mean(pred.avg)) %>%
            mutate(d.low=p.ci(discussao, N)[1,], d.up=p.ci(discussao, N)[2,],
                   p=logist(media), p.low = p.ci(p, N)[1,], p.up=p.ci(p, N)[2,])

## A figura final: proporcoes observadas e seus intervalos de confiança
covar.f1 <- ggplot(pred.covar.final, aes(y=discussao, x=nota8, ymin=d.low, ymax=d.up))+
    scale_y_continuous(name="P de discussão excelente", labels=comma2) +
        scale_x_continuous(name="N de indicações em 8 outros quesitos") +
            geom_point(size=5, col="darkblue")
print(covar.f1 + geom_linerange(col="darkblue") + t1)
## Suplementar: proporcoes observadas com os ics das estimativas
print(covar.f1+geom_ribbon(aes(ymin=p.low, ymax=p.up), alpha=0.5)+geom_line(aes(y=p)))
