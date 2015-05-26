library(zoo)
library(plyr)
library(dplyr)
library(tidyr)
library(lme4)
library(bbmle)
library(ggplot2)
library(grid)

## Carrega os dados
load("dados.RData")

## Modelos para a nota que a dissertacao obteve
## Testam-se efeitos  dp comitê e do tempo de depósito sobre a nota da tese, com alunos como efeito aleatório
eco.n0 <- lmer(nota.tese~1|aluno, data=eco.m)
eco.n1 <- lmer(nota.tese~comite+(1|aluno), data=eco.m)
eco.n2 <- lmer(nota.tese~tempo.dep+(1|aluno), data=eco.m)
eco.n3 <- lmer(nota.tese~comite+tempo.dep+(1|aluno), data=eco.m)
AICctab(eco.n0, eco.n1, eco.n2, eco.n3)
## Medias das notas dos dois grupos
eco.n0.pred <- eco.m %>%
    group_by(aluno, comite) %>%
        summarize(media=mean(nota.tese), media.t=mean(tempo.dep)) %>%
            group_by(comite) %>%
                summarize(N=n(),se=sd(media)/sqrt(n()),se.t=sd(media.t)/sqrt(n()), media=mean(media), media.t=mean(media.t))%>%
                    mutate(lower=media-se*qt(0.975,df=N-1),
                           upper=media+se*qt(0.975,df=N-1),
                           lower.t=media.t-se.t*qt(0.975,df=N-1),
                           upper.t=media.t+se.t*qt(0.975,df=N-1)
                           )
## Modelos para a discussão
eco.d0 <- glmer(discussao~1|aluno, data=eco.m, family=binomial)
eco.d1 <- glmer(discussao~comite+(1|aluno), data=eco.m, family=binomial)
eco.d2 <- glmer(discussao~tempo.dep+(1|aluno), data=eco.m, family=binomial)
eco.d3 <- glmer(discussao~comite+tempo.dep+(1|aluno), data=eco.m, family=binomial)
AICctab(eco.d0, eco.d1, eco.d2, eco.d3)
## Estimativas pelo modelo vencedor (sem efeitos de comite ou tempo)

## As estimativas e ICs
eco.m$pred.d0 <- predict(eco.d0)
eco.d0.pred <- eco.m %>%
    group_by(aluno, comite) %>%
        summarize(media=mean(pred.d0)) %>%
            group_by(comite) %>%
                summarize(N=n(), media=mean(media))%>%
                    mutate(p=logist(media), p.low = p.ci(p, N)[1,], p.up=p.ci(p, N)[2,])

## Graficos
## Tema para todos os graficos
t1 <- theme_grey()+
    theme(axis.title.x = element_text(vjust=-2, size=23),
          axis.title.y = element_text(vjust=4, size=23),
          plot.margin=unit(c(1,1,2,2),"line"),
          #axis.line=element_line(size=1.1),
          axis.text=element_text(size=22))

## Figura 1: tempo medio
comite.f1 <- ggplot(eco.n0.pred, aes(y=media.t, x=comite, ymin=lower.t,ymax=upper.t))+
    geom_point() + geom_pointrange(size=2, colour="darkblue") +
        scale_y_continuous(limits=c(28,33), name="Tempo de depósito (meses)", labels=comma2) +
            scale_x_discrete(name="Ingresso", labels=c("antes de 2011", "2011 em diante"))
print(comite.f1+t1)
## Figura 2: notas medias
comite.f2 <- ggplot(eco.n0.pred, aes(y=media, x=comite, ymin=lower,ymax=upper))+
    geom_point() + geom_pointrange(size=2, colour="darkblue") +
        scale_y_continuous(limits=c(0,10),name="Média de indicações", labels=comma2) +
            scale_x_discrete(name="Ingresso", labels=c("antes de 2011", "2011 em diante"))
print(comite.f2+t1)
## Figura 3: proporcao de indicacoes para a discussao antes e depois do comitê
comite.f3 <- ggplot(eco.d0.pred, aes(y=p, x=comite, ymin=p.low, ymax=p.up))+
    geom_point() + geom_pointrange(size=2, colour="darkblue") +
        scale_y_continuous(limits = c(0, 1), name="Probabilidade de indicação", labels=comma2) +
            scale_x_discrete(name="Ingresso", labels=c("antes de 2011", "2011 em diante"))
print(comite.f3+t1)

################################################################################
## Variantes da analise: resultados equivalentes
################################################################################
## Incluindo mais uma variavel de qualidade (resultados, a segunda pior)
## Para ter efeito aleatorio do avaliador
eco2.m <-  eco.m %>%
    mutate(cod.avaliador=row.names(.)) %>%
        select(aluno, cod.avaliador, tempo.dep, comite, resultados,discussao) %>%
            gather(key=item, value=excelente, resultados,discussao)

eco.m0 <- glmer(excelente~(1|aluno)+(1|cod.avaliador), data=eco2.m, family=binomial)
eco.m1 <- glmer(excelente~item+(1|aluno)+(1|cod.avaliador), data=eco2.m, family=binomial)
eco.m2 <- glmer(excelente~item+comite+(1|aluno)+(1|cod.avaliador), data=eco2.m, family=binomial)
eco.m3 <- glmer(excelente~item+item:comite+(1|aluno)+(1|cod.avaliador), data=eco2.m, family=binomial)
eco.m4 <- glmer(excelente~item+tempo.dep+(1|aluno)+(1|cod.avaliador), data=eco2.m, family=binomial)
eco.m5 <- glmer(excelente~item*tempo.dep+(1|aluno)+(1|cod.avaliador), data=eco2.m, family=binomial)
eco.m6 <- glmer(excelente~item+comite+tempo.dep+(1|aluno)+(1|cod.avaliador), data=eco2.m, family=binomial)
eco.m7 <- glmer(excelente~item+item:comite+tempo.dep+(1|aluno)+(1|cod.avaliador), data=eco2.m, family=binomial)
eco.m8 <- glmer(excelente~item+item:comite+tempo.dep+item:tempo.dep+(1|aluno)+(1|cod.avaliador), data=eco2.m, family=binomial)

AICctab(eco.m0, eco.m1, eco.m2, eco.m3, eco.m4, eco.m5, eco.m6, eco.m7)

