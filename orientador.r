library(lme4)
library(bbmle)
library(ggplot2)
library(grid)
source("funcoes.r")
load("dados.RData")

## Há diferenças na avaliação do orientador?
## Separamos as fichas paras as quais há avaliação do orientador
## Total de fichas
length(unique(com.orient$aluno))

## Efeito na avaliacao da discussao orientador x outros
or.d0 <- glmer(discussao ~ nota.m4 + (1|aluno), data=com.orient, family=binomial)
or.d1 <- glmer(discussao ~ orientador + nota.m4 + (1|aluno), data=com.orient, family=binomial)
or.d2 <- glmer(discussao ~ orientador + titulo + nota.m4 + (1|aluno), data=com.orient, family=binomial)
or.d3 <- glmer(discussao ~ orientador + titulo + orientador:titulo + nota.m4 + (1|aluno), data=com.orient, family=binomial)
or.d4 <- glmer(discussao ~ orientador +  nota.m4 + orientador:nota.m4 + (1|aluno), data=com.orient, family=binomial)
AICctab(or.d0, or.d1, or.d2, or.d3, or.d4)
summary(or.d4)

## Lista dos modelos
or.list <- list(or.d0, or.d1, or.d2, or.d3, or.d4)
names(or.list) <- sapply(or.list, function(x) as.character(formula(x))[3])
names(or.list) <- sub(" . .1 . aluno.", "", names(or.list))
names(or.list) <- paste("~",names(or.list))
names(or.list)[1] <- "~ 1"
AICctab(or.list, weights=TRUE)
## Graficos
## Previstos por nota e tipo de avaliador
## Objeto com previstos por categoria de orientador e nota
com.orient$pred.p <- predict(or.d4)
or.pred <- com.orient %>%
        group_by(nota.m4, orientador) %>%
            summarize(discussao=mean(discussao), N=n(), media=mean(pred.p)) %>%
                mutate(d.low=p.ci(discussao, N)[1,], d.up=p.ci(discussao, N)[2,],
                       p=logist(media), p.low = p.ci(p, N)[1,], p.up=p.ci(p, N)[2,])

## Estimativas bootstrap dos previstos
myPred <- function(.){
    m1 <- model.frame(.)
    pred <- predict(., type="response")
    z <- aggregate(pred, by=list(or=m1$orientador, nota.m4=m1$nota.m4), mean)$x
    names(z) <- c("no.or_leq4","or.leq4","no.or_more4","or.more4")
    return(z)
}
## ICs bootstrap
myPred(or.d4)
or.d4.boot <- bootMer(or.d4, myPred, nsim = 1000, parallel="snow", ncpus=4)
str(boot.ci(or.d4.boot, index=3, type="perc"))
boot.ci(or.d4.boot, index=4, type="perc")
boot.ci(or.d4.boot, type="perc")

## Acrescenta os ICs
or.pred$boot.ci.low <- NA
or.pred$boot.ci.up <- NA
for(i in 1:ncol(or.d4.boot$t))
        or.pred[i,c("boot.ci.low","boot.ci.up")] <- boot.ci(or.d4.boot, index=i, type="perc")$perc[4:5]
## Grafico 
or.f1 <- ggplot(or.pred, aes(y=discussao, x=nota.m4, ymin=boot.ci.low, ymax=boot.ci.up))+
    scale_y_continuous(name="Proporção de indicações", labels=comma2) +
        scale_x_discrete(name="N de indicações nos 8 outros quesitos", labels=c("Até 4", "> 4"))
## com intervalos de confianca bootstrap para os previstos
or.f1 + geom_point(size=5, aes(colour=orientador)) +
    geom_line(aes(group=orientador, colour=orientador)) +
        geom_linerange(aes(colour=orientador)) +
            scale_colour_discrete(name="", breaks=c("TRUE","FALSE"), labels=c("Orientador", "Outros")) +
                t1 + theme(legend.position=c(0.9,0.9))

