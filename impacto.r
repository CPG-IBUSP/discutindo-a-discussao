library(lme4)
library(bbmle)
library(ggplot2)
library(grid)
source("funcoes.r")
load("dados.RData")

## Quais os itens que mais afetam o impacto potencial?
imp.m0 <- glmer(impacto.alto ~ nota.outros + (1|aluno), data=impacto, family=binomial)
imp.m1 <- glmer(impacto.alto ~ discussao + nota.outros + (1|aluno), data=impacto, family=binomial)
imp.m2 <- glmer(impacto.alto ~ analises + nota.outros + (1|aluno), data=impacto, family=binomial)
imp.m3 <- glmer(impacto.alto ~ resultados + nota.outros + (1|aluno), data=impacto, family=binomial)
imp.m4 <- glmer(impacto.alto ~ discussao + analises + nota.outros + (1|aluno), data=impacto, family=binomial)
imp.m5 <- glmer(impacto.alto ~ discussao + resultados + nota.outros + (1|aluno), data=impacto, family=binomial)
imp.m5b <- glmer(impacto.alto ~ discussao + resultados + nota.outros + discussao:resultados:nota.outros + (1|aluno),
                 data=impacto, family=binomial)
imp.m6 <- glmer(impacto.alto ~ resultados + analises + nota.outros + (1|aluno), data=impacto, family=binomial)
imp.m7 <- glmer(impacto.alto ~ resultados + analises + discussao + nota.outros + (1|aluno), data=impacto, family=binomial)
AICctab(imp.m0, imp.m1, imp.m2, imp.m3, imp.m4, imp.m5, imp.m5b,imp.m6, imp.m7)


## Calculo das poporcoes e ICs aproximados
imp.summ <- impacto %>%
    mutate(nota.sd.m4=nota.outros>4, rd=paste(resultados,discussao)) %>%
        group_by(rd, nota.sd.m4) %>%
            summarize(media=mean(impacto.alto), N=n()) %>%
                mutate(prop=media, prop.low = p.ci(prop, N)[1,], prop.up=p.ci(prop, N)[2,])
## Estimativas bootstrap dos previstos
myPred2 <- function(.){
    m1 <- model.frame(.)
    pred <- predict(., type="response")
    z <- aggregate(pred, by=list(nota.sd.m4=m1$nota.outros>4, rd=paste(m1$resultados,m1$discussao)), mean)$x
#    names(z) <- c("no.or_leq4","or.leq4","no.or_more4","or.more4")
    return(z)
}
## Observado
myPred2(imp.m5) # ok

imp.m5.boot <- bootMer(imp.m5, myPred2, nsim = 1000, parallel="snow", ncpus=4)
tmp <- bootMer(imp.m5b, myPred2, nsim = 1000, parallel="snow", ncpus=4)

## Adiciona previstos e ICs ao sumário para o grafico
imp.summ$boot.ci.low <- NA
imp.summ$boot.ci.up <- NA
for(i in 1:ncol(imp.m5.boot$t))
        imp.summ[i,c("boot.ci.low","boot.ci.up")] <- boot.ci(imp.m5.boot, index=i, type="perc")$perc[4:5]

## Grafico
imp.f1 <- ggplot(imp.summ, aes(y=prop, x=nota.sd.m4, ymin=boot.ci.low, ymax=boot.ci.up))+
    scale_y_continuous(name="P de impacto alto", labels=comma2) +
        scale_x_discrete(name="N de indicações em 8 outros quesitos", labels=c("Até 4", "> 4")) +
            scale_colour_discrete(name="Excelente em",
                                  breaks=c("TRUE FALSE","FALSE TRUE", "TRUE TRUE", "FALSE FALSE"),
                                  labels=c("Resultados", "Discussão", "Ambos", "Nenhum")) +
                t1

imp.f1 + geom_point(aes(colour=rd),size=5) + geom_line(aes(colour=rd, group=rd)) +
    geom_linerange(data=subset(imp.summ,rd %in% c("TRUE TRUE","FALSE FALSE")),aes(colour=rd)) +
        theme(legend.position=c(0.95,0.89))
