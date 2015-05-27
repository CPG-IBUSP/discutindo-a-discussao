library(lme4)
library(bbmle)
library(ggplot2)
library(grid)
source("funcoes.r")
load("dados.RData")

## Quais os itens que mais afetam o impacto potencial?
imp.m0 <- glmer(impacto.alto ~ nota.outros + (1|aluno), data=impacto, family=binomial)
imp.m1 <- glmer(impacto.alto ~ discussao + nota.outros + (1|aluno), data=impacto, family=binomial)
imp.m2 <- glmer(impacto.alto ~ resultados + nota.outros + (1|aluno), data=impacto, family=binomial)
imp.m3 <- glmer(impacto.alto ~ discussao + resultados + nota.outros + (1|aluno), data=impacto, family=binomial)
imp.m4 <- glmer(impacto.alto ~ discussao + resultados + nota.outros + discussao:resultados:nota.outros + (1|aluno),
                 data=impacto, family=binomial)
AICctab(imp.m0, imp.m1, imp.m2, imp.m3, imp.m4)

## Lista dos modelos
imp.list <- list(imp.m0, imp.m1, imp.m2, imp.m3, imp.m4)
names(imp.list) <- sapply(imp.list, function(x) as.character(formula(x))[3])
names(imp.list) <- sub(" . .1 . aluno.", "", names(imp.list))
names(imp.list) <- sub("discussao", "D", names(imp.list))
names(imp.list) <- sub("resultados", "R", names(imp.list))
names(imp.list) <- sub("nota.outros", "N", names(imp.list))
names(imp.list) <- paste("~",names(imp.list))
names(imp.list)[1] <- "~ 1"
AICctab(imp.list, weights=TRUE)

## Model averaging
imp.avg <- model.avg(imp.m3, imp.m4)
## Calculo das poporcoes e ICs aproximados
impacto$pred <- predict(imp.avg)
imp.summ <- impacto %>%
    mutate(nota.sd.m4=nota.outros>4, rd=paste(resultados,discussao)) %>%
        group_by(rd, nota.sd.m4) %>%
            summarize(media=mean(impacto.alto), pred=mean(pred), N=n()) %>%
                mutate(prop=media, prop.low = p.ci(prop, N)[1,], prop.up=p.ci(prop, N)[2,],
                       p.pred=logist(pred), p.pred.low=p.ci(p.pred, N)[1,], p.pred.up=p.ci(p.pred, N)[2,])

## Estimativas bootstrap dos previstos por dois dos modelos com efeito da discussão
myPred2 <- function(.){
    m1 <- model.frame(.)
    pred <- predict(., type="response")
    z <- aggregate(pred, by=list(nota.sd.m4=m1$nota.outros>4, rd=paste(m1$resultados,m1$discussao)), mean)$x
    return(z)
}
## Observado
myPred2(imp.m5) # ok
## Bootstrap
n.sim <- 1000
imp.m3.boot <- bootMer(imp.m3, myPred2, nsim = n.sim, parallel="snow", ncpus=4)
save.image()
imp.m4.boot <- bootMer(imp.m4, myPred2, nsim = n.sim, parallel="snow", ncpus=4)
save.image()
## Adiciona previstos e ICs ao sumário para o grafico
imp.summ$boot.m3.ci.low <- NA
imp.summ$boot.m3.ci.up <- NA
imp.summ$boot.m4.ci.low <- NA
imp.summ$boot.m4.ci.up <- NA
for(i in 1:ncol(imp.m3.boot$t)){
    imp.summ[i,c("boot.m3.ci.low","boot.m3.ci.up")] <- boot.ci(imp.m3.boot, index=i, type="perc")$perc[4:5]
    imp.summ[i,c("boot.m4.ci.low","boot.m4.ci.up")] <- boot.ci(imp.m4.boot, index=i, type="perc")$perc[4:5]
}
## Um IC conservador: o intervalo mais amplo que combina os dois ICs
imp.summ$boot.ci.low <- apply(imp.summ[, c("boot.m3.ci.low", "boot.m4.ci.low")], 1,min)
imp.summ$boot.ci.up <- apply(imp.summ[, c("boot.m3.ci.up", "boot.m4.ci.up")], 1,max)

## Grafico
imp.f1 <- ggplot(imp.summ, aes(y=prop, x=nota.sd.m4, ymin=boot.ci.low, ymax=boot.ci.up))+
        scale_y_continuous(name="P de impacto alto", labels=comma2) +
            scale_x_discrete(name="N de indicações em 8 outros quesitos", labels=c("Até 4", "> 4")) +
                scale_colour_discrete(name="Excelente em",
                                      breaks=c("TRUE FALSE","FALSE TRUE", "TRUE TRUE", "FALSE FALSE"),
                                      labels=c("Resultados", "Discussão", "Ambos", "Nenhum")) + t1

imp.f1 + geom_point(aes(colour=rd),size=5) + geom_line(aes(colour=rd, group=rd)) +
    geom_linerange(data=subset(imp.summ,rd %in% c("TRUE TRUE","FALSE FALSE")),aes(colour=rd)) +
        theme(legend.position=c(0.95,0.89))
