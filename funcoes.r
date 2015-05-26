## Funcao para convreter logitos em probabilidades
logist <- function(x) exp(x)/(1+exp(x))
## Funcao para intervalo de confiança de proporcoes
p.ci <- function(prop, n, x){
    if(missing(prop))
        z <- prop.test(x, n)$conf.int
    else
        z <- prop.test(prop*n, n)$conf.int
    as.numeric(z)
}
p.ci <- Vectorize(p.ci)
## Funcao para usar virgula como separador decimal em graficos
comma2 <- function(x, ...) {
format(x, decimal.mark = ",", trim = TRUE, scientific = FALSE, ...)
}
