works_with_R("3.0.2", rankSVMcompare="2013.9.3")

source("tikz.R")

funs <- list(l2=function(x)sum(x*x),
             l1=function(x)sum(abs(x))^2,
             linf=function(x)max(abs(x))^2)
deltas <- list(l2=function()runif(2,-1,1/2),
               l1=function()runif(2,-1/2,1/2),
               linf=function()runif(2,-1,1))
set.seed(1)
pair.sets <- list()
for(norm in names(funs)){
  delta.fun <- deltas[[norm]]
  f <- funs[[norm]]
  Xi <- c()
  Xip <- c()
  yi <- c()
  for(i in 1:500){
    x <- runif(2,-2,2)
    delta <- delta.fun()
    xp <- x+delta
    noise <- 0
    noise <- rnorm(1,sd=1/2)#comment for noiseless simulation.
    fxdiff <- f(xp)-f(x)+noise
    y <- ifelse(fxdiff < -1, -1L,
                ifelse(fxdiff > 1, 1L, 0L))
    Xi <- rbind(Xi, x)
    Xip <- rbind(Xip, xp)
    yi <- c(yi, y)
  }
  pair.sets[[norm]] <- list(Xi=Xi, Xip=Xip, yi=yi)
}
lapply(pair.sets, with, table(yi))

levs <- seq(-2,2,l=41)
X.grid <- as.matrix(expand.grid(x1=levs,x2=levs))
all.ranks <- data.frame()
## fit SVM.
for(norm in names(pair.sets)){
  Pairs <- pair.sets[[norm]]
  fold <- sample(rep(1:2, l=length(Pairs$yi)))
  is.train <- fold==1
  err.df <- data.frame()
  Cvals <- 2^seq(-2,2,by=1)
  models <- list()
  kvals <- Cvals
  model.df <- expand.grid(C=Cvals, k.width=kvals)
  for(model.i in 1:nrow(model.df)){
    model <- model.df[model.i,]
    Cval <- model$C
    k.width <- model$k.width
    ker <- rbfdot(k.width)
    ker <- laplacedot(k.width)
    cat(sprintf("%4d / %4d C=%5.2f k.width=%5.2f\n",
                model.i, nrow(model.df), Cval, k.width))
    fit <- softCompareQP(Pairs, ker, C=Cval)
    models[[model.i]] <- fit
    ##X.grid$rank <- fit$rank(as.matrix(X.grid))
    ## pretty good train err:
    yhat <- fit$predict(Xi, Xip)
    table(yi, yhat)
    sets <- list(train=is.train, test=!is.train)
    for(set in names(sets)){
      s <- sets[[set]]
      pred <- yhat[s]
      true <- yi[s]
      err.df <- rbind(err.df, {
        data.frame(FpFnInv(true, pred), Cval, k.width, set, norm)
      })
    }
  }
## train/test error curves.
library(grid)
ggplot(err.df)+
  geom_line(aes(log2(Cval), error, group=set, colour=set))+
  facet_wrap("k.width")+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"))

  test.err <- subset(err.df, set=="test")
  chosen <- which.min(test.err$error)
  fit <- models[[chosen]]
  r <- fit$rank(X.grid)
  f <- funs[[norm]]
  rank.df <- rbind(data.frame(X.grid, rank=r-min(r), what="learned"),
                   data.frame(X.grid, rank=apply(X.grid, 1, f), what="latent"))
  ggplot(rank.df, aes(x1, x2, z=rank))+
    geom_contour(aes(colour=..level.., linetype=what, group=what))
  all.ranks <- rbind(all.ranks, data.frame(rank.df, norm))
}
labels <- c(l1="||x||_1^2",
            l2="||x||_2^2",
            lint="||x||_\\infty^2")
p <- ggplot(all.ranks, aes(x1, x2, z=rank))+
  geom_contour(aes(linetype=what, group=what), colour="black",
               ##breaks=seq(1,10,by=1))+
               breaks=NULL)+
  facet_grid(.~norm,labeller=function(var, val){
    sprintf("$r(x) = %s$", labels[val])
  })+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"))+
  coord_equal()+
  scale_linetype_discrete("ranking\nfunction")+
  xlab("feature 1")+
  ylab("feature 2")
print(p)

tikz("figure-simulation.tex", h=2.5)
print(p)
dev.off()

