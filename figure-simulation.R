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
  for(i in 1:1000){
    x <- runif(2,-2,2)
    delta <- delta.fun()
    xp <- x+delta
    noise <- 0
    noise <- rnorm(1,sd=1)#comment for noiseless simulation.
    fxdiff <- f(xp)-f(x)+noise
    y <- ifelse(fxdiff < -1, -1L,
                ifelse(fxdiff > 1, 1L, 0L))
    Xi <- rbind(Xi, x)
    Xip <- rbind(Xip, xp)
    yi <- c(yi, y)
  }
  pair.sets[[norm]] <- list(Xi=Xi, Xip=Xip, yi=yi)
}
N <- 200
lapply(pair.sets, with, table(yi))
## Pick exactly N equality and inequality pairs.
pairs.picked <- list()
for(norm in names(pair.sets)){
  Pairs <- pair.sets[[norm]]
  is.zero <- Pairs$yi == 0
  equal <- which(is.zero)
  not.equal <- which(!is.zero)
  i <- c(sample(equal, N), sample(not.equal, N))
  pairs.picked[[norm]] <-
    list(Xi=Pairs$Xi[i,],
         Xip=Pairs$Xip[i,],
         yi=Pairs$yi[i])
}
lapply(pairs.picked, with, table(yi))
## Plot the points.
point.df <- data.frame()
for(norm in names(pairs.picked)){
  m <- with(pairs.picked[[norm]], rbind(Xi, Xip))
  point.df <- rbind(point.df, data.frame(m, norm, row.names=NULL))
}
library(grid)
ggplot()+
  geom_point(aes(X1, X2), data=point.df)+
  facet_grid(.~norm)+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"))+
  coord_equal()
## Looks fine.
levs <- seq(-2,2,l=41)
X.grid <- as.matrix(expand.grid(x1=levs,x2=levs))
all.ranks <- data.frame()
## fit SVM.
for(norm in names(pairs.picked)){
  Pairs <- pairs.picked[[norm]]
  is.zero <- Pairs$yi == 0
  equal <- which(is.zero)
  not.equal <- which(!is.zero)
  i <- c(sample(equal, N/2), sample(not.equal, N/2))
  is.train <- seq_along(Pairs$yi) %in% i
  Pairs.train <- list(Xi=Pairs$Xi[is.train,],
                      Xip=Pairs$Xip[is.train,],
                      yi=Pairs$yi[is.train])
  err.df <- data.frame()
  Cvals <- 3^seq(-2,6,by=1)
  models <- list()
  kvals <- 2^seq(-6, 2, by=1)
  model.df <- expand.grid(C=Cvals, k.width=kvals)
  for(model.i in 1:nrow(model.df)){
    model <- model.df[model.i,]
    Cval <- model$C
    k.width <- model$k.width
    ker <- rbfdot(k.width)
    ##ker <- laplacedot(k.width)
    cat(sprintf("%4d / %4d C=%5.2f k.width=%5.2f\n",
                model.i, nrow(model.df), Cval, k.width))
    fit <- softCompareQP(Pairs.train, ker, C=Cval)
    models[[model.i]] <- fit
    ##X.grid$rank <- fit$rank(as.matrix(X.grid))
    ## pretty good train err:
    yhat <- with(Pairs, fit$predict(Xi, Xip))
    yi <- Pairs$yi
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
  test.err <- subset(err.df, set=="test")
  chosen <- which.min(test.err$error)
  ## if the optimal model occurs on the min/max of the tested
  ## hyperparameters, then this is probably sub-optimal and we need to
  ## define a larger grid.
  overfitPlot <- ggplot(err.df, aes(log2(Cval), error))+
    geom_line(aes(group=set, colour=set))+
    facet_wrap("k.width")+
    theme_bw()+
    theme(panel.margin=unit(0,"cm"))+
    geom_point(data=test.err[chosen,])
  print(overfitPlot)
  stopifnot(test.err[chosen,"Cval"] != range(Cvals))
  stopifnot(test.err[chosen,"k.width"] != range(kvals))

  fit <- models[[chosen]]
  r <- fit$rank(X.grid)
  f <- funs[[norm]]
  rank.df <- rbind(data.frame(X.grid, rank=r-min(r), what="learned"),
                   data.frame(X.grid, rank=apply(X.grid, 1, f), what="latent"))
  normContour <- ggplot(rank.df, aes(x1, x2, z=rank))+
    geom_contour(aes(colour=..level.., linetype=what, group=what))+
    coord_equal()
  print(normContour)
  all.ranks <- rbind(all.ranks, data.frame(rank.df, norm))
}
labels <- c(l1="||x||_1^2",
            l2="||x||_2^2",
            linf="||x||_\\infty^2")
all.ranks$label <- sprintf("$r(x) = %s$", labels[as.character(all.ranks$norm)])
p <- ggplot(all.ranks, aes(x1, x2, z=rank))+
  geom_contour(aes(linetype=what, group=what), colour="black",
               ##breaks=seq(1,10,by=1))+
               breaks=NULL)+
  facet_grid(.~label)+
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

