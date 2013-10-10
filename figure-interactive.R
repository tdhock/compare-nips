works_with_R("3.0.2", animint="2013.10.4", plyr="1.8",reshape2="1.2.2")

load("simulation.samples.RData")

## matrix versions of the norm.
funs <- list(l2=function(x)rowSums(x*x),
             l1=function(x)rowSums(abs(x))^2,
             linf=function(x)apply(abs(x), 1, max)^2)

size.list <- simulation.samples$data
err <- simulation.samples$err
rank.df <- simulation.samples$rank
keep <- seq(-2, 2, by=0.2)
is.ok <- with(rank.df, x1 %in% keep & x2 %in% keep)
is.ok <- TRUE
rank.df <- rank.df[is.ok,]
err$percent <- err$error / err$count * 100
err$set.id <- NA
## sets of training data and bayes error on test data.
sets <- dcast(err, N + seed + norm ~ fit.name, value.var="percent")
sets$diff <- sets$compare-sets$rank
sets$set.id <- 1:nrow(sets)
diff.df <- ddply(sets, .(N, norm), summarize,
                   N=N[1], norm=norm[1],
                   mean=mean(diff), sd=sd(diff))
rank.df$set.id <- NA
train.df <- data.frame()
bayes.df <- data.frame()
for(set.id in sets$set.id){
  e <- sets[set.id,]
  N <- as.character(e$N)
  norm <- as.character(e$norm)
  seed <- as.character(e$seed)
  err$set.id[err$norm == norm & err$N == N & err$seed == seed] <- set.id
  rank.in.set <- rank.df$norm == norm & rank.df$N == N & rank.df$seed == seed
  rank.df$set.id[rank.in.set] <- set.id
  set.list <- size.list[[N]][[seed]][[norm]]
  info <- data.frame(N, norm, seed, set.id)
  ## The Bayes error on the test data set.
  test <- set.list$test
  fun <- funs[[norm]]
  fxdiff <- with(test, fun(Xip)-fun(Xi))
  yhat <- ifelse(fxdiff > 1, 1L,
                 ifelse(fxdiff < -1, -1L, 0))
  table(yhat, test$yi)
  percent <- mean(yhat != test$yi) * 100
  bayes.df <- rbind(bayes.df, data.frame(info, percent))
  ## Train pairs, oriented in the same way:
  pair.df <- with(set.list$train,{
    rbind(data.frame(Xt=Xi[yi==1,],Xtp=Xip[yi==1,],yt=1),
          data.frame(Xt=Xip[yi==-1,],Xtp=Xi[yi==-1,],yt=1),
          data.frame(Xt=Xi[yi==0,],Xtp=Xip[yi==0,],yt=-1))
  })
  train.df <- rbind(train.df, data.frame(pair.df, info))
}
percents <-
  ddply(err, .(N, fit.name, norm), summarize,
        mean=mean(percent),
        se=sd(percent)/sqrt(length(percent)))
model.colors <- c(compare="#00bfc4", #bluish
                  rank="#f8766d",
                  latent="grey")
## Show rank and compare model at the same time.
curves <-
  list(error=ggplot()+
       make_tallrect(percents, "N")+
       geom_ribbon(aes(N, ymin=mean-se, ymax=mean+se, fill=fit.name,
                       group=interaction(fit.name, norm),
                       clickSelects=norm),alpha=3/4,data=percents)+
       geom_line(aes(N, percent, colour=fit.name,
                     group=interaction(fit.name, norm, seed),
                     showSelected=norm,
                     clickSelects=seed),
                 lwd=3,data=err)+
       scale_colour_manual("model",values=model.colors)+
       ggtitle("test error")+
       ylab("percent incorrectly predicted test pairs")+
       xlab("number of labeled pairs in the training set"),
       data=ggplot()+
       make_text(train.df, -2, 3, "N")+
       make_text(train.df, 2, 3, "norm")+
       make_text(train.df, -2, -3, "seed")+
       geom_segment(aes(Xt.1, Xt.2, xend=Xtp.1, yend=Xtp.2, colour=factor(yt),
                      showSelected=N,
                      showSelected2=norm,
                      showSelected3=seed),
                    data=train.df)+
       geom_point(aes(Xtp.1, Xtp.2, colour=factor(yt),
                      showSelected=N,
                      showSelected2=norm,
                      showSelected3=seed),
                  data=subset(train.df, yt==1))+
       scale_colour_manual("label",values=c("1"="red","-1"="black"))+
       ggtitle("training data"))
for(model in c("compare", "rank")){
  sub.df <- subset(rank.df, what %in% c(model, "latent"))
  L <- list(ggplot()+
    geom_contour(aes(x1, x2, z=rank, group=interaction(what, norm, seed, N),
                     colour=what,
                     showSelected=norm,
                     showSelected2=seed,
                     showSelected3=N), data=sub.df)+
    scale_colour_manual("model",values=model.colors)+
    ggtitle(sprintf("learned SVM%s model",model)))
  names(L) <- model
  curves <- c(curves, L)
}
gg2animint(curves, "interactive-both")
## Show rank or compare model on ground truth level curves.
xl <- xlab("feature 1")
yl <- ylab("feature 2")
x.lab <- "number of labeled pairs in the training set"
dots <-
  list(data=ggplot()+
       geom_segment(aes(Xt.1, Xt.2, xend=Xtp.1, yend=Xtp.2, colour=factor(yt),
                        showSelected=set.id),
                    data=train.df)+
       geom_point(aes(Xtp.1, Xtp.2, colour=factor(yt),
                      showSelected=set.id),
                  data=subset(train.df, yt==1))+
       scale_colour_manual("label",values=c("1"="red","-1"="black"))+
       xl+yl+
       ggtitle("training data"),
       error=ggplot()+
       make_text(err, 200, 35, "norm")+
       geom_point(aes(N, percent, colour=fit.name,
                      showSelected=norm,
                      clickSelects=set.id),
                 lwd=3,alpha=3/4,data=err)+
       ylab("percent incorrectly predicted test pairs")+
       scale_colour_manual("model", values=model.colors)+
       xlab(x.lab)+
       ggtitle("test error, select data set"),
       diff=ggplot()+
       geom_ribbon(aes(N, ymin=mean-sd, ymax=mean+sd, group=norm,
                       clickSelects=norm), alpha=1/2,
                   data=diff.df)+
       geom_line(aes(N, mean, group=norm, clickSelects=norm), 
                   data=diff.df)+
       geom_hline(yintercept=0, color="red")+
       geom_text(aes(x,y,label=label),color="red",
                 data=data.frame(x=150,y=1,label="no difference"))+
       ggtitle("test error difference, select norm")+
       xlab(x.lab)+
       ylab("<- compare better (test error percent difference) rank better->"))
for(model in c("compare", "rank")){
  sub.df <- subset(rank.df, what %in% c(model, "latent"))
  L <- list(ggplot()+
    geom_contour(aes(x1, x2, z=rank, group=interaction(what, norm, seed, N),
                     colour=what, showSelected=set.id), data=sub.df)+
    scale_colour_manual("model",values=model.colors)+
            xl+yl+
    ggtitle(sprintf("learned SVM%s model",model)))
  names(L) <- model
  dots <- c(dots, L)
}
gg2animint(dots, "interactive-dots")
