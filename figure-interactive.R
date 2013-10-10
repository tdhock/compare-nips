works_with_R("3.0.2", animint="2013.10.4", plyr="1.8",reshape2="1.2.2")

load("simulation.samples.RData")

size.list <- simulation.samples$data
err <- simulation.samples$err
err$percent <- err$error / err$count * 100
err$set.id <- NA
## sets of training data.
sets <- dcast(err, N + seed + norm ~ fit.name, value.var="percent")
sets$set.id <- 1:nrow(sets)
train.df <- data.frame()
for(set.id in sets$set.id){
  e <- sets[set.id,]
  N <- as.character(e$N)
  norm <- as.character(e$norm)
  seed <- as.character(e$seed)
  err$set.id[err$norm == norm & err$N == N & err$seed == seed] <- set.id
  set.list <- size.list[[N]][[seed]][[norm]]
  info <- data.frame(N, norm, seed, set.id)
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
## Show rank and compare model at the same time.
both <-
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
gg2animint(both, "interactive-both")
## Show rank or compare model on ground truth level curves.
one <-
  list(error=ggplot()+
       make_text(err, 200, 35, "norm")+
       geom_point(aes(N, percent, colour=fit.name,
                      showSelected=norm,
                      clickSelects=set.id),
                 lwd=3,alpha=3/4,data=err)+
       ylab("percent incorrectly predicted test pairs")+
       xlab("number of labeled pairs in the training set")+
       ggtitle("test error"),
       data=ggplot()+
       geom_segment(aes(Xt.1, Xt.2, xend=Xtp.1, yend=Xtp.2, colour=factor(yt),
                        showSelected=set.id),
                    data=train.df)+
       geom_point(aes(Xtp.1, Xtp.2, colour=factor(yt),
                      showSelected=set.id),
                  data=subset(train.df, yt==1))+
       scale_colour_manual("label",values=c("1"="red","-1"="black"))+
       ggtitle("training data"))
gg2animint(one, "interactive-one")
