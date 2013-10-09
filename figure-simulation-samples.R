works_with_R("3.0.2", plyr="1.8")

source("tikz.R")

load("simulation.samples.RData")

err <- simulation.samples$err
err$percent <- err$error / err$count * 100
percents <-
  ddply(err, .(N, fit.name, norm), summarize,
        mean=mean(percent),
        se=sd(percent)/sqrt(length(percent)))
library(grid)
percents$fit.name <- factor(percents$fit.name, c("rank", "compare"))
labels <- c(l1="||x||_1^2",
            l2="||x||_2^2",
            linf="||x||_\\infty^2")
percents$label <- sprintf("$r(x) = %s$", labels[as.character(percents$norm)])
leg <- "SVM\ntype"
boring <- ggplot(percents, aes(N, mean, group=fit.name))+
  geom_ribbon(aes(ymin=mean-se,ymax=mean+se,fill=fit.name),alpha=1/2)+
  geom_line(aes(colour=fit.name),lwd=1.5)+
  facet_grid(.~label)+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"))+
  scale_colour_discrete(leg)+
  scale_fill_discrete(leg)+
  ylab("percent incorrectly predicted test pairs")+
  xlab("$n=$ number of labeled pairs in the training set")

tikz("figure-simulation-samples.tex",h=3)
print(boring)
dev.off()
