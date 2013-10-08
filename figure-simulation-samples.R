works_with_R("3.0.2", plyr="1.8")

source("tikz.R")

load("simulation.samples.RData")
err <- simulation.samples$err
err$percent <- err$error / err$count * 100
percents <-
  ddply(err, .(N, fit.name, norm), summarize,
        mean=mean(percent),
        sd=sd(percent))
library(grid)
boring <- ggplot(percents, aes(N, mean, group=fit.name))+
  geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd,fill=fit.name),alpha=1/2)+
  geom_line(aes(colour=fit.name))+
  facet_grid(.~norm)+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"))
tikz("figure-simulation-samples.tex",h=3)
print(boring)
dev.off()
