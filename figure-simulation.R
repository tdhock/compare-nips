load("simulation.RData")

source("tikz.R")

all.ranks <- simulation$rank

labels <- c(l1="||x||_1^2",
            l2="||x||_2^2",
            linf="||x||_\\infty^2")
all.ranks$label <- sprintf("$r(x) = %s$", labels[as.character(all.ranks$norm)])
not.latent <- subset(all.ranks, what!="latent")
p <- ggplot(not.latent, aes(x1, x2, z=rank))+
  geom_contour(aes(colour=what, group=what))+
  facet_grid(.~label)+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"))+
  coord_equal()+
  scale_linetype_discrete("SVM\ntype")+
  xlab("feature 1")+
  ylab("feature 2")
print(p)

tikz("figure-simulation.tex", h=2.5)
print(p)
dev.off()

