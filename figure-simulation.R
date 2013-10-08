load("simulation.RData")

source("tikz.R")

all.ranks <- simulation$rank
labels <- c(l1="||x||_1^2",
            l2="||x||_2^2",
            linf="||x||_\\infty^2")
all.ranks$label <- sprintf("$r(x) = %s$", labels[as.character(all.ranks$norm)])
toplot <- subset(all.ranks, what!="latent")
##latent <- subset(all.ranks, what=="latent")
toplot$what <- factor(toplot$what, c("rank","compare"))
##toplot <- all.ranks
toplot$fun <- ifelse(toplot$what=="latent", "latent", "learned")
library(grid)
p <- ggplot(toplot, aes(x1, x2, z=rank))+
  geom_contour(aes(colour=what, group=what), size=1.2)+
  ##geom_contour(data=latent)+
  facet_grid(.~label)+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"))+
  coord_equal()+
  scale_colour_manual("SVM\ntype",values=c(compare="black",rank="red"))+
  ##scale_linetype_manual("function",values=c(latent="dashed",learned="solid"))+
  ##  scale_size_manual("function",values=c(latent=1,learned=1))+
  xlab("feature 1")+
  ylab("feature 2")
print(p)

tikz("figure-simulation.tex", h=2.8)
print(p)
dev.off()

