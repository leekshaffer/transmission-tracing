require(ggplot2)
require(gridExtra)
require(patchwork)
require(RColorBrewer)

colors <- brewer.pal(5, "RdPu")[5:2]

TestEff <- function(R0, k, a) {
  p <- (R0/k)/(1+R0/k)
  Ratio <- 1-(1-a)*(1+a*R0/k)^(-1*(1+k))
  P.Iso <- 1-(1+a*R0/k)^(-1*k)
  return(c(a=a, k=k, R0=R0, Ratio=Ratio, P.Iso=P.Iso))
}


R0s <- c(1.5,2,2.5)
ks <- seq(0.1,1,by=.3)
a <- seq(0,1,by=.01)

df <- expand.grid(R0=R0s, k=ks, a=a)
Res.df <- apply(X=df, MARGIN=1, FUN=function(x) unname(TestEff(x[1],x[2],x[3])))
rownames(Res.df) <- c("a","k","R0","Ratio","P.Iso")
FullRes <- merge(df, as.data.frame(t(Res.df)), by=c("a","k","R0"), all=TRUE)

FullRes$k <- as.factor(FullRes$k)
FullRes$R0lab <- as.factor(paste0("R[0]==",FullRes$R0))
FullRes$R0 <- as.factor(FullRes$R0)

p.c.1 <- ggplot(data=FullRes, aes(x=a*100, y=Ratio*100, group=k, color=k)) + 
  geom_line(size=0.5) +
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(limit=c(0,100), expand=c(0,0)) +
  scale_color_manual(name="Dispersion parameter (k)", values=colors) + 
  theme_bw() + theme(legend.position="top", panel.spacing = unit(1, "lines")) +
  labs(x=NULL, y="Secondary cases isolated (%)") + 
  geom_abline(intercept=0, slope=1, col="grey70", size=0.5) + 
  facet_wrap(facets=vars(R0lab), nrow=1, ncol=3, labeller=label_parsed)
p.c.1

p.c.2 <- ggplot(data=FullRes, aes(x=a*100, y=P.Iso*100, group=k, color=k)) + 
  geom_line(size=0.5) +
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(limit=c(0,100), expand=c(0,0)) + 
  scale_color_manual(name="Dispersion parameter (k)", values=colors) + 
  theme_bw() + theme(legend.position="top", panel.spacing = unit(1, "lines")) +
  labs(x="Test Sensitivity (%)", y="Index cases whose contacts are isolated (%)") + 
  facet_wrap(facets=vars(R0lab), nrow=1, ncol=3, labeller=label_parsed)
p.c.2

ggsave(filename="Fig2.eps", 
       plot=(p.c.1+ theme(legend.position = "none", axis.title=element_text(size=10))) + 
         (p.c.2 + theme(legend.position = "bottom", axis.title=element_text(size=10))) + 
         plot_layout(ncol=1, nrow=2),
       width=8, height=6, units="in", dpi=600)


