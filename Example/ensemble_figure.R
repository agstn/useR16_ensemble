pacman::p_load(dplyr,tidyr)
pacman::p_load(lattice,latticeExtra)

e1 <- data.frame(model = "ensemble",
                 metalearner = c("nnls","deeplearning"),
                 rep = 1,
                 AUC=c(0.789151875671175,0.787556901593124))

e2 <- data.frame(model = "ensemble_cv",
                 metalearner = c("nnls","deeplearning"),
                 rep = 1:3,
                 AUC = c(0.7850711,0.7857685,0.7855375,
                         0.7853144,0.7846356,0.7856505))

e <- rbind(e1,e2)

e$metalearner <- as.factor(e$metalearner)
e$metalearner <- relevel(e$metalearner,"nnls")

f1 <- xyplot(AUC~metalearner|model,data=e, groups=rep,
       cex=1.2, pch=16,
       between=list(x=0.5),
       xlab="",
       par.settings=list(strip.background=list(col="gray90")),
       scales=list(y=list(alternating=3, tck=c(1,1)),
                   x=list(tck=c(1,0), alternating=1)),
       ylim=c(0.75,0.80),
       panel = function(x,y,...){
         m <- tapply(y,x,mean)
         panel.segments(x0=c(0.7,1.7),y0=m,x1=c(1.3,2.3),y1=m,col='gray50',lwd=2)
         panel.superpose(x,y,...)
         # if (panel.number()==1) panel.xyplot(x,y,...)
         # if (panel.number()==2) panel.xyplot(x,y,jitter.x=T,amount=0.30,...)
       },
       panel.groups = function(x,y,...){
         if (panel.number()==1) panel.xyplot(x,y,col='black',cex=1.2,pch=16)
         if (panel.number()==2) panel.xyplot(x,y,jitter.x=T,amount=0.30,...)
       }
)

e3 <- data.frame(model = "ensemble_cv",
                 metalearner =  "nnls",
                 name = c('Fold1.Rep1','Fold2.Rep1','Fold3.Rep1','Fold4.Rep1','Fold5.Rep1',
                          'Fold1.Rep2','Fold2.Rep2','Fold3.Rep2','Fold4.Rep2','Fold5.Rep2',
                          'Fold1.Rep3','Fold2.Rep3','Fold3.Rep3','Fold4.Rep3','Fold5.Rep3'),
                 AUC=c(0.7827979,0.7895625,0.7808797,0.7959311,0.7761844,
                       0.7884651,0.7931434,0.7847623,0.7824066,0.7800649,
                       0.7936632,0.7831582,0.7891104,0.7790694,0.7826860))

e4 <- data.frame(model = "ensemble_cv",
                 metalearner =  "deeplearning",
                 name = c('Fold1.Rep1','Fold2.Rep1','Fold3.Rep1','Fold4.Rep1','Fold5.Rep1',
                          'Fold1.Rep2','Fold2.Rep2','Fold3.Rep2','Fold4.Rep2','Fold5.Rep2',
                          'Fold1.Rep3','Fold2.Rep3','Fold3.Rep3','Fold4.Rep3','Fold5.Rep3'),
                 AUC=c(0.7833980,0.7898279,0.7799771,0.7969472,0.7764217,
                       0.7886016,0.7933652,0.7828642,0.7814056,0.7769415,
                       0.7941153,0.7846975,0.7879087,0.7783484,0.7831828))

ez <- rbind(e3,e4)

ez$metalearner <- as.factor(ez$metalearner)
ez$metalearner <- relevel(ez$metalearner,"nnls")

ez <- ez %>% separate(name,c("Fold","Rep")) %>%
  mutate(Fold=extract_numeric(Fold),
         Rep = extract_numeric(Rep))

f2 <- xyplot(AUC~metalearner|Rep,data=ez, group=Rep,
             cex=1.2, pch=16,
             between=list(x=0.5),
             xlab="",
             par.settings=list(strip.background=list(col="gray90")),
             scales=list(y=list(alternating=3, tck=c(1,1)),
                         x=list(tck=c(1,0), alternating=1)),
             ylim=c(0.75,0.80),
             panel = function(x,y,...){
               m <- tapply(y,x,mean)
               panel.segments(x0=c(0.7,1.7),y0=m,x1=c(1.3,2.3),y1=m,col='gray50',lwd=2)
               panel.superpose(x,y,...)
             }
)

pdf(file="C:/Git/useR16_ensemble/Example/ensemble_figure.pdf",width=9,height=3,useDingbats = F)
update(c(f1,f2,y.same=T,x.same=T,layout=c(5,1)),ylim=c(0.75,0.80))
dev.off()
