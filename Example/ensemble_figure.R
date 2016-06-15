e1 <- data.frame(model = "ensemble",
                 metalerner = c("nnls","deeplearning"),
                 rep = 1,
                 AUC=c(0.789151875671175,0.787556901593124))

e2 <- data.frame(model = "ensemble_cv",
                 metalerner = c("nnls","deeplearning"),
                 rep = 1:3,
                 AUC = c(0.7853754,0.7855145,0.7867766,
                         0.7843563,0.7853095,0.7865438))

e <- rbind(e1,e2)

e$metalerner <- as.factor(e$metalerner)
e$metalerner <- relevel(e$metalerner,"nnls")

pacman::p_load(lattice)

pdf("Example/ensemble_figure.pdf",width=8,height=4)
xyplot(AUC~metalerner|model,data=e, 
       cex=1.2, col='black', jitter.x=T,
       between=list(x=0.5),
       xlab="",
       par.settings=list(strip.background=list(col="gray90")),
       scales=list(y=list(alternating=3, tck=c(1,1)),
                   x=list(tck=c(1,0), alternating=1)),
       ylim=c(0.75,0.80),
       panel = function(x,y,...){
         m <- tapply(y,x,median)
         panel.segments(x0=c(0.7,1.7),y0=m,x1=c(1.3,2.3),y1=m,col='red',lwd=2)
         panel.xyplot(x,y,...)
       }
)
dev.off()