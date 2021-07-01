
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
#The goal of this model is to determine how quantified aspects of pollinator behavior cause RI in plants
# H is proportion of heterospecific pollen deposition (inverse of RI) (varies from 0-1)
# f is frequency of focal plant relative to plant it can hybridize with (varies from 0-1)
#two aspects of pollinator behavior: 
# k is constancy (varies from -1 to 1) 
# r is preference (varies from -1 to 1)

#single pollinator, two plants with ONE being focal plant:

H<-function(f,r,k){
  -(-1+f)*(-1+k)*(-1+r)/(1+r*(2*f-1) + k*(2*f-1+r) )
}


#RI = (1-2H)
RI<-function(f,r,k){
  h<-(-(-1+f)*(-1+k)*(-1+r)/(1+r*(2*f-1) + k*(2*f-1+r)))
  ri<-(1-2*h)
  return(ri)
}

#This model can be expanded to incoprorate 2 pollinators 
#k1 & r1 describe behavior of polinator 1 and k2 & r2 describe behavior of pollinator 2
#f is the frequency of focal plant
#phi is the proprotion of total visits made by pollinator 1 to all plants compared to the total visits made by all pollinators to all plants(pollinator frequency)
#v is the propotion of visits to focal plant made by pollinator1 (given all the visits to focal plant what proportion are from pollinator1)
Htot<-function(f,phi,r1,k1,r2,k2){
  psi<-function(fr,rho){(fr*(1+rho)/(1+(-1+2*fr)*rho))}
  v<-psi(f,r1)*phi/(psi(f,r1)*phi+(1-phi)*psi(f,r2))
                
  h1<-H(f,r1,k1)
  h2<-H(f,r2,k2)
  Ht<-(v*h1)+((1-v)*h2)
  return (Ht)
}


1-2*Htot(0.75,.5,0.7,0,0.5,0)
Htot(0.75,1,0.7,0,0.5,0)
RItot<-function(f,phi,r1,k1,r2,k2){
  psi<-function(fr,rho){(fr*(1+rho)/(1+(-1+2*fr)*rho))}
  
  v<-psi(f,r1)*phi/(psi(f,r1)*phi+(1-phi)*psi(f,r2))
  
  h1<-H(f,r1,k1)
  h2<-H(f,r2,k2)
  Ht<-(v*h1)+((1-v)*h2)
  ri<-(1-(2*Ht))
  return (ri)
}


#this can be frequency dependent for one pollinator:
Hfreq<-function(f,r,k,b){
  -(((1-f)^b * (-1+k))/((1-f)^b + (f*(-r/(-1+r)))^b + k* (-(1-f)^b+(f*(-r/(-1+r)))^b)))
}



#***************************************************************#
#figure 1 preference, constancy and both together in three blocks!#

#GOAL 1: One pollinator model PREFERENCE
#graph reproductive isolation across plant frequencies with no constancy (k=0) for preference from 0.01-100

#preference with one pollinator across plant frequency#
RI1<-function(x){RI(x,.8,0)}
RI2<-function(x){RI(x,.4,0)}
RI3<-function(x){RI(x,0,0)}
RI4<-function(x){RI(x,-0.8,0)}

#GOAL 2: One pollinator model constancy
# Graph H by F with no preferenc (r=0.5) across constancy 
#Constancy with one pollinator across plant frequency#
RI5<-function(x){RI(x,0,.8)}
RI6<-function(x){RI(x,0,.4)}
RI7<-function(x){RI(x, 0,0)}
RI8<-function(x){RI(x, 0,-.8)}

#GOAL 3: One pollinator model both preference and constancy vary

RI9<-function(x){RI(x,0.8,.8)} #strong pref strong const
RI10<-function(x){RI(x,0.8,.4)} #weak pref strong const
RI11<-function(x){RI(x, 0.4,.8)} #strong pref weak const
RI12<-function(x){RI(x, 0.4,0.4)} #weak pref and strong const

### Graph!!!

quartz(width = 10, height = 5) #this opens a window so you get a good size
lay.mat <- matrix(1:3, nrow = 1) #making a matrix for the layout
layout(lay.mat)
#if you want to see how this works, use
#layout.show(n = 2)
#lay.mat2 <- matrix(1:4, nrow = 2, byrow = TRUE)
#layout(lay.mat2)
#layout.show(n  = 4)

xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(RI1, RI2, RI3, RI4)
ltys <- c(1,1,3,2)
cols <- c("black", "gray65", "gray65", "black" )
plot(0,0, xlim = c(0,1), ylim = c(-1,1), pch = NA, bty = "n", 
     xlab = expression(paste("Relative frequency focal plant (",italic("f"),")")), 
     ylab = "",
     mtext(side=2, "Reproductive Isolation (RI)", line=2.2, cex=1.4),
     cex.lab=1.4, cex.axis=1.3)
for(f in 1:length(funcs)){
  func.f <- funcs[f][[1]]
  points(x = xs, y = func.f(xs), lty = ltys[f], col = cols[f], type = "l", lwd = 2)
}
legend.txt <- c(expression(paste(rho,"= 0.8")),
                expression(paste(rho,"= 0.4")),
                expression(paste(rho,"= 0")),
                expression(paste(rho,"= -0.8"))
)
legend("topleft", legend = legend.txt, lty = ltys, col = cols, lwd =2.5, cex = 1.1, bty = "n", title = expression(bold("Preference")))
mtext("A.", side = 2, las = 2, adj = 3, padj = -15) #adj controls horizontal and padj controls vertical and basically you just have to fuss around with them until you are happy with the location


xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(RI5, RI6, RI7, RI8)
ltys <- c(1,1,3,2)
cols <- c("black", "gray65", "gray65", "black" )
plot(0,0, xlim = c(0,1), ylim = c(-1,1), pch = NA, bty = "n", 
     xlab = expression(paste("Relative frequency focal plant (",italic("f"),")")), 
     ylab = "",
     mtext(side=2, "Reproductive Isolation (RI)", line=2.2, cex=1.4),
     cex.lab=1.4, cex.axis=1.3)
for(f in 1:length(funcs)){
  func.f <- funcs[f][[1]]
  points(x = xs, y = func.f(xs), lty = ltys[f], col = cols[f], type = "l", lwd = 2)
}
legend.txt <- c(expression(paste(kappa,"= 0.8")),
                expression(paste(kappa,"= 0.4")),
                expression(paste(kappa,"= 0")),
                expression(paste(kappa,"= -0.8"))
)
legend("topleft", legend = legend.txt, lty = ltys, col = cols, lwd =2, cex = 1.1, bty = "n", title =expression(bold("Constancy")))
mtext("B.", side = 2, las = 2, adj = 3, padj = -15) 

xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(RI9, RI10, RI11, RI12)
ltys <- c(1,1,2,2)
cols <- c("black", "gray65", "black", "gray65" )
plot(0,0, xlim = c(0,1), ylim = c(-1,1), pch = NA, bty = "n", 
     xlab = expression(paste("Relative frequency focal plant (",italic("f"),")")), 
     ylab = "",
     mtext(side=2, "Reproductive Isolation (RI)", line=2.2, cex=1.4),
     cex.lab=1.4, cex.axis=1.3)
for(f in 1:length(funcs)){
  func.f <- funcs[f][[1]]
  points(x = xs, y = func.f(xs), lty = ltys[f], col = cols[f], type = "l", lwd = 2)
}
legend.txt <-  c(expression(paste(rho,"= 0.8, ",kappa,"= 0.8")),
                 expression(paste(rho,"= 0.4, ",kappa,"= 0.8")),
                 expression(paste(rho,"= 0.8, ",kappa,"= 0.4")),
                 expression(paste(rho,"= 0.4, ",kappa,"= 0.4"))
)
legend("bottomright", legend = legend.txt, lty = ltys, col = cols, lwd =2, cex = 1.1, bty = "n", title = expression(bold("Preference and Constancy")))
mtext("C.", side = 2, las = 2, adj = 3, padj = -15) 



#RI


#***************************************************************#
#preference with two pollinators- plant frequency and pollinator frequency
#***************************************************************#
#RI<-function(f,t,r1,k1,r2,k2)
#preference with two pollinators across pollinator frequency#
#plants equal frequ (f=0.5), constancy=0
RI13<-function(x){ RItot(0.5,x,0.8,0,0,0)} #high pref no pref
RI14<-function(x){ RItot(0.5,x,0.8,0,-.8,0)} #high pref pref against
RI16<-function(x){ RItot(0.5,x,0.4,0,-.8,0)} #low pref pref against 
RI17<-function(x){ RItot(0.5,x,0.4,0,-.4,0)} #low pref low pref against

#pollinator with equal frequency constancy =0
RI18<-function(x){ RItot(x,0.5,0.8,0,0,0)} #high pref no pref
RI19<-function(x){ RItot(x,0.5,0.8,0,-0.8,0)} #high pref pref against
RI21<-function(x){ RItot(x,0.5,0.4,0,-0.8,0)} #low pref strongpref against 
RI22<-function(x){ RItot(x,0.5,0.4,0,-0.4,0)} #low pref low pref against

RItot(0.5,0.5,0.4,0,-0.4,0)

quartz(width = 9, height = 5) #this opens a window so you get a good size
lay.mat <- matrix(1:2, nrow = 1) #making a matrix for the layout
layout(lay.mat)

xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(RI13, RI14,  RI16, RI17)
ltys <- c(1,2,2,1)
cols <- c("black", "black",  "gray65", "gray65")
plot(0,0, xlim = c(0,1), ylim = c(-1,1), pch = NA, bty = "n",
     xlab = expression(paste("Proportion of visits by pollinator 1 (",phi,")")),  
     ylab = "",
     mtext(side=2, "Reproductive Isolation (RI)", line=2.2, cex=1.4),
     cex.lab=1.4, cex.axis=1.3)
for(f in 1:length(funcs)){
  func.f <- funcs[f][[1]]
  points(x = xs, y = func.f(xs), lty = ltys[f], col = cols[f], type = "l", lwd = 2)
}

legend.txt <- c(expression(paste(rho,"1= 0.8, ",rho,"2= 0")),
                expression(paste(rho,"1= 0.8, ",rho,"2= -0.8")),
                expression(paste(rho,"1= 0.4, ",rho,"2= -0.8")),
                expression(paste(rho,"1= 0.4, ",rho,"2= -0.4"))
)
legend("bottomright", legend = legend.txt, lty = ltys, col = cols, lwd =2, cex = 1,adj=.05, bty = "n", title = expression(bold("Preference")))
mtext("A.", side = 2, las = 2, adj = 4, padj = -13) 

xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(RI18, RI19,  RI21, RI22)
ltys <- c(1,2,2,1)
cols <- c("black", "black", "gray65", "gray65")
plot(0,0, xlim = c(0,1), ylim = c(-1,1), pch = NA, bty = "n",
     xlab = expression(paste("Relative frequency of focal plant (",italic("f"),")")),  
     ylab = "",
     mtext(side=2, "Reproductive Isolation (RI)", line=2.2, cex=1.4),
     cex.lab=1.4, cex.axis=1.3)
for(f in 1:length(funcs)){
  func.f <- funcs[f][[1]]
  points(x = xs, y = func.f(xs), lty = ltys[f], col = cols[f], type = "l", lwd = 2)
}
legend("bottomright", legend = legend.txt, lty = ltys, col = cols, lwd =2, cex = 1,adj=.05, bty = "n", title = expression(bold("Preference")))
mtext("B.", side = 2, las = 2, adj = 4, padj = -13) 




#***************************************************************#
#constancy with two pollinators- plant frequency and pollinator frequency
#***************************************************************#

#constancy with two pollinators across plant frequency#

RI23<-function(x){ RItot(0.5,x,0,.8,0,0)}
RI24<-function(x){ RItot(0.5,x,0,.8,0,-.80)}
RI25<-function(x){ RItot(0.5,x,0,.4,0,-.8)}
RI26<-function(x){ RItot(0.5,x,0,.4,0,-.4)}

RI27<-function(x){ RItot(x,0.5,0,.8,0,0)}
RI28<-function(x){ RItot(x,0.5,0,.8,0,-.80)}
RI29<-function(x){ RItot(x,0.5,0,.4,0,-.8)}
RI30<-function(x){ RItot(x,0.5,0,.4,0,-.4)}

quartz(width = 9, height = 5) #this opens a window so you get a good size
lay.mat <- matrix(1:2, nrow = 1) #making a matrix for the layout
layout(lay.mat)

xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(RI23, RI24,  RI25, RI26)
ltys <- c(1,2,2,1)
cols <- c("black", "black",  "gray65", "gray65")
plot(0,0, xlim = c(0,1), ylim = c(-1,1), pch = NA, bty = "n",
     xlab = expression(paste("Proportion of visits by pollinator 1 (",phi,")")),  
     ylab = "",
     mtext(side=2, "Reproductive Isolation (RI)", line=2.2, cex=1.4),
     cex.lab=1.4, cex.axis=1.3)
for(f in 1:length(funcs)){
  func.f <- funcs[f][[1]]
  points(x = xs, y = func.f(xs), lty = ltys[f], col = cols[f], type = "l", lwd = 2)
}

legend.txt <- c(expression(paste(kappa,"1= 0.8, ",kappa,"2= 0")),
                expression(paste(kappa,"1= 0.8, ",kappa,"2= -0.8")),
                expression(paste(kappa,"1= 0.4, ",kappa,"2= -0.8")),
                expression(paste(kappa,"1= 0.4, ",kappa,"2= -0.4"))
)
legend("bottomright", legend = legend.txt, lty = ltys, col = cols, lwd =2, cex = 1., bty = "n", title = expression(bold("Constancy")))
mtext("A.", side = 2, las = 2, adj = 4, padj = -13) 

xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(RI27, RI28, RI29, RI30)
ltys <- c(1,2,2,1)
cols <- c("black", "black",  "gray65", "gray65")
plot(0,0, xlim = c(0,1), ylim = c(-1,1), pch = NA, bty = "n",
     xlab = expression(paste("Relative frequency of focal plant (",italic("f"),")")),  
     ylab = "",
     mtext(side=2, "Reproductive Isolation (RI)", line=2.2, cex=1.4),
     cex.lab=1.4, cex.axis=1.3)
for(f in 1:length(funcs)){
  func.f <- funcs[f][[1]]
  points(x = xs, y = func.f(xs), lty = ltys[f], col = cols[f], type = "l", lwd = 2)
}
legend("bottomright", legend = legend.txt, lty = ltys, col = cols, lwd =2, cex = 1.,adj=.05, bty = "n", title = expression(bold("Constancy")))

mtext("B.", side = 2, las = 2, adj = 4, padj = -13) 





####################################

##################################
#Phlox example#

#RI<-function(f,,r1,k1)
#only battus from science table S6
RIDRb<-function(x){RI(x,0.65,0.36)}
RILBb<-function(x){RI(x,0.55,-0.15)}
#just LB and DR
quartz() #this opens a window so you get a good size


#just battus

xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(RIDRb, RILBb)
ltys <- c(1,1)
cols <- c("black", "gray","gray","black")
plot(0,0, xlim = c(0,1), ylim = c(-1,1), pch = NA, bty = "n",
     xlab = expression(paste(italic("Phlox drummondii")," Relative Frequency")),  
     ylab = "",
     mtext(side=2, expression(paste("Reproductive Isolation from ",italic("Phlox cuspidata"))), line=2.2, cex=1.4),
     cex.lab=1.4, cex.axis=1.3
)
for(f in 1:length(funcs)){
  func.f <- funcs[f][[1]]
  points(x = xs, y = func.f(xs), lty = ltys[f], col = cols[f], type = "l", lwd = 2)
}

legend.txt <- c(expression(paste("Dark-Red, ",rho,"=0.65, ",kappa,"=0.36")),
                expression(paste("Light-Blue, ",rho,"=0.55, ",kappa,"=-0.15"))
)
legend("bottom", legend = legend.txt, lty = ltys, col = cols, lwd =2, cex = .9, bty = "n", title = expression(paste(bold(italic("Phlox ")),bold("Parameters"))))
mtext("A.", side = 2, las = 2, adj = 4, padj = -16, cex=1.3) 

#*****************************
# Ipomopsis example from Aldridge and Campbell 2007 Table 1
#************************************
#RItot<-function(f,t,r1,k1,r2,k2)
#RI with both HB and HM at GR site pollinator 1 is HB

RIag<-function(x){RItot(x,0.3, 0.81,0.03,-0.58,0.59)}
RIten<-function(x){RItot((1-x),0.3, -0.81,0.24,0.58,0.3)}
#RI with both HB and HM at PG site pollinator 1 is HB
RIag2<-function(x){RItot(x,0.6, 0.94,-0.22,0.04,-0.04)}
RIten2<-function(x){RItot((1-x),0.6, -0.94,0.85,0.04,-0.04)}

RIagtot<-function(x){RItot(x,0.43,0.86,0.04,-0.44,0.44)}
RItentot<-function(x){RItot((1-x),0.43, -0.86,0.47,0.44,0.25)}
quartz()
xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(RIag2, RIten2,RIag, RIten)

ltys <- c(1,1,2,2)
cols <- c("black", "gray","black", "gray")
plot(0,0, xlim = c(0,1), ylim = c(-1,1), pch = NA, bty = "n",
     xlab = expression(paste(italic("I. aggregata "),"Relative Frequency")),  
     ylab = "",
     mtext(side=2, "Reproductive Isolation (RI)", line=2.2, cex=1.4),
     cex.lab=1.4, cex.axis=1.3
)
for(f in 1:length(funcs)){
  func.f <- funcs[f][[1]]
  points(x = xs, y = func.f(xs), lty = ltys[f], col = cols[f], type = "l", lwd = 2)
}

legend.txt <- c(
                expression(paste(italic("I. ag"), " at PG, Bird: ",rho,"=0.94, ",kappa,"=-0.22, Moth: ",rho, "=-0.04, ",kappa,"=0.04" )),
                expression(paste(italic("I. ten"), " at PG,  Bird: ",rho,"=-0.94, ",kappa,"=0.85, Moth: ",rho, "=0.04, ",kappa,"=-0.04")),
                expression(paste(italic("I. ag"), " at GR, Bird: ",rho,"=0.81, ",kappa,"=0.03, Moth: ",rho, "=-0.58, ",kappa,"=0.59" )),
                expression(paste(italic("I. ten"), " at GR,  Bird: ",rho,"=-0.81, ",kappa,"=0.24, Moth: ",rho, "=0.58, ",kappa,"=0.3"))
)
legend(x=0.04,y=-.6,  legend = legend.txt, lty = ltys, col = cols, lwd =2, cex =0.9, bty = "n", 
       title = expression(paste(bold(italic("Ipomopsis ")),bold("Parameters"))))
mtext("B.", side = 2, las = 2, adj = 4, padj = -16, cex=1.3) 

#graph across hummingbird visitation proportions!
quartz()
RIagp<-function(x){RItot(0.5,x,0.81,0.03,-0.58,0.59)}
RItenp<-function(x){RItot(0.5,x,-0.81,0.24,0.58,0.3)}
#RI with both HB and HM at PG site pollinator 1 is HB
RIagp2<-function(x){RItot(0.5,x,0.94,-0.22,0.04,-0.04)}
RItenp2<-function(x){RItot(0.5,x,-0.94,0.85,0.04,-0.04)}
quartz()
xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(RIagp2, RItenp2,RIagp, RItenp)

ltys <- c(1,1,2,2)
cols <- c("black", "gray","black", "gray")
plot(0,0, xlim = c(0,1), ylim = c(-1,1), pch = NA, bty = "n",
     xlab = expression(paste("Hummingbird Proportion of visits")),  
     ylab = "",
     mtext(side=2, "Reproductive Isolation (RI)", line=2.2, cex=1.4),
     cex.lab=1.4, cex.axis=1.3
)
for(f in 1:length(funcs)){
  func.f <- funcs[f][[1]]
  points(x = xs, y = func.f(xs), lty = ltys[f], col = cols[f], type = "l", lwd = 2)
}

legend.txt <- c(
  expression(paste(italic("I. ag"), " at PG, Bird: ",rho,"=0.94, ",kappa,"=-0.22, Moth: ",rho, "=-0.04, ",kappa,"=0.04" )),
  expression(paste(italic("I. ten"), " at PG,  Bird: ",rho,"=-0.94, ",kappa,"=0.85, Moth: ",rho, "=0.04, ",kappa,"=-0.04")),
  expression(paste(italic("I. ag"), " at GR, Bird: ",rho,"=0.81, ",kappa,"=0.03, Moth: ",rho, "=-0.58, ",kappa,"=0.59" )),
  expression(paste(italic("I. ten"), " at GR,  Bird: ",rho,"=-0.81, ",kappa,"=0.24, Moth: ",rho, "=0.58, ",kappa,"=0.3"))
)
legend(x=0.04,y=-.6,  legend = legend.txt, lty = ltys, col = cols, lwd =2, cex =0.9, bty = "n", 
       title = expression(paste(bold(italic("Ipomopsis ")),bold("Parameters"))))
mtext("C.", side = 2, las = 2, adj = 3.5, padj = -16, cex=1.3) 




#*********************************

# FREQUENCY DEPENDENT SELECTION

RIfreq<-function(f,r,k,b){
  psi<-(((-f*(1+r))/(r-1))^b)/(((1-f)^b)+(-(f*(1+r))/(r-1))^b)
  ri<-1-((2*(k-1)*(psi-1))/(1-k+2*k*psi))
    return (ri)
}
RIfreq(0.4,.75,0,2)
RI(0.4,0.75,0)
quartz()
RIfreq1<-function(x){RIfreq(x,.5,0,1)}
RIfreq2<-function(x){RIfreq(x,.5,0,2)}
RIfreq3<-function(x){RIfreq(x,.5,0,10)}
RIfreq4<-function(x){RIfreq(x,.5,0,.2)}

xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(RIfreq1, RIfreq2, RIfreq3, RIfreq4)
ltys <- c(1,1,2,2)
cols <- c("black", "gray65", "gray65", "black" )
plot(0,0, xlim = c(0,1), ylim = c(-1,1), pch = NA, bty = "n", 
     xlab = expression(paste("Relative frequency focal plant (",italic("f"),")")), 
     ylab = "",
     mtext(side=2, "Reproductive Isolation (RI)", line=2.2, cex=1.4),
     cex.lab=1.4, cex.axis=1.3)
for(f in 1:length(funcs)){
  func.f <- funcs[f][[1]]
  points(x = xs, y = func.f(xs), lty = ltys[f], col = cols[f], type = "l", lwd = 2)
}
legend.txt <- c(expression(paste("b = 1")),
                expression(paste("b = 2")),
                expression(paste("b = 10")),
                expression(paste("b = 0.2"))
)
legend("bottomright", legend = legend.txt, lty = ltys, col = cols, lwd =2.5, cex = 1.1, bty = "n", 
       title = expression(bold("Coefficent of\nfrequency-dependence")))
mtext("Appendix Figure C1", side = 2, las = 2, adj =.5, padj = -23) #adj controls horizontal and padj controls vertical and basically you just have to fuss around with them until you are happy with the location

#### RI for both plants in one community

RIboth<-function(f,r,kt){
  psi<-((f*(1+r))/((1+r*(2*f-1))))
  ht<-(2*(kt-1)*(psi^2-psi))/(1+kt-4*kt*psi+4*kt*psi^2)
  rit<-1-2*ht
  return(rit)
}
RIboth(0.3,0.95,-0.08)

RIagb<-function(x){RI(x,0.94,-0.22)}
RItenb<-function(x){RI((1-x),-0.94,0.85)}
RIb<-function(x){RIboth(x,-0.94,-0.08)}
quartz()
xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(RIagb, RItenb, RIb)
ltys <- c(1,1,2)
cols <- c("black", "gray65", "black" )
plot(0,0, xlim = c(0,1), ylim = c(-1,1), pch = NA, bty = "n", 
     xlab = expression(paste("Relative frequency of ",italic("I. aggregata"))), 
     ylab = "",
     mtext(side=2, "Reproductive Isolation (RI)", line=2.2, cex=1.4),
     cex.lab=1.4, cex.axis=1.3)
for(f in 1:length(funcs)){
  func.f <- funcs[f][[1]]
  points(x = xs, y = func.f(xs), lty = ltys[f], col = cols[f], type = "l", lwd = 2)
}
legend.txt <- c(
  expression(paste(italic("I. aggregata: "), ,rho,"=0.94, ",kappa,"=-0.22" )),
  expression(paste(italic("I. tenuituba "),rho,"=-0.94, ",kappa,"=0.85")),
  expression(paste("Both ", rho,"=0.94, ",kappa,"t=-0.08" ))
  
)
legend(x=0.04,y=-.6,  legend = legend.txt, lty = ltys, col = cols, lwd =2, cex =0.9, bty = "n", 
       title = expression(paste(bold(italic("Ipomopsis ")),bold("Parameters"))))
mtext("Appendix Figure B1", side = 2, las = 2, adj = 0.3, padj =-18, cex=1.3) 
