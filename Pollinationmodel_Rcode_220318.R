
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
#The goal of this model is to determine how quantified aspects of pollinator behavior cause RI in plants
# H is proportion of heterospecific pollen deposition (inverse of RI) (varies from 0-1)
# f is frequency of focal plant relative to plant it can hybridize with (varies from 0-1)
#two aspects of pollinator behavior: 
# k is constancy (varies from -1 to 1) 
# r is preference (varies from -1 to 1)

#single pollinator, two plants with ONE being focal plant:

# Functions ---------------------------------------------------------------

H<-function(f,r,k){
  -(-1+f)*(-1+k)*(-1+r)/(1+r*(2*f-1) + k*(2*f-1+r) )
}


#RI= 1-2(H/(H+(1-f)))
RI<-function(f,r,k){
  h<-(-(-1+f)*(-1+k)*(-1+r)/(1+r*(2*f-1) + k*(2*f-1+r)))

  ri<-(1-2*(h/(h+(1-f))))
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

RItot<-function(f,phi,r1,k1,r2,k2){
  psi<-function(fr,rho){(fr*(1+rho)/(1+(-1+2*fr)*rho))}
  
  v<-psi(f,r1)*phi/(psi(f,r1)*phi+(1-phi)*psi(f,r2))
  
  h1<-H(f,r1,k1)
  h2<-H(f,r2,k2)
  Ht<-(v*h1)+((1-v)*h2)
  c<-H(f,0,0)
  ri<-(1-(2*(Ht/(Ht(1-f)))))
  return (ri)
}


# Figure 1 ----------------------------------------------------------------

#figure 1 preference, constancy and both together in 6 blocks!#

#GOAL 1: One pollinator model PREFERENCE
#graph reproductive isolation across plant frequencies with no constancy (k=0) for preference from 0.01-100

#preference with one pollinator across plant frequency#
RI1<-function(x){RI(x,.8,0)}
RI2<-function(x){RI(x,.4,0)}
RI3<-function(x){RI(x,0,0)}
RI4<-function(x){RI(x,-0.8,0)}

H1<-function(x){H(x,.8,0)}
H2<-function(x){H(x,.4,0)}
H3<-function(x){H(x,0,0)}
H4<-function(x){H(x,-0.8,0)}
#GOAL 2: One pollinator model constancy
# Graph H by F with no preferenc (r=0.5) across constancy 
#Constancy with one pollinator across plant frequency#
RI5<-function(x){RI(x,0,.8)}
RI6<-function(x){RI(x,0,.4)}
RI7<-function(x){RI(x, 0,0)}
RI8<-function(x){RI(x, 0,-.8)}

H5<-function(x){H(x,0,.8)}
H6<-function(x){H(x,0,.4)}
H7<-function(x){H(x, 0,0)}
H8<-function(x){H(x, 0,-.8)}
#GOAL 3: One pollinator model both preference and constancy vary

RI9<-function(x){RI(x,0.8,.8)} #strong pref strong const
RI10<-function(x){RI(x,0.8,.4)} #weak pref strong const
RI11<-function(x){RI(x, 0.4,.8)} #strong pref weak const
RI12<-function(x){RI(x, 0.4,0.4)} #weak pref and strong const

H9<-function(x){H(x,0.8,.8)} #strong pref strong const
H10<-function(x){H(x,0.8,.4)} #weak pref strong const
H11<-function(x){H(x, 0.4,.8)} #strong pref weak const
H12<-function(x){H(x, 0.4,0.4)} #weak pref and strong const

### Graph!!!

quartz(width = 10, height = 10) #this opens a window so you get a good size
lay.mat <- matrix(1:6, nrow = 2) #making a matrix for the layout
layout(lay.mat)

#H preference
xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(H1, H2, H3, H4)
ltys <- c(1,1,3,2)
cols <- c("black", "gray65", "gray65", "black" )
plot(0,0, xlim = c(0,1), ylim = c(0,1), pch = NA, bty = "n", 
     xlab = expression(paste("Relative frequency focal plant (",italic("f"),")")), 
     ylab = "",
     mtext(side=2, "Heterospecific movement (H)", line=2.2, cex=1.4),
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
legend("bottomleft", legend = legend.txt, lty = ltys, col = cols, lwd =2.5, cex = 1.1, bty = "n", title = expression(bold("Preference")))
mtext("A.", side = 2, las = 2, adj = 3, padj = -15) #adj controls horizontal and padj controls vertical and basically you just have to fuss around with them until you are happy with the location

# RI pref
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
legend("bottomleft", legend = legend.txt, lty = ltys, col = cols, lwd =2.5, cex = 1.1, bty = "n", title = expression(bold("Preference")))
mtext("D.", side = 2, las = 2, adj = 3, padj = -15) #adj controls horizontal and padj controls vertical and basically you just have to fuss around with them until you are happy with the location



#H constancy
xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(H5, H6,H7, H8)
ltys <- c(1,1,3,2)
cols <- c("black", "gray65", "gray65", "black" )
plot(0,0, xlim = c(0,1), ylim = c(0,1), pch = NA, bty = "n", 
     xlab = expression(paste("Relative frequency focal plant (",italic("f"),")")), 
     ylab = "",
     mtext(side=2, "Heterospecific movement (H)", line=2.2, cex=1.4),
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
legend("bottomleft", legend = legend.txt, lty = ltys, col = cols, lwd =2, cex = 1.1, bty = "n", title =expression(bold("Constancy")))
mtext("B.", side = 2, las = 2, adj = 3, padj = -15) 

#RI constancy
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
legend("bottomleft", legend = legend.txt, lty = ltys, col = cols, lwd =2, cex = 1.1, bty = "n", title =expression(bold("Constancy")))
mtext("E.", side = 2, las = 2, adj = 3, padj = -15) 

#Hboth

xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(H9, H10, H11, H12)
ltys <- c(1,1,2,2)
cols <- c("black", "gray65", "black", "gray65" )
plot(0,0, xlim = c(0,1), ylim = c(0,1), pch = NA, bty = "n", 
     xlab = expression(paste("Relative frequency focal plant (",italic("f"),")")), 
     ylab = "",
     mtext(side=2, "Heterospecific movement (H)", line=2.2, cex=1.4),
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
legend("topright", legend = legend.txt, lty = ltys, col = cols, lwd =2, cex = 1.1, bty = "n", title = expression(bold("Preference and Constancy")))
mtext("C.", side = 2, las = 2, adj = 3, padj = -15) 

#RIboth
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
legend("bottomleft", legend = legend.txt, lty = ltys, col = cols, lwd =2, cex = 1.1, bty = "n", title = expression(bold("Preference and Constancy")))
mtext("F.", side = 2, las = 2, adj = 3, padj = -15) 



#RI


# Figure2 -----------------------------------------------------------------

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

#plants equal frequ (f=0.5), constancy=0
H13<-function(x){ Htot(0.5,x,0.8,0,0,0)} #high pref no pref
H14<-function(x){ Htot(0.5,x,0.8,0,-.8,0)} #high pref pref against
H16<-function(x){ Htot(0.5,x,0.4,0,-.8,0)} #low pref pref against 
H17<-function(x){ Htot(0.5,x,0.4,0,-.4,0)} #low pref low pref against

#pollinator with equal frequency constancy =0
H18<-function(x){ Htot(x,0.5,0.8,0,0,0)} #high pref no pref
H19<-function(x){ Htot(x,0.5,0.8,0,-0.8,0)} #high pref pref against
H21<-function(x){ Htot(x,0.5,0.4,0,-0.8,0)} #low pref strongpref against 
H22<-function(x){ Htot(x,0.5,0.4,0,-0.4,0)} #low pref low pref against

quartz(width = 9, height = 9) #this opens a window so you get a good size
lay.mat <- matrix(1:4, nrow = 2) #making a matrix for the layout
layout(lay.mat)
#H pol visits
xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(H13, H14,  H16, H17)
ltys <- c(1,2,2,1)
cols <- c("black", "black",  "gray65", "gray65")
plot(0,0, xlim = c(0,1), ylim = c(0,1), pch = NA, bty = "n",
     xlab = expression(paste("Proportion of visits by pollinator 1 (",phi,")")),  
     ylab = "",
     mtext(side=2, "Heterospecifics movement (H)", line=2.2, cex=1.4),
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
legend("bottomleft", legend = legend.txt, lty = ltys, col = cols, lwd =2, cex = 1,adj=.05, bty = "n", title = expression(bold("Preference")))
mtext("A.", side = 2, las = 2, adj = 4, padj = -14) 

#RI pol visits
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
legend("bottomleft", legend = legend.txt, lty = ltys, col = cols, lwd =2, cex = 1,adj=.05, bty = "n", title = expression(bold("Preference")))
mtext("C.", side = 2, las = 2, adj = 4, padj = -13) 



#H plant freq
xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(H18, H19,  H21, H22)
ltys <- c(1,2,2,1)
cols <- c("black", "black", "gray65", "gray65")
plot(0,0, xlim = c(0,1), ylim = c(0,1), pch = NA, bty = "n",
     xlab = expression(paste("Relative frequency of focal plant (",italic("f"),")")),  
     ylab = "",
     mtext(side=2, "Heterospecific movement (H)", line=2.2, cex=1.4),
     cex.lab=1.4, cex.axis=1.3)
for(f in 1:length(funcs)){
  func.f <- funcs[f][[1]]
  points(x = xs, y = func.f(xs), lty = ltys[f], col = cols[f], type = "l", lwd = 2)
}
legend("bottomleft", legend = legend.txt, lty = ltys, col = cols, lwd =2, cex = 1,adj=.05, bty = "n", title = expression(bold("Preference")))
mtext("B.", side = 2, las = 2, adj = 4, padj = -14) 

#RI plant freq
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
legend("bottomleft", legend = legend.txt, lty = ltys, col = cols, lwd =2, cex = 1,adj=.05, bty = "n", title = expression(bold("Preference")))
mtext("D.", side = 2, las = 2, adj = 4, padj = -13) 




# Figure3 -----------------------------------------------------------------

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

H23<-function(x){ Htot(0.5,x,0,.8,0,0)}
H24<-function(x){ Htot(0.5,x,0,.8,0,-.80)}
H25<-function(x){ Htot(0.5,x,0,.4,0,-.8)}
H26<-function(x){ Htot(0.5,x,0,.4,0,-.4)}

H27<-function(x){ Htot(x,0.5,0,.8,0,0)}
H28<-function(x){ Htot(x,0.5,0,.8,0,-.80)}
H29<-function(x){ Htot(x,0.5,0,.4,0,-.8)}
H30<-function(x){ Htot(x,0.5,0,.4,0,-.4)}

quartz(width = 10, height = 10) #this opens a window so you get a good size
lay.mat <- matrix(1:4, nrow = 2) #making a matrix for the layout
layout(lay.mat)

#Hpol
xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(H23, H24,  H25, H26)
ltys <- c(1,2,2,1)
cols <- c("black", "black",  "gray65", "gray65")
plot(0,0, xlim = c(0,1), ylim = c(0,1), pch = NA, bty = "n",
     xlab = expression(paste("Proportion of visits by pollinator 1 (",phi,")")),  
     ylab = "",
     mtext(side=2, "Heterospecific movement (H)", line=2.2, cex=1.4),
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
legend("bottomleft", legend = legend.txt, lty = ltys, col = cols, lwd =2, cex = 1., bty = "n", title = expression(bold("Constancy")))
mtext("A.", side = 2, las = 2, adj = 4, padj = -14) 

#RI pol
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
legend("bottomleft", legend = legend.txt, lty = ltys, col = cols, lwd =2, cex = 1., bty = "n", title = expression(bold("Constancy")))
mtext("C.", side = 2, las = 2, adj = 4, padj = -13) 

#H plant freq
xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(H27, H28, H29, H30)
ltys <- c(1,2,2,1)
cols <- c("black", "black",  "gray65", "gray65")
plot(0,0, xlim = c(0,1), ylim = c(0,1), pch = NA, bty = "n",
     xlab = expression(paste("Relative frequency of focal plant (",italic("f"),")")),  
     ylab = "",
     mtext(side=2, "Heterospecific movement (H)", line=2.2, cex=1.4),
     cex.lab=1.4, cex.axis=1.3)
for(f in 1:length(funcs)){
  func.f <- funcs[f][[1]]
  points(x = xs, y = func.f(xs), lty = ltys[f], col = cols[f], type = "l", lwd = 2)
}
legend("bottomleft", legend = legend.txt, lty = ltys, col = cols, lwd =2, cex = 1.,adj=.05, bty = "n", title = expression(bold("Constancy")))

mtext("B.", side = 2, las = 2, adj = 4, padj = -14) 

#RI plant freq
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
legend("bottomleft", legend = legend.txt, lty = ltys, col = cols, lwd =2, cex = 1.,adj=.05, bty = "n", title = expression(bold("Constancy")))

mtext("D.", side = 2, las = 2, adj = 4, padj = -13) 




####################################

# Example figure 4 --------------------------------------------------------


##################################
#Phlox example#

#RI<-function(f,,r1,k1)
#only battus from science table S6
RIDRb<-function(x){RI(x,0.65,0.36)}
RILBb<-function(x){RI(x,0.55,-0.15)}
HDRb<-function(x){H(x,0.65,0.36)}
HLBb<-function(x){H(x,0.55,-0.15)}


#*****************************
# Ipomopsis example from Aldridge and Campbell 2007 Table 1
#************************************

#RI with both HB and HM at GR site pollinator 1 is HB
RIag<-function(x){RItot(x,0.3, 0.81,0.03,-0.58,0.59)}
RIten<-function(x){RItot((1-x),0.3, -0.81,0.24,0.58,0.3)}
#RI with both HB and HM at PG site pollinator 1 is HB
RIag2<-function(x){RItot(x,0.6, 0.94,-0.22,0.04,-0.04)}
RIten2<-function(x){RItot((1-x),0.6, -0.94,0.85,0.04,-0.04)}
#H with both HB and HM at GR site pollinator 1 is HB
Hag<-function(x){Htot(x,0.3, 0.81,0.03,-0.58,0.59)}
Hten<-function(x){Htot((1-x),0.3, -0.81,0.24,0.58,0.3)}
#H with both HB and HM at PG site pollinator 1 is HB
Hag2<-function(x){Htot(x,0.6, 0.94,-0.22,0.04,-0.04)}
Hten2<-function(x){Htot((1-x),0.6, -0.94,0.85,0.04,-0.04)}

# RI across hummingbird visitation proportions!
RIagp<-function(x){RItot(0.5,x,0.81,0.03,-0.58,0.59)}
RItenp<-function(x){RItot(0.5,x,-0.81,0.24,0.58,0.3)}
#RI with both HB and HM at PG site pollinator 1 is HB
RIagp2<-function(x){RItot(0.5,x,0.94,-0.22,0.04,-0.04)}
RItenp2<-function(x){RItot(0.5,x,-0.94,0.85,0.04,-0.04)}

#H across hummingbird visitation proportions!
Hagp<-function(x){Htot(0.5,x,0.81,0.03,-0.58,0.59)}
Htenp<-function(x){Htot(0.5,x,-0.81,0.24,0.58,0.3)}
#H with both HB and HM at PG site pollinator 1 is HB
Hagp2<-function(x){Htot(0.5,x,0.94,-0.22,0.04,-0.04)}
Htenp2<-function(x){Htot(0.5,x,-0.94,0.85,0.04,-0.04)}

###Graph
quartz(width = 12, height = 12) #this opens a window so you get a good size
lay.mat <- matrix(1:6, nrow = 3) #making a matrix for the layout
layout(lay.mat)

#A#phlox H 
xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(HDRb, HLBb)
ltys <- c(1,1)
cols <- c("black", "gray","gray","black")
plot(0,0, xlim = c(0,1), ylim = c(0,1), pch = NA, bty = "n",
     xlab = expression(paste(italic("Phlox drummondii")," Relative Frequency")),  
     ylab = "",
     mtext(side=2, expression(paste("Heterospecific movement (H)")), line=2.2, cex=1.2),
     cex.lab=1.4, cex.axis=1.3
)
for(f in 1:length(funcs)){
  func.f <- funcs[f][[1]]
  points(x = xs, y = func.f(xs), lty = ltys[f], col = cols[f], type = "l", lwd = 2)
}

legend.txt <- c(expression(paste("Dark-Red, ",rho,"=0.65, ",kappa,"=0.36")),
                expression(paste("Light-Blue, ",rho,"=0.55, ",kappa,"=-0.15"))
)
legend("top", legend = legend.txt, lty = ltys, col = cols, lwd =2, cex = .9, bty = "n", title = expression(paste(bold(italic("Phlox ")),bold("Parameters"))))
mtext("A.", side = 2, las = 2, adj = 2.5, padj = -10, cex=1.2) 

#C#H ag freq
xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(Hag2, Hten2,Hag, Hten)

ltys <- c(1,1,2,2)
cols <- c("black", "gray","black", "gray")
plot(0,0, xlim = c(0,1), ylim = c(0,1), pch = NA, bty = "n",
     xlab = expression(paste(italic("I. aggregata "),"Relative Frequency")),  
     ylab = "",
     mtext(side=2, "Heterospecific movement (H)", line=2.2, cex=1.2),
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
legend("top",  legend = legend.txt, lty = ltys, col = cols, lwd =2, cex =0.9, bty = "n", 
       title = expression(paste(bold(italic("Ipomopsis ")),bold("Parameters"))))
mtext("C.", side = 2, las = 2, adj = 2.5, padj = -10, cex=1.2) 

#E# H hum freq 
xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(Hagp2, Htenp2,Hagp, Htenp)

ltys <- c(1,1,2,2)
cols <- c("black", "gray","black", "gray")
plot(0,0, xlim = c(0,1), ylim = c(0,1), pch = NA, bty = "n",
     xlab = expression(paste("Hummingbird Proportion of visits")),  
     ylab = "",
     mtext(side=2, "Heterospecific movement (H)", line=2.2, cex=1.2),
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
legend("top",  legend = legend.txt, lty = ltys, col = cols, lwd =2, cex =0.9, bty = "n", 
       title = expression(paste(bold(italic("Ipomopsis ")),bold("Parameters"))))
mtext("E.", side = 2, las = 2, adj = 2.5, padj = -10, cex=1.2) 




#B# Phlox RI
xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(RIDRb, RILBb)
ltys <- c(1,1)
cols <- c("black", "gray","gray","black")
plot(0,0, xlim = c(0,1), ylim = c(-1,1), pch = NA, bty = "n",
     xlab = expression(paste(italic("Phlox drummondii")," Relative Frequency")),  
     ylab = "",
     mtext(side=2, expression(paste("Reproductive Isolation (RI)")), line=2.2, cex=1.2),
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
mtext("B.", side = 2, las = 2, adj = 2.5, padj = -10, cex=1.2) 



#D# aggre freq RI
xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(RIag2, RIten2,RIag, RIten)

ltys <- c(1,1,2,2)
cols <- c("black", "gray","black", "gray")
plot(0,0, xlim = c(0,1), ylim = c(-1,1), pch = NA, bty = "n",
     xlab = expression(paste(italic("I. aggregata "),"Relative Frequency")),  
     ylab = "",
     mtext(side=2, "Reproductive Isolation (RI)", line=2.2, cex=1.2),
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
legend("bottom",  legend = legend.txt, lty = ltys, col = cols, lwd =2, cex =0.9, bty = "n", 
       title = expression(paste(bold(italic("Ipomopsis ")),bold("Parameters"))))
mtext("D.", side = 2, las = 2, adj = 2.5, padj = -10, cex=1.2) 


#F# hum freq RI
xs <- seq(1e-6,1, length.out = 1000)
funcs <- c(RIagp2, RItenp2,RIagp, RItenp)

ltys <- c(1,1,2,2)
cols <- c("black", "gray","black", "gray")
plot(0,0, xlim = c(0,1), ylim = c(-1,1), pch = NA, bty = "n",
     xlab = expression(paste("Hummingbird Proportion of visits")),  
     ylab = "",
     mtext(side=2, "Reproductive Isolation (RI)", line=2.2, cex=1.2),
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
legend("bottom",  legend = legend.txt, lty = ltys, col = cols, lwd =2, cex =0.9, bty = "n", 
       title = expression(paste(bold(italic("Ipomopsis ")),bold("Parameters"))))
mtext("F.", side = 2, las = 2, adj = 2.5, padj = -10, cex=1.2) 




#*********************************

# Frequency dependent -----------------------------------------------------


RIfreq<-function(f,r,k,b){
  psi<-(((-f*(1+r))/(r-1))^b)/(((1-f)^b)+(-(f*(1+r))/(r-1))^b)
  h<-(((k-1)*(psi-1))/(1-k+2*k*psi))
   ri<-(1-2*(h/(h+(1-f))))
    return (ri)
}


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
legend("bottomleft", legend = legend.txt, lty = ltys, col = cols, lwd =2.5, cex = 1.1, bty = "n", 
       title = expression(bold("Coefficent of\nfrequency-dependence")))
mtext("Appendix Figure C1", side = 2, las = 2, adj =.5, padj = -23) #adj controls horizontal and padj controls vertical and basically you just have to fuss around with them until you are happy with the location

