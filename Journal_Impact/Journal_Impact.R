#############################################################################
# Comparing Influence of Ecology Journals
# Daniel J. Hocking
# Start: 05 April 2013
#############################################################################

par (mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01, mfrow=c(1,1))

# 2011 Impact Factor, Citations, Eigenfactor, & Article Influence retrieved from Journal Citation Reports on ISI Web of Knowledge on 04 April 2013

# 2007-2011 h, g, hc, hi indices retrieved using Publish or Perish software on 06 April 2013: Harzing, A.W. (2007) Publish or Perish, available from http://www.harzing.com/pop.htm 

# Metrics from PoP limited to top 1000 papers in each journal

Data <- read.table('Importance_2007-2011.csv', header=TRUE, sep=',')

summary(Data)



#------------------Data Exploration------------------
source('/Users/Dan/Documents/Statistics/R/Functions/panelcor.R')
source('/Users/Dan/Documents/Statistics/R/Functions/panelcor_spear.R')

Data1 <- na.omit(Data) # remove rows with incomplete data

Data1 <- Data1[which(Data1$Papers >= 50), ] # remove data with fewer than 50 google scholar results (fewer than 50 papers found from 2007-2011)

summary(Data1)

Means <- apply(X=as.matrix(Data1[,4:32]), MARGIN=2, FUN=mean, na.rm=TRUE)
apply(X=as.matrix(Data1[,4:32]), MARGIN=2, FUN=sd, na.rm=TRUE)

# Scatterplot matrix and pearson correlations
Pairs <- Data1[,c("JIF", "JIF5", "Eigenfactor", "AI", "h_index", "AR_index", "g_index", "e_index", "hm_index", "Cites_Paper_Google", "Cites_2011_JCR", "Citations_Google")]
pairs(Pairs, upper.panel=panel.smooth, lower.panel=panel.cor.spear)

Corr.Pearson <- cor(Pairs, method="pearson") # assumes linear relationships
Corr.Spearman <- cor(Pairs, method="spearman") # assumes monotonic but not necessarily linear relationships

Corr.Spearman - Corr.Pearson # greater than 0 indicates monotonic but not linear

## put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="gray", ...)
}

panel.cor2 <- function(x,y, digits=2, prefix="", cex.cor){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  spear <- cor(x, y, method="spearman")
  pears <- cor(x, y, method="pearson")
  lm1 <- lm(y ~ x)
  nlm1 <- lm(log(y) ~ log(x))
  dAIC <- AIC(lm1) - AIC(nlm1)
  Diff <- spear - pears
  if(dAIC <= 2) {
    Cor.Type = "p"
    r = pears
  }
  else {
    r = spear
    Cor.Type = "s"
  }
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  #if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
  text(0.5, 0.5, txt, cex = 1.5)
  text(0.9, 0.9, Cor.Type, cex = 1)
}

panel.diff <- function(x,y, digits=2, prefix="", cex.cor){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Diff <- max(abs(x - y))
  txt <- format(c(Diff, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  #if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
  text(0.5, 0.5, txt, cex = 1)
}


Pearson.Pairs <- Data1[,c("JIF", "JIF5", "AI", "SNIP", "SJR", "hn_index")]
pairs(Pearson.Pairs, upper.panel=panel.smooth, lower.panel=panel.cor2, diag.panel=panel.hist)

Pairs2 <- Data1[,c("JIF", "JIF5", "AI", "SNIP", "SJR", "Eigenfactor", "h_index", "hc_index", "g_index", "e_index", "AR_index")]
pairs(Pairs2, upper.panel=panel.smooth, lower.panel=panel.cor2, diag.panel=panel.hist, cex.labels = c(cex=1), labels=c("JIF", "JIF5", "AI", "SNIP", "SJR", "Eigenfactor", "h-index", "hc-index", "g-index", "e-index", "AR-index"))

Pairs3 <- Data1[,c("JIF", "JIF5", "AI", "SNIP", "SJR", "Eigenfactor", "h_index", "hc_index", "g_index", "e_index", "AR_index", "Cites_2011_JCR", "Citations_Google")]
pairs(Pairs3, upper.panel=panel.smooth, lower.panel=panel.cor2, diag.panel=panel.hist)

for(i in 1:length(Pairs2)){
  for(j in 1:length(Pairs2)){
    diff[i,j] <- Pairs2[i,j] - Pairs2[i,j]
  }
}

PairsRank <- Data1[,c("JIF_Rank", "JIF5_Rank", "AI_Rank", "SNIP_Rank", "SJR_Rank", "EF_Rank", "H_Rank", "Hc_Rank", "g_Rank", "e_Rank", "AR_Rank")]

#pdf("Scatterplot-Matrix.pdf", )
pairs(Pairs2, upper.panel=panel.smooth, lower.panel=panel.cor.spear, diag.panel=panel.hist)

pairs(PairsRank, upper.panel=panel.diff, lower.panel=panel.cor.spear)


# Histogram of journals AI
hist(Data1$AI, breaks=10, freq=FALSE)

# ------------Examine relationship of EF and JIF more closely---------
plot(Data1$Eigenfactor, Data1$JIF5)
lines(smooth.spline(Data1$Eigenfactor, Data1$JIF5), col='red')

# log-log Linear regression of EF on JIF
lm.JIF.EF <- lm(log(Data1$JIF5) ~ log(Data1$Eigenfactor))
plot(lm.JIF.EF)
summary(lm.JIF.EF)
R2.JIF.EF <- summary(lm.JIF.EF)$r.squared

# Plot log-log EF & JIF
plot(log(Data1$Eigenfactor), log(Data1$JIF5), xlab=expression(paste(log[e], " ", Eigenfactor^TM)), ylab=expression(paste(log[e], " Journal Impact Factor (5-yr)")))
lines(log(Data1$Eigenfactor), fitted(lm.JIF.EF), col='red')
text(-7.5, 2.1, labels=expression(paste(R^2==0.6404)))
text(-7.5, 2.6, labels=expression(paste(log[e](JIF)==3.26 + 0.471%*%log[e](EF))))

# Models and tests of relationships
source('/Users/Dan/Documents/Statistics/R/Functions/Xiao_Power_Function.R') # Reference: Xiao et al. 2011 Ecology

# JIF5 as function of EF
(PA_EF_JIF5 <- power_analysis(x=Data1$Eigenfactor, y=Data1$JIF5, output_plot=TRUE)) # Error appears multiplicative (log regresion better) # a=29.34, b=0.498

Data1$y.JIF5 <- 29.34*Data1$Eigenfactor^0.498 # calculate predicted values
dev.off()
par (mfrow=c(1,1), mar=c(4,3,2,1), mgp=c(2,.7,0), tck=-.01)
Data1 <- Data1[order(Data1$Eigenfactor), ]
plot(Data1$Eigenfactor, Data1$JIF5, xlab='Eigenfactor', ylab='Journal impact factor (5-yr)')
lines(Data1$Eigenfactor, Data1$y.JIF5)
text(0.088, y=17, labels=expression(R^2==0.6387))

summary(lm.JIF.EF)
(R2 <- summary(lm.JIF.EF)$r.squared) # 0.6387
(R2adj <- summary(lm.JIF.EF)$adj.r.squared) # 0.6356

lm.JIF.EF <- lm(JIF5 ~ Eigenfactor, Data1)
llm.JIF5.EF <- lm(JIF5 ~ log(Eigenfactor), Data1)
lm.JIF.EF2 <- lm(JIF5 ~ Eigenfactor + I(Eigenfactor^2), Data1)
lm.JIF.EF3 <- lm(JIF5 ~ Eigenfactor + I(Eigenfactor^2) + I(Eigenfactor^3), Data1)
lm.JIF.EF4 <- lm(JIF5 ~ Eigenfactor + I(Eigenfactor^2) + I(Eigenfactor^3) + I(Eigenfactor^4), Data1)
lm.JIF.EF5 <- lm(JIF5 ~ Eigenfactor + I(Eigenfactor^2) + I(Eigenfactor^3) + I(Eigenfactor^4) + I(Eigenfactor^5), Data1)

AIC(lm.JIF.EF)
AIC(llm.JIF5.EF)
AIC(lm.JIF.EF2)
AIC(lm.JIF.EF3)
AIC(lm.JIF.EF4)
AIC(lm.JIF.EF5)

summary(lm.JIF.EF2)

plot(log(Data1$Eigenfactor), Data1$JIF5)
plot(log(Data1$Eigenfactor), log(Data1$JIF5))

# Function: Ramsey Regression Equation Specification Error Test (RESET) test (Ramsey, 1969) to test for linearity. If any higher order coefficients are significant then the data support a nonlinear fit. 
# Not sure if this is the corrent method for the RESET test
reset.lm <- function(fit){
  N.var <- dim(summary(fit)$coefficients)[1] - 1
  P.min <- min(summary(fit)$coefficients[-c(1,2), 4])
  return(P.min)
} 

reset.lm(fit2)
reset.lm(fit3)
reset.lm(fit4)
reset.lm(fit5)

# Try reset test in the lmtest package
library(lmtest)
data(Mandible)
mandible <- log(Mandible)
resettest(length ~ age, power = 2, type = "regressor", data = mandible)
resettest(length ~ age, power = 3, type = "regressor", data = mandible)
xx <- c(1:30)
yy1 <- 1 + xx + xx^2 + rnorm(30)
yy2 <- 1 + xx + rnorm(30)
resettest(yy1 ~ xx , power=2, type="regressor")
resettest(yy2 ~ xx , power=2, type="regressor")

resettest(fit1, power = 2, type = "regressor") # provides same answer as mine
resettest(fit1, power = 3, type = "regressor") # different answer than mine
resettest(fit1, power = 4, type = "regressor")
resettest(fit1, power = 5, type = "regressor")
resettest(fit1, power = 15, type = "regressor")
resettest(fit1, power = 200, type = "regressor")

# AIC comparison to test if linear model is the best
aic.lm <- function(x, y){
  fit1 <- lm(y ~ x)
  fitlog <- lm(y ~ log(x))
  fitexp <- lm(y ~ exp(x))
  fit2 <- lm(y ~ x + I(x^2))
  fit3 <- lm(y ~ x + I(x^2) + I(x^3))
  fit4 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4))
  fit5 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5))
  
  AICs <- AIC(fitlog, fitexp, fit2, fit3, fit4, fit5)
  
  dAIC <- AIC(fit1) - AICs
  
  MaxDeltaAIC <- max(dAIC[,2]) # if greater than 2, then simple linear model not the best
  
  return(MaxDeltaAIC)
} # interestingly it always seems to choose models with some wiggle

aic.lm(Data1$AI, Data1$JIF5)
aic.lm(Data1$JIF, Data1$JIF5)
aic.lm(Data1$Eigenfactor, Data1$JIF5)

reset.lm()

# JIF as function of H-index
plot(Data1$h_index, Data1$JIF5) # model as linear

lm.JIF.h <- lm(JIF5 ~ h_index, data=Data1)
plot(lm.JIF.h) # poor fit
lm.logJIF.h <- lm(log(JIF5) ~ h_index, data=Data1)
plot(lm.logJIF.h) # still poor fit
lm.logJIF.logh <- lm(log(JIF5) ~ log(h_index), data=Data1)
plot(lm.logJIF.logh) # better residuals but poor normality
summary(lm.logJIF.logh)

(PA_H_JIF5 <- power_analysis(x=Data1$h_index, y=Data1$JIF5, output_plot=TRUE)) # Error appears multiplicative (log regresion better) # a=0.0515, b=1.1405

dev.off()
Data1$y.JIF5.h <- 0.0514*Data1$h_index^1.1405 # calculate predicted values
par (mfrow=c(1,1), mar=c(4,3,2,1), mgp=c(2,.7,0), tck=-.01)
Data1 <- Data1[order(Data1$h_index), ]
plot(Data1$h_index, Data1$JIF5, xlab='H-index', ylab='Journal impact factor (5-yr)')
lines(Data1$h_index, Data1$y.JIF5.h)
text(10, y=17, labels=expression(R^2==0.758))

summary(lm.JIF.EF)
(R2 <- summary(lm.JIF.EF)$r.squared) # 0.6387
(R2adj <- summary(lm.JIF.EF)$adj.r.squared) # 0.6356


# Relationship between H and EF
plot(Data1$h_index, Data1$Eigenfactor)

lm(log(Eigenfactor) ~ h_index, data=Data1)

(PA_H_EF <- power_analysis(x=Data1$h_index, y=Data1$Eigenfactor, output_plot=TRUE)) # Error appears multiplicative (log regresion better) # a=0.0515, b=1.1405

# Relationship between AI and JIF
lm.JIF.AI <- lm(JIF5 ~ AI, Data1)
summary(lm.JIF.AI) # slope = 1.888, intercept = 0.470
plot(Data1$AI, Data1$JIF5, xlab='Article Influence', ylab='Journal Impact Factor (5 yr)')
abline(lm.JIF.AI)
text(x=6, y=17, labels=expression(slope==2.048))
text(x=6, y=15.8, labels=expression(R^2==0.9763))

llm.JIF5.AI <- lm(JIF5 ~ log(AI), Data1)

AIC(lm.JIF.AI)
AIC(llm.JIF5.AI)







