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
Pairs <- Data1[,c("JIF_5yr", "Eigenfactor", "AI", "h_index", "g_index", "hc_index", "hI_index", "hI_norm", "AW_index", "e_index", "hm_index", "Authors_Paper", "Cites_Paper", "Cites2011", "Citations")]
pairs(Pairs, upper.panel=panel.smooth, lower.panel=panel.cor.spear)

Corr.Pearson <- cor(Pairs, method="pearson") # assumes linear relationships
Corr.Spearman <- cor(Pairs, method="spearman") # assumes monotonic but not necessarily linear relationships

Corr.Spearman - Corr.Pearson # greater than 0 indicates monotonic but not linear

# Histogram of journals AI
hist(Data1$AI, breaks=10, freq=FALSE)

# ------------Examine relationship of EF and JIF more closely---------
plot(Data1$Eigenfactor, Data1$JIF_5yr)
lines(smooth.spline(Data1$Eigenfactor, Data1$JIF_5yr), col='red')

# log-log Linear regression of EF on JIF
lm.JIF.EF <- lm(log(Data1$JIF_5yr) ~ log(Data1$Eigenfactor))
plot(lm.JIF.EF)
summary(lm.JIF.EF)
R2.JIF.EF <- summary(lm.JIF.EF)$r.squared

# Plot log-log EF & JIF
plot(log(Data1$Eigenfactor), log(Data1$JIF_5yr), xlab=expression(paste(log[e], " ", Eigenfactor^TM)), ylab=expression(paste(log[e], " Journal Impact Factor (5-yr)")))
lines(log(Data1$Eigenfactor), fitted(lm.JIF.EF), col='red')
text(-7.5, 2.1, labels=expression(paste(R^2==0.6404)))
text(-7.5, 2.6, labels=expression(paste(log[e](JIF)==3.26 + 0.471%*%log[e](EF))))

# Models and tests of relationships
source('/Users/Dan/Documents/Statistics/R/Functions/Xiao_Power_Function.R') # Reference: Xiao et al. 2011 Ecology

# JIF5 as function of EF
(PA_EF_JIF5 <- power_analysis(x=Data1$Eigenfactor, y=Data1$JIF_5yr, output_plot=TRUE)) # Error appears multiplicative (log regresion better) # a=29.34, b=0.498

Data1$y.JIF5 <- 29.34*Data1$Eigenfactor^0.498 # calculate predicted values
dev.off()
par (mfrow=c(1,1), mar=c(4,3,2,1), mgp=c(2,.7,0), tck=-.01)
Data1 <- Data1[order(Data1$Eigenfactor), ]
plot(Data1$Eigenfactor, Data1$JIF_5yr, xlab='Eigenfactor', ylab='Journal impact factor (5-yr)')
lines(Data1$Eigenfactor, Data1$y.JIF5)
text(0.088, y=17, labels=expression(R^2==0.6387))

summary(lm.JIF.EF)
(R2 <- summary(lm.JIF.EF)$r.squared) # 0.6387
(R2adj <- summary(lm.JIF.EF)$adj.r.squared) # 0.6356


# JIF as function of H-index
plot(Data1$h_index, Data1$JIF_5yr) # model as linear

lm.JIF.h <- lm(JIF_5yr ~ h_index, data=Data1)
plot(lm.JIF.h) # poor fit
lm.logJIF.h <- lm(log(JIF_5yr) ~ h_index, data=Data1)
plot(lm.logJIF.h) # still poor fit
lm.logJIF.logh <- lm(log(JIF_5yr) ~ log(h_index), data=Data1)
plot(lm.logJIF.logh) # better residuals but poor normality
summary(lm.logJIF.logh)

(PA_H_JIF5 <- power_analysis(x=Data1$h_index, y=Data1$JIF_5yr, output_plot=TRUE)) # Error appears multiplicative (log regresion better) # a=0.0515, b=1.1405

dev.off()
Data1$y.JIF5.h <- 0.0514*Data1$h_index^1.1405 # calculate predicted values
par (mfrow=c(1,1), mar=c(4,3,2,1), mgp=c(2,.7,0), tck=-.01)
Data1 <- Data1[order(Data1$h_index), ]
plot(Data1$h_index, Data1$JIF_5yr, xlab='H-index', ylab='Journal impact factor (5-yr)')
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
lm.JIF.AI <- lm(JIF_5yr ~ AI, Data1)
summary(lm.JIF.AI) # slope = 1.888, intercept = 0.470
plot(Data1$AI, Data1$JIF_5yr, xlab='Article Influence', ylab='Journal Impact Factor (5 yr)')
abline(lm.JIF.AI)
text(x=6, y=17, labels=expression(slope==2.048))
text(x=6, y=15.8, labels=expression(R^2==0.9763))










