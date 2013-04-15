#############################################################################
# Comparing Influence of Ecology Journals
# Daniel J. Hocking
# Start: 05 April 2013
#############################################################################

par (mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01)

# 2011 Impact Factor, Citations, Eigenfactor, & Article Influence retrieved from Journal Citation Reports on ISI Web of Knowledge on 04 April 2013

# 2007-2011 h, g, hc, hi indices retrieved using Publish or Perish software on 06 April 2013: Harzing, A.W. (2007) Publish or Perish, available from http://www.harzing.com/pop.htm 

# Metrics from PoP limited to top 1000 papers in each journal

Data <- read.table('jcr_2011_04-05013.csv', header=TRUE, sep=',')

summary(Data)



#------------------Data Exploration------------------
source('/Users/Dan/Documents/Statistics/R/Functions/panelcor.R')

Data1 <- na.omit(Data) # remove NA for correlations
# Scatterplot matrix and pearson correlations
pairs(Data1[,c("JIF_5yr", "Eigenfactor", "AI", "Cites2011")], upper.panel=panel.smooth, lower.panel=panel.cor)

cor(Data1[,c("JIF_5yr", "Eigenfactor", "AI", "Cites2011")], method="pearson")
cor(Data1[,c("JIF_5yr", "Eigenfactor", "AI", "Cites2011")], method="spearman")

# Examine relationship of EF and JIF more closely
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
text(-7.5, 2.6, labels=expression(paste(log[e](JIF)==3.26 + 0.471%*%log[e](Eigenfactor))))

# Test 
source('/Users/Dan/Documents/Statistics/R/Functions/Xiao_Power_Function.R') # Reference: Xiao et al. 2011 Ecology

power_analysis(x=Data1$Eigenfactor, y=Data1$JIF_5yr, output_plot=TRUE) # Error appears multiplicative (log regresion better)

summary(lm.JIF.EF)
(R2 <- summary(lm.JIF.EF)$r.squared) # 0.6404
(R2adj <- summary(lm.JIF.EF)$adj.r.squared) # 0.6375




