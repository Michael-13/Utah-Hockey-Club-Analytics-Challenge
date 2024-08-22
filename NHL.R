library(leaps)
library(ggplot2)
library(MASS)
# Load in Data Frame
teamStats <- read.csv("/home/mhockey/Downloads/teamStats.csv")

# Use Exhaustive Search to best subset
best.subset = summary(regsubsets(teamStats$W ~ ., data=teamStats, nbest=1))
best.subset

# Best Model
m8 = lm(teamStats$W~teamStats$SRS+teamStats$GF.G+teamStats$PP.
                   +teamStats$PK.+teamStats$S+teamStats$S.+teamStats$SA
                   +teamStats$PDO, data = teamStats)
outdat=cbind(1:8, best.subset$rss, best.subset$adjr2, best.subset$cp, best.subset$bic)
colnames(outdat) = c("p", "SSR", "adjR2", "Cp", "BIC")
outdat

# Plot all values to see if there is a linear relationship
pairs(teamStats$W~teamStats$SRS+teamStats$GF.G+teamStats$PP.
      +teamStats$PK.+teamStats$S+teamStats$S.+teamStats$SA
      +teamStats$PDO,
      data=teamStats, gap=0.1)

# Residual Analysis
r = round(m8$res,2)
rstd= round(rstandard(m8),2)
leverage = round(hatvalues(m8),2)
cooksD =round(cooks.distance(m8), 2)
data.frame(leverage, r, rstd, cooksD)

# Residual Plots (stdres.vs.fittedvalues, leverage plot, Cook's Distance)
par(mfrow=c(2,2))
plot(m8$fitted.values, rstd, ylim=c(-5, 5),
     xlab="Fitted values", ylab="Standardized residuals")
abline(h=c(3, -3), col="red", lty=2)

index=1:127
plot(index, leverage, ylab="Leverage")
abline(h=2*mean(leverage), col="red", lty=2)

plot(index, cooksD,ylab="Cook's distance")

par(mfrow=c(2,2))
plot(m8)

# Fit new models without outliers
dataNo14 <- read.csv("/home/mhockey/Downloads/teamStatsno14.csv")
m8No14 = lm(dataNo14$W~dataNo14$SRS+dataNo14$GF.G+dataNo14$PP.
        +dataNo14$PK.+dataNo14$S+dataNo14$S.+dataNo14$SA
        +dataNo14$PDO, data = dataNo14)
dataNo36 <- read.csv("/home/mhockey/Downloads/teamStatsno36.csv")
m8No36 = lm(dataNo36$W~dataNo36$SRS+dataNo36$GF.G+dataNo36$PP.
            +dataNo36$PK.+dataNo36$S+dataNo36$S.+dataNo36$SA
            +dataNo36$PDO, data = dataNo36)
dataNo63 <- read.csv("/home/mhockey/Downloads/teamStatsno63.csv")
m8No63 = lm(dataNo63$W~dataNo63$SRS+dataNo63$GF.G+dataNo63$PP.
            +dataNo63$PK.+dataNo63$S+dataNo63$S.+dataNo63$SA
            +dataNo63$PDO, data = dataNo63)

summary(m8No14)
summary(m8No36)
summary(m8No63)
summary(m8)

# do we need a transformation
bc = boxcox(m8No14)
lambda=bc$x[which.max(bc$y)]

m8No14Trans = lm(dataNo14$W^lambda~dataNo14$SRS+dataNo14$GF.G+dataNo14$PP.
                 +dataNo14$PK.+dataNo14$S+dataNo14$S.+dataNo14$SA
                 +dataNo14$PDO, data = dataNo14)
summary(m8No14Trans)
m8No14Trans$coefficients
plot(m8No14Trans)

# Hypothesis Testing
T = (0.0409035/0.0204056)
print(T)