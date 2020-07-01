#####STA4211 PROJECT######
install.packages("tidyverse")
install.packages("caret")

library(ggplot2)
library(tidyverse)
library(caret)

dat = read.csv("covid_19_data.csv", header=TRUE)
attach(dat)

###SUMMARY STATISTICS GRAPHS
##scatter plot for densities
scatterPlot <- ggplot(dat,aes(Confirmed, Deaths, color=as.factor(Country))) + 
  geom_point() + 
  scale_color_manual(values = c('#F7DC6F', '#2ECC71', '#3498DB')) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1))
scatterPlot
scatterPlot2 <- ggplot(dat,aes(Recovered, Deaths, color=as.factor(Country))) + 
  geom_point() + 
  scale_color_manual(values = c('#F7DC6F', '#2ECC71', '#3498DB')) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1))
scatterPlot2
summary(dat)
# Marginal density plot of x 
xdensity <- ggplot(dat, aes(Confirmed, fill=as.factor(Country))) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c('#F7DC6F', '#2ECC71', '#3498DB')) + 
  xlim(0, 750)+
  theme(legend.position = "none")
xdensity

# Marginal density plot of y (right panel)
ydensity <- ggplot(dat, aes(Deaths, fill=as.factor(Country))) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c('#F7DC6F', '#2ECC71', '#3498DB')) + 
  xlim(0,150)+
  theme(legend.position = "none")
ydensity

###CREATING MODEL
mod = lm(Deaths ~ Confirmed + Recovered + as.factor(Country))
summary(mod)

###CHECKING ASSUMPTIONS
##linear relationship
#scatterplot for confirmed vs. deaths
ggplot(dat, aes(x=Confirmed, y=Deaths)) + geom_point(size=2, color= "skyblue2")
#scatterplot for recovered vs.deaths
ggplot(dat, aes(x=Recovered, y=Deaths)) + geom_point(size=2, color= "skyblue2")

##normality of errors
#qq plot 
qqnorm(mod$residuals, ylab="Residuals", xlab="Normal Scores", main ="Normal Q-Q Plot")
qqline(mod$residuals, col="firebrick3")

##multicollinearity
#VIF
car::vif(mod)

##homoscedacity
#scatterplot of residuals vs. predicted values
#obtain predicted and residual values
dat_predicted <- predict(mod)
dat_resdiuals <- residuals(mod)

plot(dat_predicted, dat_resdiuals,
       ylab="Residuals", xlab="Predicted Values", 
       main="Residuals vs. Predicted") 
abline(0, 0)  

###DIAGNOSTICS
##Finding r squared
summary(mod)$r.squared
##Finding adj r squared
summary(mod)$adj.r.squared
##f- test
#getting anova for F-test
anova(mod)
# getting p value 
qf(0.95, 4, 3528)
##t-tests
#qt for beta1
qt(0.975, 3528)

