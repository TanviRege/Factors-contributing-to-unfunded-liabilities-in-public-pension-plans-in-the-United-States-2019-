pacman :: p_load(pacman, tidyverse, dplyr, ggplot2, cowplot, pastecs, car)
install.packages("lmtest")
install.packages("sandwich")
library(stringr)
library(pastecs)
library (scales)
library(car)
library(lmtest)
library(sandwich)

#importing Public Plans Database
pension_data <- read.csv('OneDrive/Desktop/ECON 6375 Econometrics/Research paper/ppd-data-latest.csv')

#filtering for 2019 Fiscal Year and State as Administering Government:
data_filter_yr <- pension_data %>%
  filter(fy== 2019)

data_final <- data_filter_yr %>%
  filter(AdministeringGovt== 0)

#creating a ratio for age dependency:
age_ratio <- (data_final$actives_tot) / (data_final$beneficiaries_tot)
data_final$age_ratio <- age_ratio

#statistical summaries of variables of interest:
vars_interest <- data_final %>%
  select(ActFundedRatio_GASB, age_ratio, PercentReqContPaid, BlendedDiscountRate)
options(scipen=999)
stat.desc(vars_interest)
vars_interest <- na.omit(vars_interest)

#histograms for each independent variable:
hist(vars_interest$age_ratio, main = "Histogram of Age Ratio",
     xlab = "Age Ratio",
     col = "red")

hist(vars_interest$PercentReqContPaid, main = "Histogram of Employer's Contribution",
     xlab = "Share of Employer's Contribution",
     col = "dark green")

hist(vars_interest$BlendedDiscountRate, main = "Histogram of Blended Discount Rate",
     xlab = "Blended Discount Rate",
     col = "brown")

#the variation in all three histograms is acceptable, with the one for Blended Discount Rate
#skewed slightly to the left.

#scatterplots of ActFundedRatio_GASB with each independent variable:
plot_Y_age <- ggplot(vars_interest, aes(x = age_ratio, y = ActFundedRatio_GASB)) +
  geom_point(color = "red")+
  xlab("Ratio of actives to beneficiaries") + ylab("Funded Ratio (percentage point)") + 
  ggtitle("A Pension Plan's Funded Ratio v/s Age Ratio", subtitle = "As the young population exceeds the old, the pension plan's funded ratio increases") +
  labs(caption = "Source: Public Plans Database (CRR and the MissionSquare Research Institute)") +
  geom_smooth(method = "lm")
plot_Y_age

plot_Y_contr <- ggplot(vars_interest, aes(x = PercentReqContPaid, y = ActFundedRatio_GASB)) +
  geom_point(color = "dark green")+
  xlab("Share of Required Contribution paid by Employer (percentage point)") + ylab("Funded Ratio (percentage point)") + 
  ggtitle("A Pension Plan's Funded Ratio v/s Share of Employer's Contribution Paid", subtitle = "The more the plan is sponsored by the employer, the higher is its funded ratio") +
  labs(caption = "Source: Public Plans Database (CRR and the MissionSquare Research Institute)") +
  geom_smooth(method = "lm")
plot_Y_contr

plot_Y_disc <- ggplot(vars_interest, aes(x = BlendedDiscountRate, y = ActFundedRatio_GASB)) +
  geom_point(color = "brown")+
  xlab("Blended Discount Rate (percentage point)") + ylab("Funded Ratio (percentage point)") + 
  ggtitle("A Pension Plan's Funded Ratio v/s Blended Discount Rate", subtitle = "A high blended discount rate is moderately associated with the plan's funded ratio") +
  labs(caption = "Source: Public Plans Database (CRR and the MissionSquare Research Institute)") +
  geom_smooth(method = "lm")
plot_Y_disc

#correlation matrix of the variables of interest:
cor(vars_interest)

#specification 1: MLR with all 3 independent variables
pairs(vars_interest) #there is no curvilinear relationship observed between any vars; can use MLR
lm1 <- lm(vars_interest$ActFundedRatio_GASB~vars_interest$age_ratio+vars_interest$PercentReqContPaid+vars_interest$BlendedDiscountRate)

plot(lm1) #to check whether the data is linear, and for any leverage points
#observation 64 is a leverage point, and must be excluded:
vars_interest <- vars_interest[-c(64),]

lm1_v2 <- lm(vars_interest$ActFundedRatio_GASB~vars_interest$age_ratio+vars_interest$PercentReqContPaid+vars_interest$BlendedDiscountRate)
plot(lm1_v2)

#as there is no leverage point now:
summary(lm1_v2)
#The Blended Discount Rate is not statistically significant, and may be removed from the model.

vif(lm1_v2) #no multicollinearity observed between independent vars (7<VIF<10 is high)

bptest(lm1_v2) #to test for Heteroskedasticity, I use the Breusch-Pagan test.
#The p-value (0.023) is lesser than 0.05, so I can reject the null hypothesis and say that
#there may exist heteroskedasticity in my sample data. Therefore, I will report the robust 
#standard errors too:

coeftest(lm1_v2, vcov = vcovHC(lm1_v2, "HC1")) #HC1 gives the robust White standard errors

#to check whether I must add a quadratic term in my model, I conduct a Ramsey RESET test:
resettest(lm1_v2, power = 2:3, type = "regressor")
#The p-value (0.65) is higher than 0.05, so I can conclude that I do not need to add a squared 
#or cubed regressor in my model.

#specification 2: MLR without Blended Discount Rate
lm2 <- lm(vars_interest$ActFundedRatio_GASB~vars_interest$age_ratio+vars_interest$PercentReqContPaid)

plot(lm2) #to check whether the data is linear, and for any leverage points
#as there is no leverage point:
summary(lm2)
coeftest(lm2, vcov = vcovHC(lm2, "HC1")) #HC1 gives the robust White standard errors
#the adjusted R squared increases slightly when Blended Discount Rate is dropped. All 
#independent variables are statistically significant at all levels of significance.

#specification 3: MLR with interaction term
lm3 <- lm(vars_interest$ActFundedRatio_GASB~vars_interest$age_ratio+vars_interest$PercentReqContPaid+vars_interest$age_ratio*vars_interest$PercentReqContPaid)

plot(lm3) #to check whether the data is linear, and for any leverage points
#as there is no leverage point:
summary(lm3)
coeftest(lm3, vcov = vcovHC(lm3, "HC1")) #HC1 gives the robust White standard errors
#although the R squared & adjusted R squared has increased in this specification, the
#independent variables and the interaction term are not statistically significant. Hence, my
#preferred specification is (2).

vif(lm3) #high multicollinearity observed between independent vars due to interaction term

#specification 4: Standardized Coefficients of the specification (2)
lm4 <- lm(scale(vars_interest$ActFundedRatio_GASB) ~ scale(vars_interest$age_ratio)+scale(vars_interest$PercentReqContPaid))
summary(lm4)
#the percent contributed by the employer is the strongest variable (highest beta coefficient) 
#affecting the funded ratio, followed closely by the age ratio. 

#End of R-code.
