library(survival)
library(SurvRegCensCov)
bankruptcy.df <- read.csv("/Users/chriskwan/Documents/R/RLabs/Bankruptcy Data.csv")
bankruptcy.df <- na.omit(bankruptcy.df)
#NA row checker
bankruptcy.df.na <- bankruptcy.df[rowSums(is.na(bankruptcy.df))>0,]
bankruptcy.df.na
#Plot survival first for initial diagnosis
out <- survfit(Surv(Years_to_Bankrupt,Bankruptcy)~1,data=bankruptcy.df)
y<-out$surv
y<- -log(y)
t<- out$time
plot(t,y,xlab = "Time Step (years)", ylab = "-log(probability of survival beyond time step)", main = "Survival Plot")

#Exponential
exp.out <- survreg(Surv(Years_to_Bankrupt,Bankruptcy)~Total_Revenue+Gross_Profit+Net_Income+EBITDA+Total_Current_Assets+Total_Assets+Total_Current_Liabilities+Cash_from_Ops. + Net_Debt.EBITDA+Profit_Margin+Net_Debt+Net_Workng_Capital+Current_Ratio+Debt_to_Cash_Flow_From_Ops+Market_Capitalization+Market_Capitalization.1, data=bankruptcy.df , dist='exponential')
summary(exp.out)
#Some of these variables tell us the same thing, remove EBITDA since we have it included in Net_DebtEBITDA and it is similar to Net Income
exp.out <- survreg(Surv(Years_to_Bankrupt,Bankruptcy)~Total_Revenue+Gross_Profit+Net_Income+Total_Current_Assets+Total_Assets+Total_Current_Liabilities+Cash_from_Ops. + Net_Debt.EBITDA+Profit_Margin+Net_Debt+Net_Workng_Capital+Current_Ratio+Debt_to_Cash_Flow_From_Ops+Market_Capitalization+Market_Capitalization.1, data=bankruptcy.df , dist='exponential')
summary(exp.out)
#Current ratio is current assets/current liabilities. Therefore we can remove both as they would indicate the same thing
exp.out <- survreg(Surv(Years_to_Bankrupt,Bankruptcy)~Total_Revenue+Gross_Profit+Net_Income+Total_Assets+Cash_from_Ops. + Net_Debt.EBITDA+Profit_Margin+Net_Debt+Net_Workng_Capital+Current_Ratio+Debt_to_Cash_Flow_From_Ops+Market_Capitalization+Market_Capitalization.1, data=bankruptcy.df , dist='exponential')
summary(exp.out)
#Net working capital generally tells the same thing as current ratio so lets remove it
exp.out <- survreg(Surv(Years_to_Bankrupt,Bankruptcy)~Total_Revenue+Gross_Profit+Net_Income+Total_Assets+Cash_from_Ops. + Net_Debt.EBITDA+Profit_Margin+Net_Debt+Current_Ratio+Debt_to_Cash_Flow_From_Ops+Market_Capitalization+Market_Capitalization.1, data=bankruptcy.df , dist='exponential')
summary(exp.out)
#Market Cap for a company that is might be bankrupt in previous year is more relevant than 2 years ago and likely says something similar. The market is dynamic and things can change quickly in a company. The current market cap probably says enough about the company.
exp.out <- survreg(Surv(Years_to_Bankrupt,Bankruptcy)~Total_Revenue+Gross_Profit+Net_Income+Total_Assets+Cash_from_Ops. + Net_Debt.EBITDA+Profit_Margin+Net_Debt+Current_Ratio+Debt_to_Cash_Flow_From_Ops+Market_Capitalization.1, data=bankruptcy.df , dist='exponential')
summary(exp.out)
#When a company goes bankrupt total assets generall doesn't matter it's cash that matters most. If you have a lot of assets you can still go bankrupt, it's just that you might be more likely to pay creditors with assets you have, so lets remove it
exp.out <- survreg(Surv(Years_to_Bankrupt,Bankruptcy)~Total_Revenue+Gross_Profit+Net_Income+Cash_from_Ops. + Net_Debt.EBITDA+Profit_Margin+Net_Debt+Current_Ratio+Debt_to_Cash_Flow_From_Ops+Market_Capitalization.1, data=bankruptcy.df , dist='exponential')
summary(exp.out)
#Net income and cash from operations are closely intertwind and a high net income usually means more cash from operations. Both say similar things to Gross Profit which is more significant. Profit Margin is also essentially Gross Profit. If a company is going bankrupt Gross Profit is more important since they can always slash costs to stay afloat
exp.out <- survreg(Surv(Years_to_Bankrupt,Bankruptcy)~Total_Revenue+Gross_Profit+ Net_Debt.EBITDA+Net_Debt+Current_Ratio+Debt_to_Cash_Flow_From_Ops+Market_Capitalization.1, data=bankruptcy.df , dist='exponential')
summary(exp.out)

#Try Weibull
weibull.out <- survreg(Surv(Years_to_Bankrupt,Bankruptcy)~Total_Revenue+Gross_Profit+Net_Income+EBITDA+Total_Current_Assets+Total_Assets+Total_Current_Liabilities+Cash_from_Ops. + Net_Debt.EBITDA+Profit_Margin+Net_Debt+Net_Workng_Capital+Current_Ratio+Debt_to_Cash_Flow_From_Ops+Market_Capitalization+Market_Capitalization.1, data=bankruptcy.df , dist='weibull')
weibull.out
ConvertWeibull(weibull.out)

#From here the hazard ratios which are not 1 are Net_Debt.EBITDA, Profit Margin, and Current Ratio
weibull.out <- survreg(Surv(Years_to_Bankrupt,Bankruptcy)~Net_Debt.EBITDA+Profit_Margin+Current_Ratio, data=bankruptcy.df , dist='weibull')
weibull.out
ConvertWeibull(weibull.out)

#Cox proportional Hazard Model
cox.out <- coxph(Surv(Years_to_Bankrupt,Bankruptcy)~Total_Revenue+Gross_Profit+Net_Income+EBITDA+Total_Current_Assets+Total_Assets+Total_Current_Liabilities+Cash_from_Ops. + Net_Debt.EBITDA+Profit_Margin+Net_Debt+Net_Workng_Capital+Current_Ratio+Debt_to_Cash_Flow_From_Ops+Market_Capitalization+Market_Capitalization.1, data=bankruptcy.df)
summary(cox.out)
#Remove EBITDA
cox.out <- coxph(Surv(Years_to_Bankrupt,Bankruptcy)~Total_Revenue+Gross_Profit+Net_Income+Total_Current_Assets+Total_Assets+Total_Current_Liabilities+Cash_from_Ops. + Net_Debt.EBITDA+Profit_Margin+Net_Debt+Net_Workng_Capital+Current_Ratio+Debt_to_Cash_Flow_From_Ops+Market_Capitalization+Market_Capitalization.1, data=bankruptcy.df)
summary(cox.out)
#Remove total current assets and liabilities
cox.out <- coxph(Surv(Years_to_Bankrupt,Bankruptcy)~Total_Revenue+Gross_Profit+Net_Income+Total_Assets+Cash_from_Ops. + Net_Debt.EBITDA+Profit_Margin+Net_Debt+Net_Workng_Capital+Current_Ratio+Debt_to_Cash_Flow_From_Ops+Market_Capitalization+Market_Capitalization.1, data=bankruptcy.df)
summary(cox.out)
#Remove cash from Ops
cox.out <- coxph(Surv(Years_to_Bankrupt,Bankruptcy)~Total_Revenue+Gross_Profit+Net_Income+Total_Assets+ Net_Debt.EBITDA+Profit_Margin+Net_Debt+Net_Workng_Capital+Current_Ratio+Debt_to_Cash_Flow_From_Ops+Market_Capitalization+Market_Capitalization.1, data=bankruptcy.df)
summary(cox.out)
#Remove Total Assets
cox.out <- coxph(Surv(Years_to_Bankrupt,Bankruptcy)~Total_Revenue+Gross_Profit+Net_Income+ Net_Debt.EBITDA+Profit_Margin+Net_Debt+Net_Workng_Capital+Current_Ratio+Debt_to_Cash_Flow_From_Ops+Market_Capitalization+Market_Capitalization.1, data=bankruptcy.df)
summary(cox.out)
#Remove Net Working Capital
cox.out <- coxph(Surv(Years_to_Bankrupt,Bankruptcy)~Total_Revenue+Gross_Profit+Net_Income+ Net_Debt.EBITDA+Profit_Margin+Net_Debt+Current_Ratio+Debt_to_Cash_Flow_From_Ops+Market_Capitalization+Market_Capitalization.1, data=bankruptcy.df)
summary(cox.out)
#Remove Net Income
cox.out <- coxph(Surv(Years_to_Bankrupt,Bankruptcy)~Total_Revenue+Gross_Profit+ Net_Debt.EBITDA+Profit_Margin+Net_Debt+Current_Ratio+Debt_to_Cash_Flow_From_Ops+Market_Capitalization+Market_Capitalization.1, data=bankruptcy.df)
summary(cox.out)
#Remove Market Cap
cox.out <- coxph(Surv(Years_to_Bankrupt,Bankruptcy)~Total_Revenue+Gross_Profit+ Net_Debt.EBITDA+Profit_Margin+Net_Debt+Current_Ratio+Debt_to_Cash_Flow_From_Ops+Market_Capitalization.1, data=bankruptcy.df)
summary(cox.out)
#Remove Profit Margin
cox.out <- coxph(Surv(Years_to_Bankrupt,Bankruptcy)~Total_Revenue+Gross_Profit+ Net_Debt.EBITDA+Net_Debt+Current_Ratio+Debt_to_Cash_Flow_From_Ops+Market_Capitalization.1, data=bankruptcy.df)
summary(cox.out)
#Remove 1s
cox.out <- coxph(Surv(Years_to_Bankrupt,Bankruptcy)~Total_Revenue+Gross_Profit+ Net_Debt.EBITDA+Current_Ratio+Debt_to_Cash_Flow_From_Ops+Market_Capitalization.1, data=bankruptcy.df)
summary(cox.out)
cox.out <- coxph(Surv(Years_to_Bankrupt,Bankruptcy)~Gross_Profit+ Net_Debt.EBITDA+Current_Ratio+Debt_to_Cash_Flow_From_Ops+Market_Capitalization.1, data=bankruptcy.df)
summary(cox.out)
cox.out <- coxph(Surv(Years_to_Bankrupt,Bankruptcy)~Net_Debt.EBITDA+Debt_to_Cash_Flow_From_Ops+Market_Capitalization.1, data=bankruptcy.df)
summary(cox.out)
