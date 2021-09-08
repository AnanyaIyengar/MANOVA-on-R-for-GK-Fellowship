#Work on final data sets

library(ggplot2)
library(dplyr)
library(readr)
library(AER)
library(car)
library(MASS)
library(mvtnorm)
library(ggpubr)
library(mvShapiroTest)
library(stargazer)
library(readxl)
library(gridExtra)
library(psych)
library(biotools)
library(DescTools)
library(rstatix)

#Loading Data Sets  

before<-read_excel("before.xlsx")
after<-read_excel("after.xlsx")       
combined<-read_excel("combined.xlsx")

boxplot1<-ggplot(combined, aes(group, impersonation))+geom_boxplot(color="black", fill="light blue")+theme_pubclean()+ylab("Reliability Score")+ggtitle("Impersonation")+xlab("")
boxplot2<-ggplot(combined, aes(group, discredit))+geom_boxplot(color="black", fill="light blue")+theme_pubclean()+ylab("Reliability Score")+ggtitle("Discredit")+xlab("")
boxplot3<-ggplot(combined, aes(group, conspiracy))+geom_boxplot(color="black", fill="light blue")+theme_pubclean()+ylab("Reliability Score")+ggtitle("Conspiracy")+xlab("")
boxplot4<-ggplot(combined, aes(group, control))+geom_boxplot(color="black", fill="light blue")+theme_pubclean()+ylab("Reliability Score")+ggtitle("Control")+xlab("")

grid.arrange(boxplot1, boxplot2, boxplot3, boxplot4)

description_by_group_impersonation<-describeBy(combined$impersonation, combined$group)
description_by_group_discredit<-describeBy(combined$discredit,combined$group)
descrption_by_group_conspiracy<-describeBy(combined$conspiracy, combined$group)
description_by_group_control<-describeBy(combined$control, combined$group)

description_by_group_impersonation
description_by_group_discredit
descrption_by_group_conspiracy
description_by_group_control

dependent_variables<-combined%>%dplyr::select(impersonation, conspiracy, discredit, control)
mvShapiro.Test(as.matrix(dependent_variables))  # not normal

wilcox.test(before$pre_impersonation, after$post_impersonation, paired = TRUE)

cor(dependent_variables) 

#BASIC MANOVA MODEL
basicmanova<-manova(cbind(impersonation, discredit, conspiracy, control)~as.factor(group), data=combined)
summary(basicmanova, intercept = TRUE)
summary(basicmanova, intercept = TRUE, test = "Wilks")
summary(basicmanova, intercept = TRUE, test = "Hotelling")
basicmanova$coefficients
summary.aov(basicmanova)

#MANCOVA: Adding continuous covariates
mancova1<-manova(cbind(impersonation, discredit, conspiracy, control)~ as.factor(group)+age+politics, data=combined)
summary(mancova1, intercept = TRUE)
summary(mancova1, intercept = TRUE, test = "Wilks")
summary(mancova1, intercept = TRUE, test = "Hotelling")
mancova1$coefficients
summary.aov(mancova1)

#MANCOVA: With interaction terms with the covariates
mancova2<-manova(cbind(impersonation, discredit, conspiracy, control)~as.factor(group)+politics+as.factor(group)*politics+age+as.factor(group)*age, data=combined)
summary(mancova2, intercept = TRUE)
summary(mancova2, intercept = TRUE, test = "Wilks")
summary(mancova2, intercept = TRUE, test = "Hotelling")
mancova2$coefficients
summary.aov(mancova2)


#ANOVA Models to include Dummies, Interaction terms and Bonferroni Confidence Levels (0.05/4 = 0.125)

impersonation_anova<-lm(impersonation~as.factor(group)+age+gender+intuition+politics+as.factor(group)*age + as.factor(group)*gender + as.factor(group)*intuition + as.factor(group)*politics, data=combined)
impersonation_anova
summary(impersonation_anova)
AIC(impersonation_anova) 

conspiracy_anova<-lm(conspiracy~as.factor(group)+age+gender+intuition+politics+as.factor(group)*age+ as.factor(group)*gender+as.factor(group)*politics, data=combined)
conspiracy_anova                        
summary(conspiracy_anova)   

discredit_anova<-lm(discredit~as.factor(group)+age+gender+intuition+politics+as.factor(group)*age+ as.factor(group)*gender+as.factor(group)*politics, data=combined)
discredit_anova
summary(discredit_anova)                        

control_anova<-lm(control~as.factor(group)+age+gender+intuition+politics+as.factor(group)*age+ as.factor(group)*gender+as.factor(group)*politics, data=combined)
control_anova
summary(control_anova) #clearly, other variables do not affect control scores

cohens_d(combined, conspiracy~group, paired='TRUE')
cohens_d(combined, discredit~group, paired = TRUE)
cohens_d(combined, impersonation~group, paired = "TRUE")
cohens_d(combined, control~group, paired=TRUE)

violin1<-ggplot(combined, aes(group, impersonation, fill=group))+geom_violin()+geom_boxplot(width=0.1, alpha=0.5)+theme_pubclean()+xlab("")+ylab("Reliability Score")+ggtitle("Impersonation")
violin2<-ggplot(combined, aes(group, conspiracy, fill=group))+geom_violin()+geom_boxplot(width=0.1, alpha=0.5)+theme_pubclean()+xlab("")+ylab("Reliability Score")+ggtitle("Conspiracy")
violin3<-ggplot(combined, aes(group, discredit, fill=group))+geom_violin()+geom_boxplot(width=0.1, alpha=0.5)+theme_pubclean()+xlab("")+ylab("Reliability Score")+ggtitle("Discredit")
violin4<-ggplot(combined, aes(group, control, fill=group))+geom_violin()+geom_boxplot(width=0.1, alpha=0.5)+theme_pubclean()+xlab("")+ylab("Reliability Score")+ggtitle("Control")

grid.arrange(violin1, violin2, violin3, violin4)






