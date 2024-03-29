#FINAL CODE

library(ggplot2)
library(dplyr)
library(readr)
library(AER)
library(car)
library(MASS)
library(mvtnorm)
library(ggpubr)
library(mvShapiroTest)
library(readxl)
library(gridExtra)
library(psych)
library(biotools)
library(DescTools)
library(rstatix)
library(lmtest)
library(effectsize)

###PLEASE IMPORT THE THREE DATA SETS: before, after and combined TO CONTINUE###

#DESCRIPTIVE STATISTICS

combined$group<-factor(combined$group, levels = c("before", "after"), ordered = TRUE)

boxplot1<-ggplot(combined, aes(group, impersonation))+geom_boxplot(color="black", fill="light blue")+theme_pubclean()+ylab("Reliability Score")+ggtitle("Impersonation")+xlab("")+stat_summary(fun = mean, geom = "point", size = 4, color = "black", fill = "black")
boxplot2<-ggplot(combined, aes(group, discredit))+geom_boxplot(color="black", fill="light blue")+theme_pubclean()+ylab("Reliability Score")+ggtitle("Discredit")+xlab("")+stat_summary(fun=mean, geom="point", size = 4, color = "black", fill = "black")
boxplot3<-ggplot(combined, aes(group, conspiracy))+geom_boxplot(color="black", fill="light blue")+theme_pubclean()+ylab("Reliability Score")+ggtitle("Conspiracy")+xlab("")+stat_summary(fun=mean, geom="point", size = 4, color = "black", fill = "black")
boxplot4<-ggplot(combined, aes(group, control))+geom_boxplot(color="black", fill="light blue")+theme_pubclean()+ylab("Reliability Score")+ggtitle("Control")+xlab("")+stat_summary(fun=mean, geom="point", size = 4, color = "black", fill = "black")

grid.arrange(boxplot1, boxplot2, boxplot3, boxplot4)

description_by_group_impersonation<-describeBy(combined$impersonation, combined$group)
description_by_group_discredit<-describeBy(combined$discredit,combined$group)
descrption_by_group_conspiracy<-describeBy(combined$conspiracy, combined$group)
description_by_group_control<-describeBy(combined$control, combined$group)

description_by_group_impersonation
description_by_group_discredit
descrption_by_group_conspiracy
description_by_group_control

#NORMALITY TEST

dependent_variables<-combined%>%dplyr::select(impersonation, conspiracy, discredit, control)
mvShapiro.Test(as.matrix(dependent_variables))  

#NON_PARAMETRIC TESTING

wilcox.test(before$pre_impersonation, after$post_impersonation, paired = TRUE)

cor(dependent_variables) 

#AGGREGATE PRE-POST INDEX VARIABLE CREATION

aggregate_pre <- (before$pre_conspiracy+before$pre_discredit+before$pre_impersonation)/3
aggregate_post <- (after$post_impersonation + after$post_conspiracy + after$post_discredit)/3

mean_together_pre <- mean(aggregate_pre)
mean_together_post <- mean(aggregate_post)

before <- before%>%dplyr::mutate(aggregate_pre = (pre_conspiracy + pre_discredit + pre_impersonation)/3)
after <- after%>%dplyr::mutate(aggregate_post = (post_conspiracy + post_impersonation + post_discredit)/3)

combined <- combined%>%dplyr::mutate(avg = (impersonation + discredit + conspiracy)/3)
t.test(combined$avg ~ combined$group, var.equal = TRUE)

#PRIOR SUSCEPTIBILITY TESTING 

pre_median <- median(aggregate_pre)
post_median <- median(aggregate_post)

before_lower_than_median <- before%>%dplyr::filter(aggregate_pre < 3.66667)
before_greater_than_median <- before%>%dplyr::filter(aggregate_pre >= 3.66667)
after_lower_than_median <- after%>%dplyr::filter(aggregate_post< 3.333)
after_greater_than_median <- after%>%dplyr::filter(aggregate_post >= 3.333)


mean(after_lower_than_median$aggregate_post)
mean(before_lower_than_median$aggregate_pre)

mean(after_greater_than_median$aggregate_post)
mean(before_greater_than_median$aggregate_pre)

t.test(before_lower_than_median$aggregate_pre, after_lower_than_median$aggregate_post, paired = FALSE)
t.test(before_greater_than_median$aggregate_pre, after_greater_than_median$aggregate_post, paired = FALSE)

#BASIC MANOVA MODEL
basicmanova<-manova(cbind(impersonation, discredit, conspiracy, control)~as.factor(group), data=combined)
summary(basicmanova, intercept = TRUE)
summary(basicmanova, intercept = TRUE, test = "Wilks")
summary(basicmanova, intercept = TRUE, test = "Hotelling")
basicmanova$coefficients
summary.aov(basicmanova)
effectsize::eta_squared(basicmanova)

t.test(combined$impersonation ~ combined$group, var.equal = TRUE)
t.test(combined$conspiracy ~ combined$group, var.equal = TRUE)
t.test(combined$control ~ combined$group, var.equal = TRUE)
t.test(combined$discredit ~ combined$group, var.equal = TRUE)

#ANOVA Models to include Dummies, Interaction terms and Bonferroni Confidence Levels (0.05/4 = 0.125)

impersonation_anova<-lm(impersonation~as.factor(group)+age+gender+education+intuition+factcheck+LeftRight+LibAuth+group*LeftRight+group*LibAuth, data=combined)
bptest(impersonation_anova, data = combined, studentize = FALSE) 
a = DurbinWatsonTest(impersonation_anova)
print(a)
robust_impersonation<-coeftest(impersonation_anova, vcov=vcovHC(impersonation_anova, type = "HC0"))
robust_impersonation

conspiracy_anova<-lm(conspiracy~as.factor(group)+age+gender+education+intuition+factcheck+LeftRight+LibAuth+group*LeftRight+group*LibAuth, data=combined)
bptest(conspiracy_anova, data=combined, studentize = FALSE) 
b = DurbinWatsonTest(conspiracy_anova)
print(b)
robust_conspiracy<-coeftest(conspiracy_anova, vcov=vcovHC(conspiracy_anova, type = "HC0"))
robust_conspiracy

discredit_anova<-lm(discredit~as.factor(group)+age+gender+education+intuition+factcheck+LeftRight+LibAuth+group*LeftRight+group*LibAuth, data=combined)
bptest(discredit_anova, data = combined, studentize = FALSE) 
c = DurbinWatsonTest(discredit_anova)
print(c)
robust_discredit<-coeftest(discredit_anova, vcov=vcovHC(discredit_anova, type = "HC0"))
robust_discredit

control_anova<-lm(control~as.factor(group)+age+gender+education+intuition+factcheck+LeftRight+LibAuth+group*LeftRight+group*LibAuth, data=combined)
bptest(control_anova, data = combined, studentize = FALSE) 
d = DurbinWatsonTest(control_anova)
print(d)
robust_control<-coeftest(control_anova, vcov=vcovHC(control_anova, type = "HC0"))
robust_control

#EFFECT SIZES

cohen.d(combined$avg, combined$group)
cohen.d(combined$impersonation, combined$group)
cohen.d(combined$conspiracy, combined$group)
cohen.d(combined$discredit, combined$group)
cohen.d(combined$control, combined$group)

#VISUALISATION

violin1<-ggplot(combined, aes(group, impersonation, fill=group))+geom_violin()+geom_boxplot(width=0.1, alpha=0.5)+theme_pubclean()+xlab("")+ylab("Reliability Score")+ggtitle("Impersonation")
violin2<-ggplot(combined, aes(group, conspiracy, fill=group))+geom_violin()+geom_boxplot(width=0.1, alpha=0.5)+theme_pubclean()+xlab("")+ylab("Reliability Score")+ggtitle("Conspiracy")
violin3<-ggplot(combined, aes(group, discredit, fill=group))+geom_violin()+geom_boxplot(width=0.1, alpha=0.5)+theme_pubclean()+xlab("")+ylab("Reliability Score")+ggtitle("Discredit")
violin4<-ggplot(combined, aes(group, control, fill=group))+geom_violin()+geom_boxplot(width=0.1, alpha=0.5)+theme_pubclean()+xlab("")+ylab("Reliability Score")+ggtitle("Control")

grid.arrange(violin1, violin2, violin3, violin4)




