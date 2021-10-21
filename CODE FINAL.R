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
library(stargazer)
library(readxl)
library(gridExtra)
library(psych)
library(biotools)
library(DescTools)
library(rstatix)


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

#ANOVA Models to include Dummies, Interaction terms and Bonferroni Confidence Levels (0.05/4 = 0.125)

impersonation_anova<-lm(impersonation~as.factor(group)+age+gender+education+voterstatus+intuition+factcheck+LeftRight+LibAuth+group*LeftRight+group*LibAuth, data=combined)
impersonation_anova
a = summary(impersonation_anova)


conspiracy_anova<-lm(conspiracy~as.factor(group)+age+gender+education+voterstatus+intuition+factcheck+LeftRight+LibAuth+group*LeftRight+group*LibAuth, data=combined)
conspiracy_anova                        
b = summary(conspiracy_anova)   

discredit_anova<-lm(discredit~as.factor(group)+age+gender+education+voterstatus+intuition+factcheck+LeftRight+LibAuth+group*LeftRight+group*LibAuth, data=combined)
discredit_anova
c = summary(discredit_anova)                        

control_anova<-lm(control~as.factor(group)+age+gender+education+voterstatus+intuition+factcheck+LeftRight+LibAuth+group*LeftRight+group*LibAuth, data=combined)
control_anova
d = summary(control_anova) 


cohens_d(combined, conspiracy~group, paired= TRUE)
cohens_d(combined, discredit~group, paired = TRUE)
cohens_d(combined, impersonation~group, paired = TRUE)
cohens_d(combined, control~group, paired=TRUE)

violin1<-ggplot(combined, aes(group, impersonation, fill=group))+geom_violin()+geom_boxplot(width=0.1, alpha=0.5)+theme_pubclean()+xlab("")+ylab("Reliability Score")+ggtitle("Impersonation")
violin2<-ggplot(combined, aes(group, conspiracy, fill=group))+geom_violin()+geom_boxplot(width=0.1, alpha=0.5)+theme_pubclean()+xlab("")+ylab("Reliability Score")+ggtitle("Conspiracy")
violin3<-ggplot(combined, aes(group, discredit, fill=group))+geom_violin()+geom_boxplot(width=0.1, alpha=0.5)+theme_pubclean()+xlab("")+ylab("Reliability Score")+ggtitle("Discredit")
violin4<-ggplot(combined, aes(group, control, fill=group))+geom_violin()+geom_boxplot(width=0.1, alpha=0.5)+theme_pubclean()+xlab("")+ylab("Reliability Score")+ggtitle("Control")

grid.arrange(violin1, violin2, violin3, violin4)




