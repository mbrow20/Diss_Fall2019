CAN2015REL_FULLIMPUTE2<-read.csv("CAN2015REL_FULLIMPUTE2.csv",header=T, sep=",")
data8<-CAN2015REL_FULLIMPUTE2
data8_control<-subset(data8, data8$treat=="0")
data8_treat<-subset(data8, data8$treat=="1")

formula17<-as.formula(ESCS~SC048Q02 + SC061Q07 +  CLSIZE + SC048Q03 + MMINS
                      + ST013Q01 + SC048Q01 + GENDER + ST011Q07 + HISCED + MISCED+ST034Q01+ST011Q06 +
                        SC048Q02:SC048Q03 + I(MMINS^2) + I(CLSIZE^2))

lm17_control<-lm(formula17, data=data8_control)
lm17_treat <-lm(formula17, data = data8_treat)
#summary(lm17_control)
#summary(lm17_treat)
sp_square = ((nrow(data8_control)-1)*(summary(lm17_control)$coefficients[[31]])^2+
               (nrow(data8_treat)-1)*(summary(lm17_treat)$coefficients[[31]])^2)/
  (nrow(data8_control)+nrow(data8_treat)-2)
sp<-sqrt(sp_square)
independ_ttest1<-(summary(lm17_control)$coefficients[[1]]-summary(lm17_treat)$coefficients[[1]])/sp
#independ_ttest1
########################MATCHED#########################################################
CAN2015REL_MATCHEDIMPUTE2<-read.csv("CAN2015REL_MATCHEDIMPUTE2.csv",header=T, sep=",")
data8<-CAN2015REL_MATCHEDIMPUTE2
data8_control<-subset(data8, data8$treat=="0")
data8_treat<-subset(data8, data8$treat=="1")

formula17<-as.formula(ESCS~SC048Q02 + SC061Q07 +  CLSIZE + SC048Q03 + MMINS
                      + ST013Q01 + SC048Q01 + GENDER + ST011Q07 + HISCED + MISCED+ST034Q01+ST011Q06 +
                        SC048Q02:SC048Q03 + I(MMINS^2) + I(CLSIZE^2))

lm17_control<-lm(formula17, data=data8_control)
lm17_treat <-lm(formula17, data = data8_treat)
#summary(lm17_control)
#summary(lm17_treat)
sp_square = ((nrow(data8_control)-1)*(summary(lm17_control)$coefficients[[31]])^2+
               (nrow(data8_treat)-1)*(summary(lm17_treat)$coefficients[[31]])^2)/
  (nrow(data8_control)+nrow(data8_treat)-2)
sp<-sqrt(sp_square)
independ_ttest2<-(summary(lm17_control)$coefficients[[1]]-summary(lm17_treat)$coefficients[[1]])/sp
#independ_ttest2
sprintf("The t-stat for the full imputed data is %f, and for the matched is %f",independ_ttest1,independ_ttest2)