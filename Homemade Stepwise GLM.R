library(pscl)

###TRY TO REMOVE THE ADDITIONAL Xs
###BETTER ONE
rm(results)
rm(keep)
rm(mod)
rm(y_exp)
rm(unk_r2)
rm(use_mod)

rm(R2)
rm(AP)
rm(AIC)
#rm(var_exp)
rm(ires)
rm(X_vars)
rm(ResultSummary)

results<-data.frame("int"=0, "factor_in" = 'var', "AIC" = 0.00, "R2" = 0.00)
keep<-data.frame("VarCnt"=0, "Exp"='exp', "int"=0, "factor_in" = 'var', "AIC" = 0.00, "R2" = 0.00)
t=ncol(T_Train)
unk_r2=data.frame("new_r2"=0)

#j=1
for (j in 1:t){
  if (j < 2){
    y="SalePrice~"
    y_exp<-y
    X_vars<-T_Train
  } else {
    y_exp<-paste(y_exp, keep[(j-1),4],'+')
  }
  
  k=ncol(X_vars)
 # print(k)
 # print(y_exp)
  for (i in 1:(k-2)) {
    #for (i in 1: 11) {
    #x=names(T_Train)[(i+1)]
    var = names(X_vars)[i+1]
    mod<-glm(paste(y_exp,var),
             family = Gamma(link="log"), 
             data = T_Train)
    AIC<-mod$aic
    R2<-with(summary(mod), 1 - deviance/null.deviance)
    ires<-cbind(i,var,AIC,R2)
    results[i,]<-ires
  }
  
  keep[j,]<-cbind(j, y_exp, results[order(results$R2, decreasing = TRUE), ][1,])
  use_mod<-glm(paste(keep[j,2],keep[j,4]),
               family = Gamma(link="log"), 
               data = T_Train)
  AP<-predict(object = use_mod, newdata = T_Val, type = "response")
  unk_r2[j,1]<-cor(AP,T_Val[,1], method = "spearman")
  X_vars<-select(X_vars,-(keep[j,4]))
}
ResultSummary<-cbind(keep,unk_r2)
ResultSummary
summary(use_mod)
names(ResultSummary)
ggplot(ResultSummary, aes(x=VarCnt) )+
#  geom_jitter(aes(y=R2), color = "blue")+
  geom_line(aes(y=new_r2), color = "red")+
  geom_line(aes(y=as.numeric(R2)), color = "blue")
####END BETTER ONE
ggplot(ResultSummary)+
geom_jitter(aes(x=new_r2, y = as.numeric(R2)))


ConComb variable needs work 





















j=2
ResultSummary<-cbind(Keep,unk_r20)
library(ggplot2)
ggplot(T_Train, aes(x=Ext_Concat))+
  geom_bar()+
  geom_text(aes(label = ..count..), stat="count")

table(data_fg$Exterior1st)
ggplot(T_Train, aes(x=Exterior1st))+
  geom_bar()+
  geom_text(aes(label = ..count..), stat="count")

x=fct_reorder(Exterior1st, SalePrice)
library(forcats)
ggplot(data_fg, aes(x=fct_reorder(tt, SalePrice), y = SalePrice))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle=90))
summary(data_fg$CntExtFin)
ggplot(data_fg, aes(x=fct_reorder(tt, SalePrice)))+
  geom_bar()+
  geom_text(aes(label = ..count..), stat="count")+
  theme(axis.text.x = element_text(angle=90))

levels(data_fg$Ext_Concat)  

data_fg$tt<-case_when(data_fg$Exterior1st == data_fg$Exterior2nd ~ paste("Dbl_",data_fg$Exterior1st),
          TRUE ~ paste(data_fg$Exterior1st,'_Mixed')
          )

ggplot(T_Val, aes(x=Ext_Concat))+geom_bar()
table(data_fg$Ext_Concat)
