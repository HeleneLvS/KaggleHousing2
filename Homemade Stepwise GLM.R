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
t=ncol(T_Train)-1
unk_r2=data.frame("new_r2"=0)

#j=2
for (j in 1:t){
  if (j < 2){
    y="SalePrice~"
    y_exp<-y
    X_vars<-T_Train
  } else {
    y_exp<-paste(y_exp, keep[(j-1),4],'+')
    }
  if (j>1 & cond > 0){
    break
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
    AIC<-as.numeric(mod$aic)
    R2<-as.numeric(with(summary(mod), 1 - deviance/null.deviance))
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
  if (j>1) {
    cond <- as.numeric(keep[j,5]) - as.numeric(keep[j-1,5])
    } else { cond<- 0 }
}



ResultSummary<-cbind(keep,unk_r2)
#ResultSummary$AIC<-as.numeric(ResultSummary$AIC)
summary(use_mod)
names(ResultSummary)
ggplot(ResultSummary, aes(x=VarCnt) )+
#  geom_jitter(aes(y=R2), color = "blue")+
  geom_line(aes(y=new_r2), color = "red")+
  geom_line(aes(y=as.numeric(R2)), color = "blue")
####END BETTER ONE
ggplot(ResultSummary)+
geom_jitter(aes(x=new_r2, y = as.numeric(R2)))
ResultSummary$StopIdea<-ResultSummary$new_r2-as.numeric(ResultSummary$R2)
ResultSummary$AICDelta<-ResultSummary$AIC - lag(ResultSummary$AIC,default = 1000000)

#Get the value where the new_r2 diminishes for 3 consecutive iterations






















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



###Code that can be reused

```{r }
glm.M1<-glm(SalePrice~.,
            family=Gamma(link = "log"),
            data    = T_Train[,1:67])

with(summary(glm.M1), 1 - deviance/null.deviance)
format( pR2(glm.M1), scientific = FALSE)
summary(glm.M1)

step_gamma_glm<-step(glm.M1, direction="backwards")

step_gamma_glm$aic
step_gamma_glm$anova
step_gamma_glm$finalModel

x<-step_gamma_glm$model
yhat<-step_gamma_glm$fitted.values
y<-T_Train$SalePrice
fitted<-as.data.frame(cbind(yhat,y))

finalModel<-glm(formula = SalePrice ~ MSZoning + LotArea + Street + LandContour + 
                  Utilities + LotConfig + LandSlope + Neighborhood + Condition1 + 
                  Condition2 + BldgType + OverallQual + OverallCond + YearBuilt + 
                  YearRemodAdd + RoofMatl + Exterior1st + MasVnrArea + ExterCond + 
                  Foundation + BsmtExposure + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + 
                  Heating + HeatingQC + CentralAir + X1stFlrSF + X2ndFlrSF + 
                  LowQualFinSF + BsmtFullBath + FullBath + HalfBath + KitchenAbvGr + 
                  KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + GarageYrBlt + 
                  GarageCars + GarageArea + GarageQual + GarageCond + WoodDeckSF + 
                  EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + PoolQC + 
                  Fence + SaleType + SaleCondition, family = Gamma(link = "log"), 
                data = T_Train)
summary(finalModel)



valResult<-predict(finalModel,T_Val)

ggplot(fitted, aes(x=yhat, y=y))+
  geom_jitter()+
  geom_line(aes(x=y, y=y))

mse
```


```{r}
glm.K<-glm(SalePrice~1, family = Gamma(link="log"), data = T_Train)
summary(glm.K)
glm.All<-glm(SalePrice~., family = Gamma(link="log"), data = T_Train)
summary(glm.All)
glm.All$aic

#ActualPredicted set
ap<-as.data.frame(cbind("Actual" = T_Train$SalePrice, "Predicted_K" = glm.K$fitted.values))


ggplot(ap, aes(x = Actual, y=Predicted_K)) + geom_point()


step_gamma_glm<-step(glm.K,
                     direction = "forward",
                     scope = formula(glm.All),
                     trace=0
)
step_gamma_glm$aic
step_gamma_glm$anova
summary(step_gamma_glm)

##Below only works for numerical values???


```

```{r linseqmod}
#library(ggcorrplot) didn't use this in the end, corr sign is too strict
#install.packages("AutoStepwiseGLM") only works for gaussian identity. No good
#install.packages("leaps")
library(leaps)
#HERE WE DO THE MANUAL LOGY USING LM

lm.K<-lm(LogY~1,data = T_Train[,2:68] )

lm.Full<-lm(LogY~.,data = T_Train[,2:68])

lm.models <- regsubsets(LogY~., 
                        data = T_Train[,2:68],
                        nvmax = 5,
                        method = "forward")


summary(lm.models)

set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(LogY ~., data = T_Train[,2:68],
                    method = "leapSeq", 
                    tuneGrid = data.frame(nvmax = 1:260),
                    trControl = train.control
)
step.model$bestTune
step.model$results
step.model$finalModel
coef(step.model$finalModel, 67)

```
sub2<-select(data_fg,c("X1stFlrSF", "X2ndFlrSF", "LotArea", "GardenSF", "SalePrice", ))

sub2$mc<-pmax(data_fg$X1stFlrSF, data_fg$X2ndFlrSF)+
sub2$GardenSF<-sub2$LotArea - sub2$mc
ggplot(sub2, aes(x=GardenSF, y = SalePrice))+geom_jitter()
