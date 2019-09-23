
churndata<-read.csv("Proactive-Attrition-Management-Case-Study.csv")
View(churndata)
names(churndata)

churndata["CHURNDEP"]=NULL
str(churndata["CUSTOMER"])  #customer id is a numeric variable.
churndata["CUSTOMER"]=as.character(churndata["CUSTOMER"])     #convert cust id to a character type

#look whether two categorical variables are dependent on each other or not
View(churndata[41:43])
View(churndata[34:40])  #we can delete one credit rating category because any customer necessarily has to belong to any one of these category
View(churndata[48:54])

names(churndata)
#MARRYUN,INCMISS and SETPRCM indicate missing marital status,income value and handset price respectively. Before converting these variables
# to facors, find out how many observations have missing marital status,incomes and handset price.
sum(churndata["MARRYUN"])
(sum(churndata["MARRYUN"])/nrow(churndata))*100  #38% obs. have missing marital status. It won't be appropriate to
                                                #impute. Better not to include this variable in the model    
sum(churndata["INCMISS"])
(sum(churndata["INCMISS"])/nrow(churndata))*100  #~25% obs have missing incomes. we will do mean value imputation

sum(churndata["SETPRCM"])
(sum(churndata["SETPRCM"])/nrow(churndata))*100  #56% obs have missing incomes. we will not include this variable 
                                                  #in the model

churndata2=churndata
names_factor=c("CHURN","CHILDREN","CREDITA","CREDITAA","CREDITB" ,"CREDITC"  ,"CREDITDE", "CREDITGY", "CREDITZ", 
 "PRIZMRUR" ,"PRIZMUB" , "PRIZMTWN", "REFURB"  , "WEBCAP" ,  "TRUCK" , "RV" ,  "OCCPROF" ,"OCCCLER","OCCCRFT",
 "OCCSTUD",  "OCCHMKR"  ,"OCCRET",   "OCCSELF",  "OWNRENT","MARRYUN" ,"MARRYYES",
 "MARRYNO",  "MAILORD"  ,"MAILRES","MAILFLAG","TRAVEL" ,"PCOWN","CREDITCD", "NEWCELLY","NEWCELLN",
 "INCMISS", "SETPRCM" ,"RETCALL" , "CALIBRAT")
churndata2[,names_factor] <- lapply(churndata2[,names_factor] , as.factor)
str(churndata2[names_factor])


num_var=names(churndata2[sapply(churndata2,function(x) is.numeric(x))])
num_var


stats <- function(x) {
  n=nrow(x)
  a <- x[!is.na(x)]
  nmiss<-sum(is.na(x))
  m <- mean(a)
  std <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  median<-quantile(a,0.5)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*std
  LC <- m-3*std
  outlier_flag<- max>UC | min<LC
  return(c(total_obs=n,nmiss=nmiss, mean=m, stdev=std,min = min, p1=p1,p5=p5, median=median,p95=p95,p99=p99,max=max, UC=UC, LC=LC, outlier_flag=outlier_flag))
}
var_audit_report=data.frame(t(apply(churndata2[num_var],2,stats)))
var_audit_report
write.csv(var_audit_report,file="data audit rep.csv") #write the data audit report to a file


#Outlier Treatment

#Based on the data audit report, we will have to apply: 
#1.upper cut off of mean+3(std) on some variables
#2. lower cut off of mean-3(std) on some variables
#3. and on the whole number valued variables, we will apply cut off at 99 percentile
#4 On one variable we will apply lower cut off of 1pctl


out_up=c("REVENUE", 
         "MOU", 
         "RECCHRGE", 
         "DIRECTAS", 
         "OVERAGE", 
         "ROAM", 
         "CHANGEM", 
         "CHANGER", 
         "DROPVCE", 
         "BLCKVCE", 
         "UNANSVCE", 
         "CUSTCARE", 
         "MOUREC", 
         "OUTCALLS", 
         "INCALLS", 
         "PEAKVCE", 
         "OPEAKVCE", 
         "DROPBLK", 
         "CALLFWDV", 
         "CALLWAIT", 
         "SETPRC", 
         "THREEWAY")

out_99=c("MONTHS", 
         "UNIQSUBS", 
         "ACTVSUBS", 
         "PHONES", 
         "MODELS", 
         "EQPDAYS", 
         "AGE1", 
         "AGE2", 
         "RETCALLS", 
         "RETACCPT", 
         "REFER", 
         "INCOME", 
         "CREDITAD")

churndata2$CHANGEM[churndata2$CHANGEM< -776.7894079] =-776.7894079  #mean-3(std)
churndata2$CHANGER[churndata2$CHANGEr< -117.5180117] =-117.5180117

churndata2$EQPDAYS[churndata2$EQPDAYS<7] =7  #1 pctl


cap_99= function(x) {
  uc=quantile(x,probs=0.99,na.rm=TRUE)
  x[x>uc]=uc
  return(x)
}

cap_3std=function(x){
  uc=mean(x,na.rm=T)+3*(sd(x,na.rm=T))
  x[x>uc]=uc
  return(x)
}



churndata2[out_99]=apply(churndata2[out_99],2,cap_99)
churndata2[out_up]=apply(churndata2[out_up],2,cap_3std)


#missing value treatment
#see which variables have missing values in the data audit report


nmiss=function(x){  #function to find number of missing values in each variable of the dataset 
  nmiss=sum(is.na(x))
  return(nmiss=nmiss)
}
apply(churndata2,2,nmiss)
#These values have missing values:
#REVENUE MOU RECCHRGE DIRECTAS OVERAGE ROAM CHANGEM CHANGER AGE1 AGE2(columns: 1,2,3,4,5,6,7,8,31,32)
miss1=names(churndata2[c(1:8)])
miss2=names(churndata2[c(31:32)])
miss2


n_missing=apply(churndata2,2,nmiss)
n_missing #There is only missing value in each in the categorical vars: PHONES, MODELS and EQPDAYS.
          #So,delete these observations (at a later stage)

mean_imp1=function(x){ #mean imputation function
  m=mean(x,na.rm=T)
  x[is.na(x)]=m
  return(x)
}


mean_imp2=function(x){ #mean imputation function (additional floor function so the values of AGE1 and AGE2 remain integer)
  m=floor(mean(x,na.rm=T))
  x[is.na(x)]=m
  return(x)}


churndata2[miss1]=apply(churndata2[miss1],2,mean_imp1)
churndata2[miss2]=apply(churndata2[miss2],2,mean_imp2)

apply(churndata2,2,nmiss)

churndata2=na.omit(churndata2)  #There is one incomplete case (missing in PHONES, MODELS and EQPDAYS). Delete it.
m=mean(churndata2$INCOME,na.rm=T)
m
View(churndata2["INCOME"])
churndata2$INCOME[churndata2$INCMISS==1] =m  #mean value imputation
apply(churndata2,2,nmiss)  #There are no missing values in the data now


#correlation matrix

cor_matrix=data.frame(cor(churndata2[num_var]))
View(cor_matrix)
write.csv(cor_matrix,file="corr.csv")


#data splitting
sample_obs=sample(1:nrow(churndata2),size=floor(nrow(churndata2)*0.7))
dev=churndata2[sample_obs,]
val=churndata2[-sample_obs,]

#Select variables based on business understanding
dev2=dev #In dev2 keep only the variables which want to use in the model building

levels(as.factor(churndata2$CSA))  #774 levels. Too many dummy variables will have to be formed 
                                    #and glm will run for too long

#Out of the 7 variables indicating credit rating, delete any one.(Dummy variables=1-(no. of categories or levels))

#Keep all the three variables PRIZMRUR, PRIZMUB and PRIZMTWN because there are cust. that dont belong to any one of the three

#Also keep all the occupation related variables.

#As mentioned before, marital status and handset price were missing for a large proportion of cust, so better to avoid
    #these variables(MARRYUN, MARRYES,MARRYNO,SETPRC,SETPRCM) in the model building

#Out of NEWCELLN and NEWCELLN, keep only one.

#RETCALL(0/1) and RETCALLS(no of calls made to retention team) basically convey the same infor.
    #pick only one i.e. RETCALLS

#See correlation matrix, some variables have very correlation, these might cause multicollinearity
#Don't delete them just yet. First run the model



novars <- names(dev2) %in% c("CSA","CUSTOMER","CREDITA", "MARRYUN", "MARRYYES","MARRYNO","SETPRCM","SETPRC","NEWCELLN"
                                   ,"RETCALL","CALIBRAT","INCMISS") 
dev2 <- dev2[!novars]


f=sapply(dev2,is.factor)
nf=names(dev2[,f])
str(dev2[nf])


fit<-glm(CHURN~.,data = dev2,family = binomial(logit))
summary(fit)
Concordance(fit)
require(car)
vif(fit)
step1=step(fit) #takes too long to run. But it does yield results

fit2=glm(CHURN ~ REVENUE + MOU + RECCHRGE + OVERAGE + ROAM + CHANGEM + 
           CHANGER + DROPVCE + UNANSVCE + CUSTCARE + THREEWAY + PEAKVCE + 
           DROPBLK + MONTHS + UNIQSUBS + ACTVSUBS + PHONES + EQPDAYS + 
           AGE1 + CHILDREN + CREDITAA + CREDITB + CREDITC + CREDITDE + 
           PRIZMRUR + PRIZMTWN + REFURB + WEBCAP + OCCSTUD + MAILRES + 
           TRAVEL + RETCALLS + RETACCPT + REFER + CREDITAD,data=dev2,family=binomial(logit))
Concordance(fit2) #62.28 % concordance
summary(fit2)
require(car)
vif(fit2)  #some have very high vifs (>2)
            #REMOVE: REVENUE MOU ACTVQSUBS UNANSVCE DROPBLK
fit3=glm(CHURN ~ RECCHRGE+ OVERAGE + ROAM + CHANGEM + 
           CHANGER + DROPVCE + CUSTCARE + THREEWAY + PEAKVCE+ 
           MONTHS + UNIQSUBS + PHONES + EQPDAYS + 
           AGE1 + CHILDREN + CREDITAA + CREDITB + CREDITC + CREDITDE + 
           PRIZMRUR + PRIZMTWN + REFURB + WEBCAP + OCCSTUD + MAILRES + 
           TRAVEL + RETCALLS + RETACCPT + REFER + CREDITAD,data=dev2,family=binomial(logit))  
summary(fit3)
vifs=vif(fit3)
concord=Concordance(fit3)
write.csv(concord,"Concordance.csv")
write.csv(vifs,"vifs.csv")
#fit3 is the final model

#calculate beta coefficients
require(QuantPsyc)
stb_dev=lm.beta(fit3)
stb_dev
write.csv(stb_dev,file="stb dev.csv")


#export model summary
str(fit3)
model_sum_dev=summary(fit3)$coef
model_sum_dev
write.csv(model_sum_dev,file="model summary dev.csv")






#validation------------
#VALIDATION 1
dev_final= cbind(dev,Prob=predict(fit3,type="response"))
View(dev_final["Prob"])

#create deciles
dec_cuts=quantile(dev_final$Prob,probs=seq(from=0.1, to=0.9, by=0.1))
dev_final$dec=findInterval(dev_final$Prob,c(-Inf,dec_cuts,Inf))
View(dev_final["dec"])
require(dplyr)

dev_final$dec=as.factor(dev_final$dec)

dev_final$churn_num=as.numeric(as.character(dev_final$CHURN)) #In order to sum CHURN (in the following code),
                                                              #it should be numeric not factor.So make new churn variable 
                                                              #that is numeric and can be summed
View(dev_final["churn_num"])




sum(dev_final$churn_num)
count(dev_final["churn_num"])
sumd=dev_final %>% group_by(dec) %>% summarise(count=n(),min_prob=min(Prob),
                                               max_prob=max(Prob),churn_cnt=sum(churn_num),
                                               non_churn_cnt=count-churn_cnt) %>% arrange(desc(dec))
sumd
write.csv(sumd,file="decile_dev.csv")


#validation of val set
val_final= cbind(val,Prob=predict(fit3,val,type="response"))
View(val_final["Prob"])
View(val_final["CHURN"])

dec_cuts=quantile(val_final$Prob,probs=seq(from=0.1, to=0.9, by=0.1))
val_final$dec=findInterval(val_final$Prob,c(-Inf,dec_cuts,Inf))
View(val_final["dec"])
require(dplyr)

val_final$dec=as.factor(val_final$dec)

val_final$churn_num=as.numeric(as.character(val_final$CHURN)) #In order to sum CHURN (in the following code),
#it should be numeric not factor.So make new variable that is numeric and can be summed
View(val_final["churn_num"])




sum(val_final$churn_num)
count(val_final["churn_num"])
sum=val_final %>% group_by(dec) %>% summarise(count=n(),min_prob=min(Prob),
                                               max_prob=max(Prob),churn_cnt=sum(churn_num),
                                               non_churn_cnt=count-churn_cnt) %>% arrange(desc(dec))
sum
write.csv(sum,file="decile_val.csv")

#VALIDATION 2
fit3_val=glm(CHURN ~ RECCHRGE+ OVERAGE + ROAM + CHANGEM + 
                    CHANGER + DROPVCE + CUSTCARE + THREEWAY + PEAKVCE+ 
                    MONTHS + UNIQSUBS + PHONES + EQPDAYS + 
                    AGE1 + CHILDREN + CREDITAA + CREDITB + CREDITC + CREDITDE + 
                    PRIZMRUR + PRIZMTWN + REFURB + WEBCAP + OCCSTUD + MAILRES + 
                    TRAVEL + RETCALLS + RETACCPT + REFER + CREDITAD,data=val,family=binomial(logit))  
summary(fit3)
vifs_val=vif(fit3)
concord_val=Concordance(fit3)
stb_val=lm.beta(fit3_val)
stb_val
write.csv(stb_val,file="stb val.csv")

#Calculate AUC
require(ROCR)
pred_dev_fit3 <- prediction(dev_final$Prob, dev_final$CHURN)
perf_fit3 <- performance(pred_dev_fit3, "tpr", "fpr")
plot(perf_fit3)
abline(0, 1)
performance(pred_dev_fit3, "auc")@y.values

#Find tpr and fpr at different cutoffs
str(perf_fit3)
cutoffs <- data.frame(cut=perf_fit3@alpha.values[[1]], fpr=perf_fit3@x.values[[1]], 
                      tpr=perf_fit3@y.values[[1]])
View(cutoffs)
write.csv(cutoffs,"cutoffs.csv")
