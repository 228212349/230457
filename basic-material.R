if(FALSE) 
{
	IPTW计算基线资料代码
}

library(tableone)
library(survey)

iptw$sex<-factor(iptw$sex)
iptw$number<-factor(iptw$number)
iptw$etiology<-factor(iptw$etiology)
iptw$cirrhosis<-factor(iptw$cirrhosis)
iptw$AFP<-factor(iptw$AFP)
iptw$response<-factor(iptw$response)
vars=c( "age","size","neu","plt","ALT","AST","AFP","creatine","INR")
psModel=glm(group~age+size+neu+plt+ALT+AST+AFP+creatine+INR,
family=binomial(link="logit"),
data=iptw)
iptw$ps=predict(psModel,type="response")
iptw$wt1=1/iptw$ps
iptw$wt0=1/(1-iptw$ps)
iptw$w<-ifelse(iptw$group=="1",iptw$wt1,iptw$wt0)
dataIPTW=svydesign(ids=~1,data=iptw,weights= ~w)
myVars=c( "age","sex","size","number","etiology","cirrhosis","neu","lymp","plt","TBIL","ALB","ALT","AST","AFP","creatine","INR","response")
catVars<-c( "sex","number","etiology","cirrhosis","response","AFP")
nonvar <- c("size","neu","lymp","plt","TBIL","ALB","ALT","AST","creatine","INR")
tab_IPTW=svyCreateTableOne(vars=myVars,
factorVars = catVars,
strata="group",
data=dataIPTW,
)

Table6<- print(tab_IPTW,
nonnormal = nonvar,
catDigits = 1,
contDigits = 2,
pDigits = 3,
showAllLevels=TRUE,
quote = FALSE,
noSpaces = TRUE,
printToggle = TRUE)
write.csv(Table6, file = "Table6.csv")

library(RISCA)
library(foreign)

fit.ipw<-ipw.survival(times=iptw$OS,
failures=iptw$status==1,
variable=iptw$group,
weights=iptw$w)

p3 <- ipw.log.rank(times=iptw$OS,
failures=iptw$status==1,
variable=iptw$group,
weights=iptw$w)
p3

fit.ipw<-ipw.survival(times=iptw$DFS,
failures=iptw$fufastatus==1,
variable=iptw$group,
weights=iptw$w)

p4<- ipw.log.rank(times=iptw$DFS,
failures=iptw$fufastatus==1,
variable=iptw$group,
weights=iptw$w)
p4

