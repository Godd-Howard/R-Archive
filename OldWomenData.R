InjS<-read.csv("StrengthInjury.csv", header = TRUE)
cor.test(InjS$injury, InjS$medindex, method = "pearson")
shapiro.test(InjS$injury)
shapiro.test(InjS$medindex)
cor.test(InjS$injury, InjS$age, method = "spearman")
shapiro.test(InjS$age)
library(polycor)
InjS$recodeAge<-ifelse(InjS$age<70, 0, 1)
cor.test(InjS$injury, InjS$recodeAge)    ///p=.1478, cor = .1458
ageFreq<-table(InjS$recodeAge)
prop.table(ageFreq)
polyserial(InjS$injury, InjS$recodeAge)
InjS2<-InjS[, c("injury", "medindex", "age")]
pcor(c("injury", "medindex", "age"), var(InjS2))
pc<-pcor(c("injury", "medindex", "age"), var(InjS2))
pcor.test(pc, 1, 100)
InjandAfraid<-lm(injury ~ medindex, data = InjS)
summary(InjandAfraid)
InjOriginal5<-lm(injury ~ medindex + age + gluts, data = InjS)
summary(InjOriginal5)
InjOriginal6<-lm(injury ~ age + gluts, data = InjS)
summary(InjOriginal6)
InjS$standardRes<-rstandard(InjOriginal5)
InjS$mediumRes<-InjS$standardRes>2.58|InjS$standardRes<(-2.58)
sum(InjS$mediumRes)
InjS$largeRes<-InjS$standardRes>3.29|InjS$standardRes<(-3.29)
sum(InjS$largeRes)
InjS$smallRes<-InjS$standardRes>1.96|InjS$standardRes<(-1.96)
sum(InjS$smallRes)
InjS[InjS$smallRes, c("injury", "medindex", "age", "gluts", "smallRes", "standardRes")]
InjS$cookDis<-cooks.distance(InjOriginal5)
InjS$lever<-hatvalues(InjOriginal5)
InjS$bigC<-InjS$cookDis>1
sum(InjS$bigC)
InjS$Biglever<-InjS$lever>.12
sum(InjS$Biglever)
durbinWatsonTest(InjOriginal5)
vif(InjOriginal5)
1/vif(InjOriginal5)
mean(vif(InjOriginal5))
plot(InjOriginal5)
hist(InjS$standardRes)

  






