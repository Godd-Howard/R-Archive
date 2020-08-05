mathStat<-read.csv("CaliRain.csv", header = TRUE)
mathStat2<-lm(statexam~mathtest+engtest+eng_gpa+math_gpa+othr_gpa, data = mathStat)
summary(mathStat2)
mathStat3<-lm(statexam~mathtest, data = mathStat)
summary(mathStat3)
mathStat4<-lm(statexam~mathtest+math_gpa, data = mathStat)
summary(mathStat4)

burnoutData<-read.delim("Burnout.dat", header= TRUE)
burnoutData$burnout<-relevel(burnoutData$burnout, "Not Burnt Out")
burnoutModel<-glm(burnout~loc + cope, data=burnoutData, family=binomial())
summary(burnoutModel)
burnoutData$standResiduals<-rstandard(burnoutModel)
burnoutData$large.res<-burnoutData$standResiduals>3.29|burnoutData$standResiduals<(-3.29)
sum(burnoutData$large.res)
burnoutData$med.res<-burnoutData$standResiduals>2.58|burnoutData$standResiduals<(-2.58)
sum(burnoutData$med.res)

mathStat$standardRes2<-rstandard(mathStat3)
mathStat$cookDis2<-cooks.distance(mathStat3)
mathStat$lever2<-hatvalues(mathStat3)
mathStat$coR2<-covratio(mathStat3)
mathStat$largeRes2<-mathStat$standardRes2>3.29|mathStat$standardRes2<(-3.29)
sum(mathStat$largeRes2)
mathStat$mediumRes2<-mathStat$standardRes2>2.58|mathStat$standardRes2<(-2.58)
sum(mathStat$mediumRes2)
mathStat$smallRes2<-mathStat$standardRes2>1.96|mathStat$standardRes2<(-1.96)
sum(mathStat$smallRes2)
//Student 12 and 92 are above small resid
mathStat[mathStat$smallRes2,c("statexam", "mathtest", "math_gpa", "engtest", "eng_gpa", "othr_gpa", "standardRes2")]
mathStat$bigC2<-mathStat$cookDis2>1
sum(mathStat$bigC2)
mathStat$Biglever2<-mathStat$lever2>.06
sum(mathStat$Biglever2)
mathStat[mathStat$Biglever2,c("statexam", "mathtest", "math_gpa", "engtest", "eng_gpa", "othr_gpa", "standardRes2", "cookDis2", "lever2")]
mathStat$phattCo<-mathStat$coR>1.18|mathStat$coR<.82
sum(mathStat$phattCo)
mathStat[mathStat$phattCo,c("statexam", "mathtest", "math_gpa", "engtest", "eng_gpa", "othr_gpa", "standardRes", "cookDis", "lever", "coR")]
durbinWatsonTest(mathStat2)
dwt(mathStat3)
vif(mathStat3)
1/vif(mathStat3)
mean(vif(mathStat3))
summary(mathStat3)
plot(mathStat2)
hist(mathStat$standardRes)
hist(mathStat$standardRes2)
plot(mathStat3)







autoCategory<-read.csv("AutoCategorical.csv", header = TRUE)
autoCategory$sixCylinder<-ifelse(autoCategory$cylinders == 6, 1, 0)
autoCategory$fourCylinder<-ifelse(autoCategory$cylinders == 4, 1, 0)
autoCategoryM<-lm(mpg~horsepower+weight+fourCylinder+sixCylinder, data = autoCategory)
summary(autoCategoryM)

superModel<-read.delim("SuperModel.dat", sep = " ", header = TRUE)
superMD<-lm(SALARY~BEAUTY+AGE+YEARS, data = superModel)
summary(superMD)
superMD2<-lm(SALARY~+AGE+YEARS, data = superModel)
summary(superMD2)

MMdata<-read.csv("M&M.csv", header = TRUE)
MMdata$pMean<-(MMdata$OutMs + MMdata$WithMs)/2
grandMean<-mean(c(MMdata$OutMs, MMdata$WithMs))
MMdata$ADJ<-grandMean-MMdata$pMean
MMdata$Out_ADJ<-MMdata$OutMs + MMdata$ADJ
MMdata$With_ADJ<-MMdata$WithMs + MMdata$ADJ
MMstacked<-stack(MMdata, select = c("Out_ADJ", "With_ADJ"))
levels(MMstacked$ind)<-c("Without M&M's","With M&M's")
barMaM<-ggplot(MMstacked, aes(ind, values))
barMaM + stat_summary(fun.y=mean, geom="bar", fill="White", colour="Black") + stat_summary(fun.data=mean_cl_normal, geom="errorbar", position=position_dodge(width=0.90), width = 0.2) +labs(x="M&M Or Not", y="Exam Scores")
stat.desc(MMdata, basic = FALSE, norm = TRUE)
dep.t.test<-t.test(MMdata$OutMs, MMdata$WithMs, paired = TRUE, conf.level = 0.99, alternative = "less")
dep.t.test<-t.test(MMdata$OutMs, MMdata$WithMs, paired = TRUE, conf.level = 0.99)
dep.t.test


MMdata2<-read.csv("M&M2008.csv", header = TRUE)
MM3stacked<-stack(MMdata2, select = c("Spring.2008", "Spring.2007"))
levels(MM3stacked$ind)<-c("Spring of 2008","Spring of 2007")
barMaM<-ggplot(MM3stacked, aes(ind, values))
barMaM + stat_summary(fun.y=mean, geom="bar", fill="White", colour="Black") + stat_summary(fun.data=mean_cl_normal, geom="errorbar", position=position_dodge(width=0.90), width = 0.2) +labs(x="Semester", y="Number of M&M's Taken")
stat.desc(MMdata2, basic = FALSE, norm = TRUE)
ind.t.test<-t.test(MMdata2$Spring.2008, MMdata2$Spring.2007, alternative = "less"))


3.
RazData<-read.csv("Razors.csv", header = TRUE)
RazData$diff <- RazData$Single.Blade-RazData$Twin.Blade
stat.desc(RazData, basic = FALSE, norm = TRUE)
dep.t.test<-t.test(RazData$Single.Blade, RazData$Twin.Blade, paired = TRUE, alternative = "less")
dep.t.test
dep.t.test<-t.test(RazData$Single.Blade, RazData$Twin.Blade, paired = TRUE,)
dep.t.test

4.
Rdata<-read.csv("ReindeerCombine.csv", header = TRUE)
changed name titles in data editor
Rdata$pMean<-(Rdata$t2007 + Rdata$t2017)/2
grandMean<-mean(c(Rdata$t2007, Rdata$t2017))
Rdata$ADJ<-grandMean-Rdata$pMean
Rdata$t2007_ADJ_ADJ<-Rdata$t2007 + Rdata$ADJ
Rdata$t2017_ADJ<-Rdata$t2017 + Rdata$ADJ
Rstacked<-stack(Rdata, select = c("t2007_ADJ_ADJ", "t2017_ADJ"))
levels(Rstacked$ind)<-c("2007 Time in Seconds","2017 Time in Seconds")
barR<-ggplot(Rstacked, aes(ind, values))
barR + stat_summary(fun.y=mean, geom="bar", fill="White", colour="Black") + stat_summary(fun.data=mean_cl_normal, geom="errorbar", position=position_dodge(width=0.90), width = 0.2) +labs(x="Year", y="1000-Mile Dash in Seconds")
Rdata$diff<-Rdata$t2007-Rdata$t2017
stat.desc(Rdata, basic = FALSE, norm = TRUE)
dep.t.test<-t.test(Rdata$t2007, Rdata$t2017, paired = TRUE, alternative = "less")
dep.t.test
dep.t.test<-t.test(Rdata$t2007, Rdata$t2017, paired = TRUE)
dep.t.test

5.
RainStat<-read.csv("CaliRain.csv", header = TRUE)
RainStat2<-lm(Precipitation~Altitude+Latitude+CoastDist, data = RainStat)
summary(RainStat2)
durbinWatsonTest(RainStat2)

