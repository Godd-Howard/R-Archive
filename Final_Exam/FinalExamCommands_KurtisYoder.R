Final Exam Commands:

1.
Hpain<-read.csv("HairPain.csv", header = TRUE)
Hpain$Hcolor<-gl(4, 5, 19, labels = c("Light Blond", "Dark Blond", "Light Brunette", "Dark Brunette"))
then fixed subject 15 with data editor
HpainModel<-ggplot(Hpain, aes(Hcolor, PainScore))
HpainModel + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = 1), colour = "Blue", linetype = "solid") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Hair Color", y = "Pain Tolerance")
by(Hpain$PainScore, Hpain$Hcolor, stat.desc, basic = FALSE, norm = TRUE)
leveneTest(Hpain$PainScore, Hpain$Hcolor, center = median)
Hmodel<-aov(PainScore~Hcolor, data = Hpain)
summary(Hmodel)
contrast1<-c(-1,-1,1,1)
contrast2<-c(-1,1,0,0)
contrast3<-c(0,0,-1,1)
contrasts(Hpain$Hcolor)<-cbind(contrast1, contrast2, contrast3)
Hpain$Hcolor
HCmodel<-aov(PainScore~Hcolor, data = Hpain)
summary.lm(HCmodel)


2.
PracData<-read.csv("PracExam.csv", header = TRUE)
PracBoxplot<-ggplot(PracData, aes(Standing, Helpfullness))
PracBoxplot + geom_boxplot() + labs(x="Class Standing", y = "Helpfullness for Exam") + facet_wrap( ~ ExamPrep)
by(PracData$Helpfullness, list(PracData$Standing, PracData$ExamPrep), stat.desc, basic = TRUE, norm = TRUE)
leveneTest(PracData$Helpfullness, interaction(PracData$Standing, PracData$ExamPrep), center = median)
PracModel<-aov(Helpfullness ~ Standing*ExamPrep, data = PracData)
summary(PracModel)
PracBar <- ggplot(PracData, aes(Standing, Helpfullness))
PracBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) + labs(x = "Class Standing", y = "Helpfullness Amount") 
PracBar <- ggplot(PracData, aes(ExamPrep, Helpfullness))
PracBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) + labs(x = "Exam Prep", y = "Helpfullness Amount")
linePrac<-ggplot(PracData, aes(Standing, Helpfullness, colour = ExamPrep))
linePrac + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group = ExamPrep)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Class Standing", y = "Helpfulness Amount", colour = "Exam Prep")

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

6.
SanData<-read.csv("Santa.csv", header = TRUE)
Freshmen <- c(490, 50, 13)
Sophomore <- c(390, 37, 7 )
Junior <- c(360, 40, 14)
Senior <- c(460, 55, 27)
SanTable <- cbind(Freshmen, Sophomore, Junior, Senior)
CrossTable(SanTable, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS") 
mosaicplot(SanTable, shade = TRUE, main = "Title")

7.
AutoData<-read.csv("AutoInj.csv", header = TRUE)
AutoData$dummyMarried<-ifelse(AutoData$MARITAL == 0, 1, 0)
AutoData$dummyWidowed<-ifelse(AutoData$MARITAL == 2, 1, 0)
AutoData$dummyDivorced<-ifelse(AutoData$MARITAL == 3, 1, 0)
attor<-glm(ATTORNEY~LOSS+GENDER+SEATBELT+dummyMarried+dummyWidowed+dummyDivorced, data = AutoData, family = binomial())
summary(attor)
modelChi <- attor$null.deviance - attor$deviance
chidf <- attor$df.null - attor$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
chisq.prob
vif(attor)
1/vif(attor)
mean(vif(attor))
VIF values
exp(attor$coefficients)


8.
AutoData<-read.csv("AutoInj.csv", header = TRUE)
AutoData$ATTORNEY<-ifelse(AutoData$ATTORNEY == 1, "Has Attorney", "No Attorney")
AutoData$GENDER<-ifelse(AutoData$GENDER == 1, "Female", "Male")
AutoData$SEATBELT<-ifelse(AutoData$SEATBELT == 1, "Wore Seatbelt", "No Seatbelt")
AutoTable<-xtabs(~ ATTORNEY + GENDER + SEATBELT, data = AutoData)
CrossTable(AutoTable, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
AtableSaturated<-loglm(~ ATTORNEY*GENDER*SEATBELT, data = AutoTable)
summary(AtableSaturated)
threeWay<-loglm(~ ATTORNEY + GENDER + SEATBELT + ATTORNEY:GENDER + ATTORNEY:SEATBELT + SEATBELT:GENDER, data = AutoTable)
summary(threeWay)
anova(AtableSaturated, threeWay)
ATTORNEYGENDER<-update(threeWay, .~. -ATTORNEY:GENDER)
anova(threeWay, ATTORNEYGENDER)
ATTORNEYGENDER<-update(threeWay, .~. -ATTORNEY:SEATBELT)
anova(threeWay, ATTORNEYGENDER)
GENDERSEATBELT<-update(threeWay, .~. -GENDER:SEATBELT)
anova(threeWay, GENDERSEATBELT)
AttGenTable<-xtabs(~ ATTORNEY + GENDER, data = AutoData)
CrossTable(AttGenTable, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
GenSeatTable<-xtabs(~ GENDER + SEATBELT, data = AutoData)
CrossTable(GenSeatTable, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
AutoFinal<-loglm(~ ATTORNEY + GENDER + SEATBELT + ATTORNEY:GENDER + SEATBELT:GENDER, data = AutoTable)
summary(AutoFinal)
mosaicplot(AttGenTable, shade = TRUE)
mosaicplot(GenSeatTable, shade = TRUE)

