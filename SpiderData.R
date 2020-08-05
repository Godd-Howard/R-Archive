spiderL<-read.delim("spiderLong.dat", header=TRUE)
barplot<-ggplot(spiderL, aes(Group, Anxiety))
barplot+stat_summary(fun.y=mean, geom="bar", fill="White", colour="Black") + stat_summary(fun.data=mean_cl_normal, geom="errorbar", position=position_dodge(width=0.90), width = 0.2) +labs(x="Type of Stimulus", y="Anxiety")
spiderData<-read.delim("spiderWide.dat", header=TRUE)
spiderData$pMean<-(spiderData$picture+spiderData$real)/2
grandMean<-mean(c(spiderData$picture, spiderData$real))
spiderData$adjusted<-grandMean-spiderData$pMean
spiderData$picture_adj<-spiderData$picture+spiderData$adjusted
spiderData$real_adj<-spiderData$real+spiderData$adjusted
barplot2<-ggplot(spiderStacked, aes(ind, values))
levels(spiderStacked$ind)<-c("Picture","Real Spider") 
barplot2+stat_summary(fun.y=mean, geom="bar", fill="White", colour="Black") + stat_summary(fun.data=mean_cl_normal, geom="errorbar", position=position_dodge(width=0.90), width = 0.2) +labs(x="Type of Stimulus", y="Anxiety")
spiderStacked<-stack(spiderData, select=c("picture_adj","real_adj"))
PeniData$book<-factor(PeniData$book, levels=c(1:2), labels=c("Women Are From Bras, Men Are From Penis", "Marie Claire"))
barplotP<-ggplot(PeniData, aes(book, happy))
barplotP+stat_summary(fun.y=mean, geom="bar", fill="White", colour="Black") + stat_summary(fun.data=mean_cl_normal, geom="errorbar", position=position_dodge(width=0.90), width=0.2) + labs(x="Book Type", y="Amount of Happiness")
by(PeniData$happy, PeniData$book, stat.desc, basic = FALSE, norm = TRUE)
ind.t.test<-t.test(PeniData$happy~PeniData$book, data = PeniData)
dummyData<-read.delim("dummy.dat", header = TRUE)
Model<-lm(libido~dummy1 + dummy2, data = dummyData)
summary(Model)

MMdata<-read.csv("M&M.csv", header = TRUE)
MMdata$pMean<-(MMdata$OutMs + MMdata$WithMs)/2
grandMean<-mean(c(MMdata$OutMs, MMdata$WithMs))
MMdata$ADJ<-grandMean-MMdata$pMean
stat.desc(MMdata, basic = FALSE, norm = TRUE)