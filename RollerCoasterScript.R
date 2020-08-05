library(ggplot2)
library(Hmisc)
rollercoasterData<-read.csv("C:/Users/kurti/OneDrive/Documents/R/RollerCoasterData.csv", header = TRUE)

rollercoasterBoxplot<-ggplot(rollercoasterData, aes(Type, Height.ft.))
rollercoasterBoxplot + geom_boxplot() + labs(x = "Type of Roller Coaster", y = "Height in ft")

bar<-ggplot(rollercoasterData, aes(Park, Drop.ft.))
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + labs(x = "Park", y = "Drop in ft")

clbar<-ggplot(rollercoasterData, aes(Park, Speed.mph., fill = Type))
clbar + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + labs(x = "Park", y = "Speed in mph", fill = "Type of Roller Coaster")

scattercoaster<-ggplot(rollercoasterData, aes(Speed.mph., Height.ft.))
scattercoaster + geom_point() + geom_smooth(method = "lm", colour = "Blue") + labs(x="Speed of Roller Coaster in mph", y="Height of Roller Coaster in ft")

scatterCLcoaster<-ggplot(rollercoasterData, aes(Speed.mph., Height.ft., colour=Type))
scatterCLcoaster + geom_point() + geom_smooth(method = "lm",alpha=0.1) + labs(x="Speed of Roller Coaster in mph", y="Height of Roller Coaster in ft", colour="Type of Roller Coaster")

coasterHistogram<-ggplot(rollercoasterData, aes(Speed.mph.))
coasterHistogram + geom_histogram(binwidth=7) + labs(x="Speed of Roller Coaster in mph",y="Frequency")

attitudeData<-read.csv("C:/Users/kurti/OneDrive/Documents/R/BeerRatings.csv", header = TRUE)
barErrorAttit<-ggplot(attitudeData, aes(Beer,Rating,))
barErrorAttit+stat_summary(fun.y=mean, geom="bar", fill="White", colour="Black")+stat_summary(fun.data=mean_cl_normal, geom="errorbar", position=position_dodge(width=0.90),width=0.2) + labs(x = "Beer Advertisement", y = "Beer Rating")
