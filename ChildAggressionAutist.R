childAggression<-read.delim("ChildAggression.dat", sep=" ", header = TRUE)
childAggression2<-lm(Aggression~Parenting_Style+Computer_Games+Diet+Sibling_Aggression, data = childAggression)
summary(childAggression2)
childAggression$standard.residuals<-rstandard(childAggression2)
childAggression$standard.residuals>3.29 | childAggression$standard.residuals<(-3.29)
childAggression$large.residual<-childAggression$standard.residuals>3.29|childAggression$standard.residuals<(-3.29)
sum(childAggression$large.residual)
childAggression[childAggression$large.residual, c("Parenting_Style", "Diet", "Computer_Games", "Sibling_Aggression", "standard.residuals")]
childAggression$medium.residual<-childAggression$standard.residuals>2.58 | childAggression$standard.residuals<(-2.58)
sum(childAggression$medium.residual/666)
childAggression$cooks.distance<-cooks.distance(childAggression2)
childAggression2$cookdis<-childAggression2$cooks.distance>1
sum(childAggression2$cooksdis)

