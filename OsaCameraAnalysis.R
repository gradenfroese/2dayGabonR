# Osa camera analysis
# 12/13/18

library(lubridate)
read.csv()
?lubridate

effort <- effort[effort$Status=="working",]

table(effort$Status)

colnames(covariates)


plot(count.mat$agouti, count.mat$ocelot)
plot(count.mat$agouti, count.mat$paca)

plot(count.sp$Y~count.sp$X, pch=19, cex=0.5, las=1, ylab="Lat", xlab="Long", main="Striped Skunk", asp=1)
points(count.sp$Y[count.sp$Code=="skunk_striped" & count.sp$Count>0 ]~count.sp$X[count.sp$Code=="skunk_striped"& count.sp$Count>0 ], pch=19, cex=1, col=rgb(1,0.2,0.4,0.8))


plot(count.sp$Count[count.sp$Code=="agouti"]~count.sp$river_dist[count.sp$Code=="agouti"], pch=19, cex=0.5, las=1, ylab="Count", xlab="River Distance", main="Agouti")


plot(count.mat$agouti, count.mat$puma, ylab="Puma Count", xlab="Agouti Count", las=1)

summary(lm(count.mat$agouti~ count.mat$puma))
table(data$Code)

pairs(count.mat[2:ncol(count.mat)])







table(count.sp$Code)

boxplot(count.sp$RAI[count.sp$Code=="agouti"]~count.sp$habitat_type[count.sp$Code=="agouti"], main="Agouti", las=1, ylab="Count")

boxplot(count.sp$RAI[count.sp$Code=="coati"]~count.sp$habitat_type[count.sp$Code=="coati"])

plot(count.sp$RAI[count.sp$Code=="coati"]~count.sp$ocean_dist[count.sp$Code=="coati"])

boxplot(count.sp$RAI[count.sp$Code=="armadillo_ninebanded"]~count.sp$habitat_type[count.sp$Code=="armadillo_ninebanded"])
boxplot(count.sp$RAI[count.sp$Code=="paca"]~count.sp$habitat_type[count.sp$Code=="paca"])
boxplot(count.sp$RAI[count.sp$Code=="peccary_whitelipped"]~count.sp$habitat_type[count.sp$Code=="peccary_whitelipped"])
boxplot(count.sp$RAI[count.sp$Code=="opossum_common"]~count.sp$habitat_type[count.sp$Code=="opossum_common"])

boxplot(count.sp$RAI[count.sp$Code=="puma"]~count.sp$habitat_type[count.sp$Code=="puma"])

boxplot(count.sp$RAI[count.sp$Code=="puma"]~count.sp$habitat_type[count.sp$Code=="puma"])

boxplot(count.sp$RAI[count.sp$Code=="peccary_collared"]~count.sp$habitat_type[count.sp$Code=="peccary_collared"])
boxplot(count.sp$RAI[count.sp$Code=="tamandua"]~count.sp$habitat_type[count.sp$Code=="tamandua"])


############################
# Subset count.sp to just have coatis

coati <- count.sp[count.sp$Code=="coati",]









#########################

par(mfrow=c(1,1))
plot(coati$RAI~coati$ocean_dist, pch=19, col="pink", ylab="RAI", xlab= "Ocean Distance (m)", las=1)














m1 <- lm(RAI~ocean_dist, data=coati)
summary(m1)










coef(m1)
abline(coef(m1), col="red")




par(mfrow=c(2,2))
plot(m1)







################
m2 <- lm(RAI~ocean_dist + I(ocean_dist^2), data=coati)
summary(m2)






plot(m2)

par(mfrow=c(2,2))
plot(m2)

par(mfrow=c(1,1))
plot(coati$RAI~coati$ocean_dist, pch=19, col="pink", ylab="RAI", xlab= "Ocean Distance (m)", las=1)
lines(predict.lm(m2)[order(coati$ocean_dist)]~ coati$ocean_dist[order(coati$ocean_dist)], type="l", col="red")

#################3
AIC(m1,m2)



###############
m3 <- lm(RAI ~ ocean_dist+ I(ocean_dist^2) + habitat_type, data=coati)
summary(m3)

AIC(m1,m2,m3)
head(coati)
m4 <- lm(RAI ~ ocean_dist+ I(ocean_dist^2) + trail , data=coati)
summary(m4)
AIC(m1,m2,m3,m4)
