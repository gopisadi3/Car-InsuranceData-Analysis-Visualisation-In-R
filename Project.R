install.packages('insuranceData')
install.packages("rpart.plot")
install.packages('psych')
library(insuranceData)
dataCar
d =dataCar
head(d)
summary(d)
#plot 
scatter.smooth(x= d$numclaims, y= d$veh_age )
boxplot(d$exposure ~ d$veh_age)
hist(d$exposure)
scatter.smooth(x = d$numclaims, y = d$claimcst0)
table(d$gender)
table(d$veh_body)
#correlation mtrix
cor(d[c("exposure","veh_age","claimcst0","numclaims")])
pairs(d[c("exposure","veh_age","claimcst0","numclaims")])
library(psych)
pairs.panels(d[c("exposure","veh_age","claimcst0","numclaims")])
ssize <- floor(0.75 * nrow(d))
set.seed(123)
trainI <- sample(seq_len(nrow(d)), size = ssize)
train <- d[trainI, ]
test <- d[-trainI, ]
head(train)
head(test)
#Training a Model On Data
#LinearRegression
linear <- lm(exposure ~  veh_age , data = d)
summary(linear)
modelSummary <- summary(linear)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["d$veh_age", "Estimate"]  # get beta estimate veh_age
std.error <- modelCoeffs["veh_age", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(d)-ncol(d))  # calc p Value
f_statistic <- linear$fstatistic[1]  # fstatistic
f <- summary(linear)$fstatistic  # parameters for model p-value calc
t_value
p_value
f_statistic
f
model_p <- pf(f[1], f[2], f[3], lower=FALSE)
result <- predict(linear,a)
print(result)
#logistic
logt <- glm(d$veh_age ~ d$numclaims, family = binomial)
summary(logt)

pred <- predict.glm(veh_age,numclaims)
library(ggplot2)
ggplot(d, aes(exposure, claimcst0, color = veh_age)) + geom_point()
library(rpart.plot)
#fit <- rpart(numclaims ~ veh_age, data = train, method = 'class')
#rpart.plot(fit, extra = 8)
#kmeans clusterig
outliers <- boxplot(d$claimcst0, plot=FALSE)$out
outliers
set.seed(20)
car_c <- kmeans(d[, 2:5], 3, nstart = 20)
car_c
hist(d$numclaims)
barplot(d$veh_value,main = "Age", xlab = "Vech", col = c("red","green"))
legend("topleft", c("M","F"),fill =c("red","green"))