### INSTALL PACKAGES.

install.packages("car")
install.packages("plotly")
install.packages("knitr")
install.packages("gmodels")
library("car")
library("plotly")
library("knitr")
library("gmodels")

### CALL DATA  - csv file name: Result.csv
data <-read.csv(file = "Result.csv" , header = TRUE , sep = ";")
data2var <-read.csv(file = "Result2var.csv" , header = TRUE , sep = ";")
attach(data)
names(data)
data.subset <- subset(data , select = c(HBA1C , TSH , T4 ,T3 ,THYR))

### UNDERSTANDIND SAMPLE.
X<- table(T.G > 200)
prop.table(X)
Y <- table( CHOLE > 200)
prop.table(Y)
Z <- table(HBA1C > 6.5)
prop.table(Z)

### MEAN - Q1, Q3 - MINIMUM AND MAXIMUM VALUE - SD
summary(data)
sapply(data[,1:9], sd )

## ANALYSIS OF FREQUENCY
FQ <- with(data,table(GENDER,THYR , p ))
FQ
prop.table(FQ)

## T TEST, PEARSON 
Cor <- t.test(data2var$HBA1C~data2var$THYR ,mu=0, alt="two.sided" ,var.equa=TRUE) 
> Cor

## ANOVA
aovresults <- aov(data$HBA1C ~ data$TSHLevel * data$T4Level * data$T3Level, data = data)
anova(aovresults)
aggregate(HBA1C ~ TSHLevel + T4Level + T3Level, FUN = "mean", data = data)
aggregate(HBA1C ~ TSHLevel + T4Level + T3Level, FUN = "sd", data = data)
#Anova Plotting 
par(mfrow=c(2,2))
Plot(aovresults)



### PLOTTING 
boxplot(HBA1C~THYR)

par(mfrow=c(1,3))
plot(HBA1C , TSH , xlab = "HBA1C" , ylab = "TSH" , main = "Scatterplot of HBA1C vs TSH" , abline(lm(HBA1C ~ TSH)) )
plot(HBA1C , T4 , xlab = "HBA1C" , ylab = "T4" , main = "Scatterplot of HBA1C vs T4" , abline(lm(HBA1C ~ T4)) )
plot(HBA1C , T3 , xlab = "HBA1C" , ylab = "T3" , main = "Scatterplot of HBA1C vs T3" , abline(lm(HBA1C ~ T3)) )

par(mfrow=c(2,2))
Plot(aovresults)
scatterplot(sara2var$HBA1C ~ sara2var$TSH | sara2var$THYR , data=sara2var, 
            xlab="HBA1C", ylab="TSH", 
            main="Enhanced Scatter Plot", 
            labels=row.names(sara2var$THYR))
