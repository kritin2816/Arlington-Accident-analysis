
#DATASET 1 - CODE:
#installing and importing the required packages
install.packages("psych")
library(psych)
install.packages("tidyr")
library(tidyr)
getwd()
#reading the CSV file into Rstudio and storing it in a variable
data1 <- read.csv("C:/Users/kriti/Downloads/PROJECT DATASETS - Sheet1
(2).csv", header=T, sep =",")
data2 = data.frame(data1 )#Creating a data frame
View(data2) #used to view the data frame
#using is.na function to check for any null values
is.na(data2)
#Using the function summary and describe to represent descriptive statistics
summary(data2)
describe(data2)
#create a boxplot for the entire data frame with outliers
boxplot(data2$Number.of.lanes,data2$Number.of.Vehicles,data2$Accidents)
#dropping the outliers
Q1 <- quantile(data2$Number.of.Vehicles, .25)
Q3 <- quantile(data2$Number.of.Vehicles, .75)
IQR <- IQR(data2$Number.of.Vehicles)
#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
ndata2 <- subset(data2, data2$Number.of.Vehicles > (Q1 - 1.5*IQR) &
                   data2$Number.of.Vehicles< (Q3 + 1.5*IQR))
View(ndata2)
#view row and column count of new data frame
dim(ndata2) #Orginal dataframe had 199 values after removing outliers dataframe has
#187 values
#create a boxplot for the entire data frame without outliers
boxplot(ndata2$Number.of.lanes,ndata2$Number.of.Vehicles,ndata2$Accidents)
#Using the function summary and describe to represent descriptive statistics
summary(ndata2)
describe(ndata2)
#Continuous random variable (Number of Vehicles)
summary(ndata2)
describe(ndata2$Number.of.Vehicles) #Finding descriptive statistics
# Finding Coefficient of Variance using Standard Deviation and Variance
s <-
  var(ndata2$Number.of.Vehicles) s
l <-
  sd(ndata2$Number.of.Vehicles) l
cv <- s/l
cv
# Finding mode by creating a function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mo <-
  getmode(ndata2$Number.of.Vehicles) mo
#Exponential Mean of the continuous variable
gm1 <-
  exp(mean(log(ndata2$Number.of.Vehicles))) gm1
#Geometric mean of the continuous variable
gm2 <-
  geometric.mean(ndata2$Number.of.Vehicles) gm2
#install packages to do the groupby function
install.packages("dplyr")
library(dplyr)
#TIME OF DAY
#Calculate descriptive statistics of 'Time of Day' by 'Number of Vehicles'
tod <- ndata2 %>%
  group_by(Time.of.day) tod
to <- tod %>%
  summarise(describe(Number.of.Vehicles)) to
#calculating Variance
tov <- tod %>%
  summarise(var(Number.of.Vehicles)) tov
#Calculating Standard Deviation
tos <- tod %>%
  summarise(sd(Number.of.Vehicles)) tos
cv <- tos/tov # Finding Coefficient of Variance using Standard Deviation and Variance
cv
#calculate mode of 'Time of day' by 'Number of Vehicles'
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
data2 %>%
  group_by(Time.of.day)
%>%
  reframe(mode_points = find_mode(Number.of.Vehicles))
#Calculate the Exponential Mean for "Time of day" by 'Number of Vehicles'
ndata2 %>%
  group_by(Time.of.day)
%>%
  summarize(geometric_mean = exp(mean(log(Number.of.Vehicles))))
#NUMBER OF LANES
#Calculate descriptive statistics of 'Number of lanes' by 'Number of Vehicles'
nol <- ndata2 %>%
  group_by(Number.of.lanes) nol
no <- nol %>%
  summarise(describe(Number.of.Vehicles)) no
#Calculate the Geometric mean for "Time of day" by 'Number Of Vehicles'
ndata2 %>%
  group_by(Time.of.day)
%>%
  reframe(j = geometric.mean(Number.of.Vehicles))
#calculate mode of 'Number of Lanes' by 'Number of Vehicles'
findmode <- function(b) {
  n <- unique(b)
  tab <- tabulate(match(b, n))
  n[tab == max(tab)]
}
ndata2 %>%
  group_by(Number.of.lanes)
%>%
  reframe(modepoints = findmode(Number.of.Vehicles))
# Calculate the geometric mean
ndata2 %>%
  group_by(Number.of.lanes) %>%
  summarize(geometric_mean = exp(mean(log(Number.of.Vehicles))))
#calculate Exponential mean
ndata2 %>%
  group_by(Number.of.lanes) %>%
  summarize(geometric_mean = exp(mean(log(Number.of.Vehicles))))
#calculating Variance
tol <- nol %>%
  summarise(var(Number.of.Vehicles)) tol
#Calculating Standard Deviation
tols <- nol %>%
  summarise(sd(Number.of.Vehicles)) tols
cv <- tol/tols # Finding Coefficient of Variance using Standard Deviation and Variance
cv
#INTERSECTION TYPE
#Calculate descriptive statistics of 'Intersection Type' by 'Number of Vehicles'
it <- ndata2 %>%
  group_by(Intersection.Type) it
t <- it %>%
  summarise(describe(Number.of.Vehicles)) t
#Calculate mode of 'Intersection Type' by 'Number of Vehicles'
finddmode <- function(k) {
  ns <- unique(k)
  tab <- tabulate(match(k, ns))
  ns[tab == max(tab)]
}
ndata2 %>%
  group_by(Intersection.Type)
%>%
  reframe(mode points = finddmode(Number.of.Vehicles))
# Calculate the geometric mean of 'Intersection Type' by 'Number of Vehicles'
ndata2 %>%
  group_by(Intersection.Type)
%>%
  reframe(geo_mean = geometric.mean(Number.of.Vehicles))
#calculating Variance
itv <- it %>%
  summarise(var(Number.of.Vehicles)) itv
#Calculating Standard Deviation
its <- it %>%
  summarise(sd(Number.of.Vehicles)) its
cv <- itv/its # Finding Coefficient of Variance using Standard Deviation and Variance
cv
#calculate Exponential mean
ndata2 %>%
  group_by(Intersection.Type) %>%
  summarize(expo_mean = exp(mean(log(Number.of.Vehicles))))
# Calculate the geometric mean
ndata2 %>%
  group_by(Intersection.Type) %>%
  summarize(expo_mean = exp(mean(log(Number.of.Vehicles))))
#ACCIDENTS
#Calculating the Descriptive Statistics for Number of Accidents using Summary and describe
summary(ndata2$Accidents)
describe(ndata2$Accidents)
#Calculating Variance
s <- var(ndata2$Accidents)
#Calculating Standard Deviation
l <- sd(ndata2$Accidents)
l
cv <- s/l # Finding Coefficient of Variance using Standard Deviation and Variance
cv
# Finding mode by creating a function
getmode <- function(v) {
  hash
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mo <- getmode(ndata2$Accidents)
mo
#Exponential Mean of the continuous variable
gm1 <- exp(mean(log(ndata2$Accidents)))
gm1
#Geometric mean of the continuous variable
gm2 <- geometric.mean(ndata2$Accidents)
gm2
# Create the histogram for entire Dataset 1
histo = hist(c(ndata2$Accidents,ndata2$Num.of.Vehicles))
histo
counts <- histo$counts
breaks <- histo$breaks
samplesMean <- mean(ndata2$Num.of.Vehicles)
samplesSd <- sd(ndata2$Num.of.Vehicles)
prev <- 0
cp <- list()
for(i in 1:(length(breaks)-1)){
  low <- breaks[i]
  high <- breaks[i+1]
  classProb <- pnorm(high,mean = samplesMean, sd =samplesSd)
  if(i==(length(breaks)-1)){
    cp <- append(cp, 1-classProb)
  }
  else
    cp <- append(cp, classProb-prev)
  prev <- classProb
}
cp
sum(unlist(cp))
# perform chi-square goodness-of-fit test
#expected <- rep(length(ndata2$Num.of.Vehicles)/length(cp), length(cp))
exp <- list()
for (i in cp){
  exp <- append(exp, i*length(ndata2$Num.of.Vehicles))
}
exp
chilist <- list()
for (i in 1:length(exp)){
  #print(counts[i])
  #print(exp[i])
  e[i] <- unlist(exp[i])
  chilist <- append(chilist,(counts[i] - e[i])^2/e[i])
}
chilist
chisq <- sum((counts - e)^2/ e)
df <- length(ndata2$Num.of.Vehicles)-1
value <- qchisq(0.05, df, lower.tail= FALSE)
value
#p-values
result <- t.test(Num.of.Vehicles ~ Time.of.day, data = ndata2 , subset = Time.of.day %in%
                   c("Morning", "Afternoon"))
p_values <- result$p.value
p_values
full_model <- lm(Num.of.Vehicles ~ Number.of.lanes , data = ndata2, subset = Number.of.lanes %in%
                   c("6", "8"))
# Fit a reduced model with only the intercept
reduced_model <- lm(Num.of.Vehicles ~ 1, data = ndata2 , subset = Number.of.lanes %in% c("6",
                                                                                         "8"))
# Compare the fit of the two models using an F-test
result2 <- anova(reduced_model, full_model)
result2
# Extract the p-value for the F-test
p_value2 <- result2["Pr(>F)"]
# Print the p-value
p_value2


#DATASET 2 - CODE:
#Installing required
packages
install.packages("psych")
library(psych)
install.packages("lubridate")
library(lubridate)
install.packages("dplyr")
library(dplyr)
install.packages("hms")
library(hms)
install.packages("tidyr")
library(tidyr)
#reading the CSV file into Rstudio and storing it in a variable
data1 <- read.csv("C:/Users/sushm/Downloads/PROJECT DATASETS 2 - Sheet2
(1).csv", header=T, sep=",")
data2 = data.frame(data1) #Creating a data frame
View(data2) #used to view the data frame
#Checking for null values
is.na(data3)
#Using the function summary and describe to represent descriptive statistics
summary(data2)
describe(data2)
#create a boxplot for the Number of pedestrians
boxplot(data2$Pedestrians)
#dropping the outliers
Q1 <- quantile(data2$Pedestrians, .25)
Q3 <- quantile(data2$Pedestrians, .75)
IQR <- IQR(data2$Pedestrians)
#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
ndata2 <- subset(data2, data2$Pedestrians > (Q1 - 1.5*IQR) & data2$Pedestrians< (Q3 +
                                                                                   1.5*IQR))
View(ndata2)
boxplot(ndata2$Pedestrians)
#view row and column count of new data frame
dim(ndata2) #Orginal dataframe had 175 values after removing outliers dataframe has 170 values
#Finding descriptive statistics for pedestrians
describe(ndata2$Pedestrians)
s <- var(ndata2$Pedestrians)
s
l <- sd(ndata2$Pedestrians)
l
cv <- s/l # Finding Coefficient of Variance using Standard Deviation and Variance
cv
# Finding mode by creating a function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mo <- getmode(ndata2$Pedestrians)
mo
#Boxplot for Number of Pedestrains
boxplot(ndata2$Pedestrians)
#sorting the start time
new <-data3[order(data3$Start.Time),]
new
df <- new
View(df)
#Calculating Inter arrival time
Inter_arrival_time <- strptime(df$Start.Time, "%H:%M") -
  lag(strptime(df$Start.Time, "%H:%M"))
b <- as.numeric(Inter_arrival_time)
b
#Combining the calculated inter arrival column to main dataframe
df1 <- cbind(df, b)
df1
#checking for na values and droping them
df1 <- df1 %>% drop_na()
#Finding descriptive statistics for Inter arrival time
describe(Inter_arrival_time)
mean(df1$b)
median(df1$b)
a <- var(df1$b)
a
b <- sd(df1$b)
b
v <- a/b # Finding Coefficient of Variance using Standard Deviation ad Variance
v
df <- new
View(df)
#Calculating Inter arrival time
Inter_arrival_time <- strptime(df$Start.Time, "%H:%M") -
  lag(strptime(df$Start.Time, "%H:%M"))
b <- as.numeric(Inter_arrival_time)
b
#Combining the calculated inter arrival column to main dataframe
df1 <- cbind(df, b)
df1
#checking for na values and droping them
df1 <- df1 %>% drop_na()
#Finding descriptive statistics for Inter arrival time
describe(Inter_arrival_time)
mean(df1$b)
median(df1$b)
a <- var(df1$b)
a
b <- sd(df1$b)
b
v <- a/b # Finding Coefficient of Variance using Standard Deviation ad Variance
v
# Finding mode by creating a function
getmode <- function(p) {
  uniq <- unique(p)
  uniq[which.max(tabulate(match(p, uniq)))]
}
m <- getmode(df1$b)
m
#Boxplot for Inter arrival time
boxplot(df1$b)
#Histogram for number of Pedestrians
hist(c(df1$b,df1$Pedestrians))
#Goodness-of-Fit Calculations
samples <- ndata2$Number.of.Pedestrians[1:nrow(ndata2)]
samples
a <- mean(samples)
a
s <- sd(samples)
s
histo = hist(c(ndata2$Number.of.Pedestrians))
histo
count <- histo$counts
# calculate the class probabilities
classpro <- list()
classpro <- append(classpro, pexp(5,rate = 0.06319702))
classpro <- append(classpro, (pexp (10, rate = 0.06319702) - pexp(5, rate = 0.06319702)))
classpro <- append(classpro, (pexp (15, rate = 0.06319702) - pexp(10, rate = 0.06319702)))
classpro <- append(classpro, (pexp (20, rate = 0.06319702) - pexp(15, rate = 0.06319702)))
classpro <- append(classpro, (pexp (25, rate = 0.06319702) - pexp(20, rate = 0.06319702)))
classpro <- append(classpro, (pexp (30, rate = 0.06319702) - pexp(25, rate = 0.06319702)))
classpro <- append(classpro, (pexp (35, rate = 0.06319702) - pexp(30, rate = 0.06319702)))
classpro <- append(classpro, (pexp (40, rate = 0.06319702) - pexp(35, rate = 0.06319702)))
classpro <- append(classpro, (pexp (45, rate = 0.06319702) - pexp(40, rate = 0.06319702)))
classpro <- append(classpro, (pexp (50, rate = 0.06319702) - pexp(45, rate = 0.06319702)))
classpro <- append(classpro, (pexp (55, rate = 0.06319702) - pexp(50, rate = 0.06319702)))
classpro <- append(classpro, (1 - pexp(60, rate = 0.06319702)))
classpro
#Calculating the expected values
expected_freq <- list()
for (i in classpro){
  expected_freq <- append(expected_freq,i*length(samples))
}
expected_freq
#Calculating Chi-Squared
chiList <- list()
for(i in 1:length(expected_freq)){
  a[i] <- unlist(expected_freq[i])
  chiList <- append(chiList, (count[i] - a[i])^2 / a[i])
}
chiList
chisq <- sum((count - a)^2 / a)
df <- length(ndata2$Number.of.Pedestrians)-1
df
n <- length(data2$Pedestrians)
n
# Calculate test statistic and p-value
test_statistic <- sum((count - a)^2 / a)
test_statistic
df = length(ndata2$Number.of.Pedestrians) - 1
p_value <- qchisq(0.05, df)