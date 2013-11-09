library(plotrix)
library(forecast)
library(car)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(mgcv)
library(hydroGOF)
library(hexbin)

Q2<-"#B01A48"
Q3<-"#EDC200"
Q4<-"#FF5A00"
###########
############################Ethology Color Palette######################################
ethoPal<-c("#FF5A00","#78256F", "#0078C9","#B01A48","#CA0083","#39892F","#EDC200","#CACAC8")
########################################################################################

#Bring in data, Assign results data.v1.miss includes all data including that stupid missing week, data.v1 is clean
data.v1<-read.csv("C:/Users/Bobby Row/Google Drive/Ethology/Analytics/Forecasting/graphingset.csv")
#data.v1<-read.csv(file = 'C:/Users/Darth Nihilus/Google Drive/Ethology/Analytics/Forecasting/graphingset.csv')
attach(data.v1)

Quarter<-as.factor(data.v1$date.quarter)

##########Scatter Plots Stage 1###############

########Smoothing Function##############

#All Quarters
Sall<-ggplot(data.v1,aes( x = cost, y = goal2, colour = Quarter, size = cost)) + geom_point(size = 2.5, shape = 16) + scale_colour_manual(values = ethoPal) +
  xlab("spend") +
  ylab("credit app submissions") +
  opts(title = "spend and submissions by quarter") 



#Q1
ScatterQ1<-data.v1[which(data.v1$date.quarter == 1),]

S1<-ggplot(data = ScatterQ1, aes(x = cost, y = goal2)) + geom_point(aes(colour = "#FF5A00"), size = 2.5, shape =16) + 
  opts(legend.position = "none") +
  scale_colour_manual(values = c("#FF5A00")) +
  xlab("spend") +
  ylab("credit app submissions") +
  opts(title = "spend and submissions: 1st quarter") + scale_y_continuous(limits = c(0,6000)) + scale_x_continuous(limits = c(0,60000)) +
  #smoothing lines
  stat_smooth(geom = "smooth", method = "loess", level = .95, size = 1, colour = "#FF5A00")



#Q2
ScatterQ2<-data.v1[which(data.v1$date.quarter == 2),]

S2<-ggplot( data = ScatterQ2, aes(x = cost, y = goal2)) +geom_point(aes(colour = "#78256F") , size = 2.5, shape =16) +
  opts(legend.position = "none") +
  scale_colour_manual(values = c("#78256F")) +
  xlab("spend") +
  ylab("credit app submissions") +
  opts(title = "spend and submissions: 2nd quarter") + scale_y_continuous(limits = c(0,6000)) + scale_x_continuous(limits = c(0,60000))+
  #smoothing lines
  stat_smooth( geom = "smooth", method = "loess", colour = "#78256F", level = .95 , size = 1)



#Q3
ScatterQ3<-data.v1[which(data.v1$date.quarter == 3),]

S3<-ggplot( data = ScatterQ3, aes(x = cost, y = goal2)) +geom_point(aes(colour="#0078C9"),size = 2.5, shape =16) +
  opts(legend.position = "none") +
  scale_colour_manual(values = c("#0078C9")) +
  xlab("spend") +
  ylab("credit app submissions") +
  opts(title = "spend and submissions: 3rd quarter") + scale_y_continuous(limits = c(0,6000)) + scale_x_continuous(limits = c(0,60000)) +
  stat_smooth( geom = "smooth", method = "loess", colour = "#0078C9", level = .95 , size = 1)




#Q4
ScatterQ4<-data.v1[which(data.v1$date.quarter == 4),]

S4<-ggplot( data = ScatterQ4, aes(x = cost, y = goal2)) +geom_point(aes(colour="#B01A48"),size = 2.5, shape =16) +
  opts(legend.position = "none") +
  scale_colour_manual(values = c("#B01A48")) +
  xlab("spend") +
  ylab("credit app submissions") +
  opts(title = "spend and submissions: 4th quarter") + scale_y_continuous(limits = c(0,6000)) + scale_x_continuous(limits = c(0,60000)) +
  stat_smooth( geom = "smooth", method = "loess", colour = "#B01A48", level = .95 , size = 1)


##All Quarters with their respective regression lines

Srall<-ggplot(data.v1,aes( x = cost, y = goal2, colour = Quarter, size = cost)) + geom_point(size = 2.5, shape = 16) + scale_colour_manual(values = ethoPal) +
  xlab("spend") +
  ylab("credit app submissions") +
  opts(title = "spend and submissions by quarter") + 
  geom_smooth(method = "loess" , alpha = .2 , size = 1)


## Current Distribution & Ideal Distribution
#Simulate Data

idealspend<- rnorm(157, 45000, 3500)
set<-cbind(data.v1$cost,idealspend)
colnames(set)[1] <- 'cost'
meltset<-melt(set)

a<-ggplot(data = meltset) + geom_histogram(aes(x=cost), fill = "#78256F" , alpha = 1) +
  geom_histogram(aes(x = idealspend), fill = "#FF5A00", alpha = .8) +
  xlab("spend")+
  ylab("weeks")+
  opts(title = "all spend vs ideal spend")


##Playing with boxplots
b<-ggplot(data.v1, aes(Quarter, goal2)) + geom_boxplot(aes(fill = Quarter)) + coord_flip() + scale_fill_manual(values = ethoPal) +
  ylab("credit app submissions")  + xlim("4","3","2","1")

c<-ggplot(data.v1, aes(Quarter, cost)) + geom_boxplot(aes(fill= Quarter)) + coord_flip() + scale_fill_manual(values = ethoPal) +
  ylab("spend") + xlim("4","3","2","1")

#plotmult<- grid.arrange(b,c,nrow = 2)



######## data in total #########################
goalc<-as.numeric(data.v1$goal2)
costc<-as.numeric(data.v1$cost)

fit.l<-loess(log(goalc)~log(costc), data= data.v1)
################################################




##Confidence Interval & Prediction
fit<-ggplot(data = data.v1, aes(x = cost, y =goal2)) + geom_point()+
  xlab("spend") +
  ylab("credit app submissions") +
  opts(title = "Prediction")

##playing with non-linear, non parametric regression
fit.non.2<-fit+stat_smooth(method = "gam",formula = y~ s(x, k = 2),size  = 1, colour ="#78256F" )
fit.non.3<-fit+stat_smooth(method = "gam",formula = y~ s(x, k = 3),size  = 1)
fit.non.4<-fit+stat_smooth(method = "gam",formula = y~ s(x, k = 5),size  = 1)

plotmultfit<- grid.arrange(fit.non.2,fit.non.3,fit.non.4,ncol = 3)
############  Q4 prediction and visuals ###############

date <- strptime(data.v1$date, "%m/%d/%Y")
data.v1$date.1 <- as.POSIXct(date, tz = "GMT")

data.v2 <- data.v1
data.v2$yr <- as.factor(year(data.v2$date.1))
data.v2$goal2 <- data.v2$goal2-300

# Model Building
mod.gam <- lm(goal2 ~ cost, data = data.v2)


# error checking and reference
newdat.v1
newdat.v2 <- rename(newdat.v1, c("apps" = "goal2"))
newdat.v3 <- newdat.v2
newdat.v3$cost <- rnorm(14, 20000, 500)


p.v1 <- as.vector(predict(mod.gam, newdat.v3))

# Overall PPC performance, fit model, make updates


# Density plots and argument for ROI & Cost curve
den.roi.cost <-  ggplot(m.frame, aes(x=cost,y=ROI)) + geom_density2d()

# Additional Density Plot
p <- ggplot(m.frame,aes(x=cost,y=revenue))+
  stat_density2d(aes(alpha=..level..), geom="polygon") +
  scale_alpha_continuous(limits=c(0,0.2),breaks=seq(0,0.2,by=0.025))+
  geom_point(colour="red",alpha=0.02)+
  theme_bw()
data.v2 <- data.table(data.v2)
data.v3 <- rename(data.v2, c("goal2"="apps"))
# Q4 Spend argument visual (needs branding)
qplot(wk, apps, data = m.frame.q4.v1, size = cost, color = yr)

x <- ggplot(m.frame.q4.v1, )

qplot(wk, apps, data = m.frame.q3.v1, size = cost, color = yr)

bin <- hexbin(m.frame.q4$cost, m.frame.q4$apps, xbins = 50)
plot(bin)



