library(data.table)
library(plyr)
library(ggplot2)
library(lubridate)
library(MASS)
library(mgcv)
# This file will be to manipulate my data sets for analysis
# Paid analysis, what is happening, what are we spending, what does this look like?
eSecPal<-c("#78256F","#0078C9","#FF5A00","#39892F","#EDC200","#CA0083","#B01A48","#CACAC8","#00FFCC")
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


# Cost data wrangling
date <- strptime(cost.data$date, "%m/%d/%Y")
cost.data$date.1 <- as.POSIXct(date, tz ="GMT")

g.c <- as.data.frame(goal.completions)
g.c.v1 <- ddply(g.c, .(date), summarise, 
                visits = sum(visits),
                bounces = sum(bounces),
                pageviews = sum(pageviews),
                timeonsite = mean(avgTimeOnSite),
                apps = sum(goal2completions))

date <- strptime(g.c.v1$date, "%Y-%m-%d")
g.c.v1$date.1 <- as.POSIXct(date, tz = "GMT")


g.c.v1[order(as.Date(g.c.v1$date, "%Y-%m-%d")),]

m.frame <-g.c.v1




m.frame <- merge(g.c.v1, cost.data, by = c("date.1"))
no.vars <- names(m.frame) %in% c("date.x","date.y")
m.frame <- m.frame[!(no.vars)]



# Goal Completions Agg & Disagg

# Putting some features into our m.frame by slicing on time dim
m.frame$qtr <- as.factor(quarters(m.frame$date.1))
m.frame$mon <- as.factor(month(m.frame$date.1))
m.frame$yr <- as.factor(year(m.frame$date.1))
m.frame$wk <- as.factor(week(m.frame$date.1))
m.frame$qtr.yr <- paste0(as.character(m.frame$qtr), as.character(m.frame$yr))
#m.frame.v1 <- subset(m.frame, qtr == "Q3" & yr == "2013" & mon == "7")
m.frame <- data.table(m.frame)


g.c.qtr <- m.frame[,list(visits = sum(visits),
                              bounces = sum(bounces),
                              pageviews = sum(pageviews),
                              timeonsite = mean(timeonsite),
                              apps = sum(apps),
                              impr = sum(impr),
                              clicks = sum(clicks),
                              cost = sum(cost)),
                        by = list(qtr.yr)]

# extracting quarter from quarter year rollup 
g.c.qtr$qtr <- substr(g.c.qtr$qtr.yr,0,2)

appvalue = 330
#first attempt at an ROI figure
m.frame$ROI <- remove_outliers(((m.frame$apps*appvalue-m.frame$cost)/m.frame$cost))
m.frame$revenue <- m.frame$apps*appvalue



m.frame.q1 <- subset(m.frame, qtr == "Q1")
m.frame.q2 <- subset(m.frame, qtr == "Q2")
m.frame.q3 <- subset(m.frame, qtr == "Q3")
m.frame.q4 <- subset(m.frame, qtr == "Q4")

y.1 <- lm(apps~cost, data = m.frame.q1)
y.2 <- lm(apps~cost, data = m.frame.q2)
y.3 <- lm(apps~cost, data = m.frame.q3)
y.4 <- lm(apps~cost, data = m.frame.q4)


m.frame.q1.v1 <- m.frame.q1[, list(apps = sum(apps),
                                   cost = sum(cost),
                                   avg.pos = mean(avg.pos),
                                   impr = sum(impr)),
                            by = list(qtr.yr,wk,yr)]

m.frame.q2.v1 <- m.frame.q2[, list(apps = sum(apps),
                                   cost = sum(cost),
                                   avg.pos = mean(avg.pos),
                                   impr = sum(impr)),
                            by = list(qtr.yr,wk,yr)]

m.frame.q3.v1 <- m.frame.q3[, list(apps = sum(apps),
                                   cost = sum(cost),
                                   avg.pos = mean(avg.pos),
                                   impr = sum(impr)),
                            by = list(qtr.yr,wk,yr)]

m.frame.q4.v1 <- m.frame.q4[, list(apps = sum(apps),
                                   cost = sum(cost),
                                   avg.pos = mean(avg.pos),
                                   impr = sum(impr)),
                            by = list(qtr.yr,wk,yr)]

t.v1 <- subset(m.frame.q1.v1, qtr.yr == "Q12013")
t1 <- cor(t.v1$cost,t.v1$apps)
t.v2 <- subset(m.frame.q2.v1, qtr.yr == "Q22013")
t2 <- cor(t.v2$cost,t.v2$apps)
t.v3 <- subset(m.frame.q3.v1, qtr.yr == "Q32013")
t3 <- cor(t.v3$cost,t.v3$apps)
t.v4o <- subset(m.frame.q4.v1, qtr.yr == "Q42012")
t4 <- cor(t.v4o$cost,t.v4o$apps)
newdat.v1 <- subset(m.frame.q3.v1, qtr.yr == "Q32013")


m.frame.bg.d <- m.frame[, list(visits = sum(visits),
                               apps = sum(apps),
                               impr = sum(impr),
                               cost = sum(cost)),
                        by = list(qtr.yr,yr, qtr)]

apps.plot <- ggplot(m.frame.bg.d, aes(x = qtr.yr, y = apps, fill = qtr)) + geom_bar(position = position_dodge())  + 
  scale_colour_manual(values = eSecPal) 
    

cost.plot <- ggplot(m.frame.bg.d, aes(x = qtr.yr, y = cost, fill = qtr)) + geom_bar(position = position_dodge()) + 
  scale_colour_manual(values = eSecPal)

#multiplot(apps.plot, cost.plot, cols = 2)