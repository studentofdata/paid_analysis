library(data.table)
library(plyr)
library(ggplot2)
library(lubridate)
library(MASS)
library(mgcv)


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

slope.loess <-function(X, data){
  # First your loess function:
  my_loess <- loess(y~x, data=data, subset=data$product_group==X, degree=2)
  # Then the first difference
  first_diff <- diff(my_loess$fitted)
  # Then the corresponding x and y values for the minima and maxima
  res <- cbind(my_loess$x[c(which.min(first_diff), which.max(first_diff))], 
               my_loess$fitted[c(which.min(first_diff), which.max(first_diff))])
  x <- my_loess$x[c(which.min(first_diff), which.max(first_diff))]
  print(X)
  print(x)
  #print(res)
  
  #colnames(res) <- c("x", "y")
  #rownames(res) <- c("min", "max")
  res
}

# Create margin key to merge with revenue table to produce estimated profits
margins <- c(.357, .357, .491, .292, .336, .124, .461, .357, .357, .357, .357)
product_group <- as.character(unique(ppc.app.rev$product_group))
margin.key <- as.data.frame(cbind(product_group, margins))
margin.key$margins <- as.numeric(as.character(margin.key$margins))

rev <- data.table(ppc.app.rev)
rev <- merge(rev, margin.key, by = "product_group")

# Create estimated profit vector
rev$profit <- (rev$total_revenue*rev$margins)

# Create temporal features
date <- strptime(rev$dt, "%m/%d/%Y")
rev$date <- as.POSIXct(date)
rev$yr <- as.factor(year(rev$date))
rev$mon <- as.factor(month(rev$date))
rev$wk <- as.factor(week(rev$date))
rev$qtr <- as.factor(quarter(rev$date))


cost.pg <- rev[, list(visits = sum(total_visits),
                     clicks = sum(total_clicks),
                     cost = sum(adwords_cost),
                     sub.apps = sum(submitted_apps),
                     used.apps = sum(used_apps),
                     app.used.conv.rate = sum(app_used_conv_rate),
                     total.rev = sum(total_revenue),
                     value.per.visit = sum(value_per_visit),
                     value.sub.app = sum(value_per_submitted_app),
                     avg.invoice.val = sum(avg_invoice_value),
                     profit = sum(profit)),
              by = list(product_group)]


# Costs vs Profits by Category
cost.plot <- ggplot(cost.pg, aes(x = product_group, y = cost)) + geom_bar()
profit.plot <- ggplot(cost.pg, aes(x = product_group, y = profit)) + geom_bar()
#multiplot(cost.plot, profit.plot, cols = 2)


rev.wk <- rev[, list(visits = sum(total_visits),
                     clicks = sum(total_clicks),
                     cost = sum(adwords_cost),
                     sub.apps = sum(submitted_apps),
                     used.apps = sum(used_apps),
                     app.used.conv.rate = sum(app_used_conv_rate),
                     total.rev = sum(total_revenue),
                     avg.invoice.val = sum(avg_invoice_value),
                     profit = sum(profit)),
              by = list(yr, mon, wk, qtr,product_group)]

rev.wk.wobrand <- subset(rev.wk, product_group!= "Brand")


rev.v1 <- rev[, list(visits = sum(total_visits),
                     clicks = sum(total_clicks),
                     cost = sum(adwords_cost),
                     sub.apps = sum(submitted_apps),
                     used.apps = sum(used_apps),
                     app.used.conv.rate = sum(app_used_conv_rate),
                     total.rev = sum(total_revenue),
                     avg.invoice.val = sum(avg_invoice_value),
                     profit = sum(profit)),
              by = list(yr, mon, wk, qtr, product_group)]
rev.v1$net.profit <- (rev.v1$profit - rev.v1$cost)
rev.v1$value.per.sub.app <- (rev.v1$total.rev/rev.v1$sub.apps)
rev.v1$profit.per.sub.app <- (rev.v1$net.profit/rev.v1$sub.apps)

# Electronics
rev.elec <- subset(rev, product_group == "Electronics")
rev.elec.v1 <- rev.elec[, list(visits = sum(total_visits),
                               clicks = sum(total_clicks),
                               cost = sum(adwords_cost),
                               sub.apps = sum(submitted_apps),
                               used.apps = sum(used_apps),
                               app.used.conv.rate = sum(app_used_conv_rate),
                               total.rev = sum(total_revenue),
                               avg.invoice.val = sum(avg_invoice_value),
                               profit = sum(profit)),
                        by = list(yr, mon, wk, qtr)]
rev.elec.v1$net.profit <- (rev.elec.v1$profit - rev.elec.v1$cost)
rev.elec.v1$value.per.sub.app <- (rev.elec.v1$total.rev/rev.elec.v1$sub.apps)
rev.elec.v1$profit.per.sub.app <- (rev.elec.v1$net.profit/rev.elec.v1$sub.apps)

# Furniture
rev.furn <- subset(rev, product_group == "Furniture")
rev.furn.v1 <- rev.furn[, list(visits = sum(total_visits),
                               clicks = sum(total_clicks),
                               cost = sum(adwords_cost),
                               sub.apps = sum(submitted_apps),
                               used.apps = sum(used_apps),
                               app.used.conv.rate = sum(app_used_conv_rate),
                               total.rev = sum(total_revenue),
                               avg.invoice.val = sum(avg_invoice_value),
                               profit = sum(profit)),
                        by = list(yr, mon, wk, qtr)]
rev.furn.v1$net.profit <- (rev.furn.v1$profit - rev.furn.v1$cost)
rev.furn.v1$value.per.sub.app <- (rev.furn.v1$total.rev/rev.furn.v1$sub.apps)
rev.furn.v1$profit.per.sub.app <- (rev.furn.v1$net.profit/rev.furn.v1$sub.apps)

# Appliances
rev.appl <- subset(rev, product_group == "Appliances")
rev.appl.v1 <- rev.appl[, list(visits = sum(total_visits),
                               clicks = sum(total_clicks),
                               cost = sum(adwords_cost),
                               sub.apps = sum(submitted_apps),
                               used.apps = sum(used_apps),
                               app.used.conv.rate = sum(app_used_conv_rate),
                               total.rev = sum(total_revenue),
                               avg.invoice.val = sum(avg_invoice_value),
                               profit = sum(profit)),
                        by = list(yr, mon, wk, qtr)]
rev.appl.v1$net.profit <- (rev.appl.v1$profit - rev.appl.v1$cost)
rev.appl.v1$value.per.sub.app <- (rev.appl.v1$total.rev/rev.appl.v1$sub.apps)
rev.appl.v1$profit.per.sub.app <- (rev.appl.v1$net.profit/rev.appl.v1$sub.apps)

# Bedding
rev.bed <- subset(rev, product_group == "Bedding")
rev.bed.v1 <- rev.bed[, list(visits = sum(total_visits),
                               clicks = sum(total_clicks),
                               cost = sum(adwords_cost),
                               sub.apps = sum(submitted_apps),
                               used.apps = sum(used_apps),
                               app.used.conv.rate = sum(app_used_conv_rate),
                               total.rev = sum(total_revenue),
                               avg.invoice.val = sum(avg_invoice_value),
                               profit = sum(profit)),
                        by = list(yr, mon, wk, qtr)]
rev.bed.v1$net.profit <- (rev.bed.v1$profit - rev.bed.v1$cost)
rev.bed.v1$value.per.sub.app <- (rev.bed.v1$total.rev/rev.bed.v1$sub.apps)
rev.bed.v1$profit.per.sub.app <- (rev.bed.v1$net.profit/rev.bed.v1$sub.apps)

# Branding
rev.brand <- subset(rev, product_group == "Brand")
rev.brand.v1 <- rev.brand[, list(visits = sum(total_visits),
                               clicks = sum(total_clicks),
                               cost = sum(adwords_cost),
                               sub.apps = sum(submitted_apps),
                               used.apps = sum(used_apps),
                               app.used.conv.rate = sum(app_used_conv_rate),
                               total.rev = sum(total_revenue),
                               avg.invoice.val = sum(avg_invoice_value),
                               profit = sum(profit)),
                        by = list(yr, mon, wk, qtr)]
rev.brand.v1$net.profit <- (rev.brand.v1$profit - rev.brand.v1$cost)
rev.brand.v1$value.per.sub.app <- (rev.brand.v1$total.rev/rev.brand.v1$sub.apps)
rev.brand.v1$profit.per.sub.app <- (rev.brand.v1$net.profit/rev.brand.v1$sub.apps)

# Finance
rev.fin <- subset(rev, product_group == "Finance")
rev.fin.v1 <- rev.fin[, list(visits = sum(total_visits),
                               clicks = sum(total_clicks),
                               cost = sum(adwords_cost),
                               sub.apps = sum(submitted_apps),
                               used.apps = sum(used_apps),
                               app.used.conv.rate = sum(app_used_conv_rate),
                               total.rev = sum(total_revenue),
                               avg.invoice.val = sum(avg_invoice_value),
                               profit = sum(profit)),
                        by = list(yr, mon, wk, qtr)]
rev.fin.v1$net.profit <- (rev.fin.v1$profit - rev.fin.v1$cost)
rev.fin.v1$value.per.sub.app <- (rev.fin.v1$total.rev/rev.fin.v1$sub.apps)
rev.fin.v1$profit.per.sub.app <- (rev.fin.v1$net.profit/rev.fin.v1$sub.apps)

# Lawn Equipment
rev.le <- subset(rev, product_group == "Lawn Equipment")
rev.le.v1 <- rev.fin[, list(visits = sum(total_visits),
                             clicks = sum(total_clicks),
                             cost = sum(adwords_cost),
                             sub.apps = sum(submitted_apps),
                             used.apps = sum(used_apps),
                             app.used.conv.rate = sum(app_used_conv_rate),
                             total.rev = sum(total_revenue),
                             avg.invoice.val = sum(avg_invoice_value),
                             profit = sum(profit)),
                      by = list(yr, mon, wk, qtr)]
rev.le.v1$net.profit <- (rev.le.v1$profit - rev.le.v1$cost)
rev.le.v1$value.per.sub.app <- (rev.le.v1$total.rev/rev.le.v1$sub.apps)
rev.le.v1$profit.per.sub.app <- (rev.le.v1$net.profit/rev.le.v1$sub.apps)


fit <- ggplot(data = rev.fin.v1, aes(x = cost, y = net.profit)) + geom_point(size = 2.5,shape = 16)+
  stat_smooth(geom = "smooth", method = "loess", colour = "#0078C9",level =.95, size = 1)

plot(fit)

# names(rev.v1)[8] <- "x"
# names(rev.v1)[15] <- "y"
# 
# #Then apply the function to each group
# slope.rev.v1 <- lapply(levels(rev.v1$product_group), FUN=slope.loess, data=rev.v1)
# names(slope.rev.v1) <- levels(rev.v1$product_group)
