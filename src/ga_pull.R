#Open connection instance
library("rga")
options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))
rga.open(instance = "ga")


# Get profiles so I can subset
profile_df <- ga$getProfiles()
#16014260
domains <- c("http://www.conns.com")
id.info <- profile_df[which(profile_df$websiteUrl %in% domains), ]



start.date <- ga$getFirstDate(id.info[2,1])

goal.completions <- ga$getData(id.info[2,1], batch = TRUE, walk = TRUE,
                                start.date = start.date,
                                end.date = today()-1,
                                metrics = "ga:visits,ga:bounces,ga:pageviews,ga:avgTimeOnSite, ga:goal2completions",
                                dimensions = "ga:date",
                                filter = "ga:medium==CPC,ga:medium==cpc")


