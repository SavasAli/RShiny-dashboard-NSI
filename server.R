## server.R ##
library("rga")
library("shiny")
library("shinydashboard")
library("tidyr")
library("ggplot2")
library("zoo")
library("xts")
library("data.table")
library("scales")
library("plyr")
library("dplyr")
library("xlsx")
library("plotly")

##reading existing GA data
ga_data <- read.csv(file = "ga_data_2017.csv", stringsAsFactors = FALSE)
ga_data$date <- as.Date(ga_data$date, format = "%Y-%m-%d")

##Determining begin and end date of the data to be retrieved
lastDay <- as.Date(ga_data$date[nrow(ga_data)])
lastSunday <- Sys.Date() - as.POSIXlt(Sys.Date())$wday

if (lastSunday - lastDay >= 7){
  #profile ID of your Google Analytics account is the number in the url which starts after the 'p'
  id <- 6204970
  
  #Run this locally the first time. Hereby you allow R to acces GA data. Copy and paste the code in the console below.
  #A token is saved whereby you can acces GA data without asking authorization again.
  #rga.open(instance = "ga")
  
  #Run this when you've already saved a token
  rga.open(where = "token.rga")
  
  #Getting GA data
  ga_data_new <- ga$getData(id = id, start.date = lastDay+1, #lastDay+1, #as.Date("2017-01-02"), #, 
                        end.date = lastSunday,
                        metrics = "ga:sessions, ga:goal2Completions, ga:transactions, ga: transactionRevenue",
                        dimensions = "ga:date, ga:source, ga:medium, ga: sourceMedium",
                        batch = TRUE)
  
  names(ga_data_new) <- names(ga_data)
  ga_data <- rbind(ga_data, ga_data_new)
  write.table(ga_data_new, "ga_data_2017.csv", append = T, row.names = F, col.names = F, sep = ",")
  } 
  
#reading in targets
targets <- read.xlsx("targets.xlsx", 2) #sheet two contains targets for 2017

#functions_17.R

#functions for determining nsi specific channels
f_total <- function(data){
  aggregate(x = data[c(5:8)], FUN = sum, by = list(date = data$date))
}

f_seo <- function(data){
  data <- data[which(data[,3] == "organic"),]
  aggregate(x = data[c(5:8)], FUN = sum, by = list(date = data$date))
}

f_sea <- function(data){
  data <- data[which(data[,3] == "cpc"),]
  aggregate(x = data[c(5:8)], FUN = sum, by = list(date = data$date))
}

f_ns <- function(data){
  data <- data[which(data[,2] == "ns.nl" & data[,3] != "email" | data[,2] == "ns" & data[,3] != "email" ),]
  aggregate(x = data[c(5:8)], FUN = sum, by = list(date = data$date))
}

f_direct <- function(data){
  data <- data[which(data[,2] == "(direct)"),]
  aggregate(x = data[c(5:8)], FUN = sum, by = list(date = data$date))
}

f_nsi_app <- function(data){
  data <- data[which(data[,2] == "nsinternational.nl" & data[,3] == "app"),]
  aggregate(x = data[c(5:8)], FUN = sum, by = list(date = data$date))
}

f_affiliates <- function(data){
  data <- data[which(data[,3] == "affiliate"),]
  aggregate(x = data[c(5:8)], FUN = sum, by = list(date = data$date))
}

f_nsi_email <- function(data){
  data <- data[which(data[,2] == "nsinternational.nl" & data[,3] == "email"),]
  aggregate(x = data[c(5:8)], FUN = sum, by = list(date = data$date))
}

f_referral <- function(data){
  data <- data[which(data[,3] == "referral" & data[,2] != "ns.nl"),]
  aggregate(x = data[c(5:8)], FUN = sum, by = list(date = data$date))
}

f_retargeting <- function(data){
  data <- data[which(data[,2] == "criteo" | data[,2] == "8Ax6p4" | data[,2] == "1Xqiv9" | data[,2] == "cas.fr.eu.criteo.com" | data[,2] == "cas.nl.eu.criteo.com"),]
  aggregate(x = data[c(5:8)], FUN = sum, by = list(date = data$date))
}

f_facebook <- function(data){
  data <- data[which(data[,2] == "m.facebook.com" | data[,2] == "facebook.com" | data[,2] == "lm.facebook.com" | data[,2] == "l.facebook.com" | data[,2] == "facebook" & data[,3] == "social"),]
  aggregate(x = data[c(5:8)], FUN = sum, by = list(date = data$date))
}

f_webvertising <- function(data){
  data <- data[which(data[,3] == "webvertising" & data[,2] != "criteo" & data[,2] != "8Ax6p4" & data[,2] != "1Xqiv9" & data[,2] != "cas.fr.eu.criteo.com" & data[,2] != "cas.nl.eu.criteo.com"),]
  aggregate(x = data[c(5:8)], FUN = sum, by = list(date = data$date))
}

f_ns_email <- function(data){
  data <- data[which(data[,2] == "ns.nl" & data[,3] == "email"),]
  aggregate(x = data[c(5:8)], FUN = sum, by = list(date = data$date))
}

#splitting data up into different channels
total <- f_total(ga_data)
seo <- f_seo(ga_data)
sea <- f_sea(ga_data)
ns <- f_ns(ga_data)
direct <- f_direct(ga_data)
nsi_app <- f_nsi_app(ga_data)
affiliates <- f_affiliates(ga_data)
nsi_email <- f_nsi_email(ga_data)
referral <- f_referral(ga_data)
retargeting <- f_retargeting(ga_data)
facebook <- f_facebook(ga_data)
webvertising <- f_webvertising(ga_data)
ns_email <- f_ns_email(ga_data)

#filling missing dates with zeros
alldates <- data.table(date=seq.Date(min(ga_data$date), max(ga_data$date), by = "day"))

#Filling missing dates with zeros
if (nrow(seo)<nrow(alldates)) {
  seo <- merge(seo, alldates, by="date", all=TRUE)
  seo[is.na(seo)] <- 0
}

if (nrow(sea)<nrow(alldates)) {
  sea <- merge(sea, alldates, by="date", all=TRUE)
  sea[is.na(sea)] <- 0
}

if (nrow(ns)<nrow(alldates)) {
  ns <- merge(ns, alldates, by="date", all=TRUE)
  ns[is.na(ns)] <- 0
}

if (nrow(direct)<nrow(alldates)) {
  direct <- merge(direct, alldates, by="date", all=TRUE)
  direct[is.na(direct)] <- 0
}

if (nrow(nsi_app)<nrow(alldates)) {
  nsi_app <- merge(nsi_app, alldates, by="date", all=TRUE)
  nsi_app[is.na(nsi_app)] <- 0
}

if (nrow(affiliates)<nrow(alldates)) {
  affiliates <- merge(affiliates, alldates, by="date", all=TRUE)
  affiliates[is.na(affiliates)] <- 0
}

if (nrow(nsi_email)<nrow(alldates)) {
  nsi_email <- merge(nsi_email, alldates, by="date", all=TRUE)
  nsi_email[is.na(nsi_email)] <- 0
}

if (nrow(referral)<nrow(alldates)) {
  referral <- merge(referral, alldates, by="date", all=TRUE)
  referral[is.na(referral)] <- 0
}

if (nrow(retargeting)<nrow(alldates)) {
  retargeting <- merge(retargeting, alldates, by="date", all=TRUE)
  retargeting[is.na(retargeting)] <- 0
}

if (nrow(facebook)<nrow(alldates)) {
  facebook <- merge(facebook, alldates, by="date", all=TRUE)
  facebook[is.na(facebook)] <- 0
}

if (nrow(webvertising)<nrow(alldates)) {
  webvertising <- merge(webvertising, alldates, by="date", all=TRUE)
  webvertising[is.na(webvertising)] <- 0
}

if (nrow(ns_email)<nrow(alldates)) {
  ns_email <- merge(ns_email, alldates, by="date", all=TRUE)
  ns_email[is.na(ns_email)] <- 0
}

#determining last channel other
f_other <- function(data){
  other <- total[,2:5]-sea[,2:5]-ns[,2:5]-retargeting[,2:5]-webvertising[,2:5]-affiliates[,2:5]-seo[,2:5]-direct[,2:5]-facebook[,2:5]-nsi_email[,2:5]-nsi_app[,2:5]-ns_email[,2:5]-referral[,2:5]
  cbind(total[,1], other)
}

other <- f_other(ga_data)
colnames(other)[1] <- "date"

#function in order to remove al NaN's
is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))
}

#extra column for first day of the week. Starting on Monday.
affiliates$week <- as.Date(cut(affiliates$date, breaks = "week", start.on.monday = TRUE))
direct$week <- as.Date(cut(direct$date, breaks = "week", start.on.monday = TRUE))
facebook$week <- as.Date(cut(facebook$date, breaks = "week", start.on.monday = TRUE))
ns$week <- as.Date(cut(ns$date, breaks = "week", start.on.monday = TRUE))
ns_email$week <- as.Date(cut(ns_email$date, breaks = "week", start.on.monday = TRUE))
nsi_app$week <- as.Date(cut(nsi_app$date, breaks = "week", start.on.monday = TRUE))
nsi_email$week <- as.Date(cut(nsi_email$date, breaks = "week", start.on.monday = TRUE))
referral$week <- as.Date(cut(referral$date, breaks = "week", start.on.monday = TRUE))
retargeting$week <- as.Date(cut(retargeting$date, breaks = "week", start.on.monday = TRUE))
sea$week <- as.Date(cut(sea$date, breaks = "week", start.on.monday = TRUE))
seo$week <- as.Date(cut(seo$date, breaks = "week", start.on.monday = TRUE))
total$week <- as.Date(cut(total$date, breaks = "week", start.on.monday = TRUE))
webvertising$week <- as.Date(cut(webvertising$date, breaks = "week", start.on.monday = TRUE))
other$week <- as.Date(cut(other$date, breaks = "week", start.on.monday = TRUE))

#switching columns: putting the week column after the date column
affiliates <- affiliates[, c("date", "week", "sessions", "qualified_traffic", "transactions", "revenue")]
direct <- direct[, c("date", "week", "sessions", "qualified_traffic", "transactions", "revenue")]
facebook <- facebook[, c("date", "week", "sessions", "qualified_traffic", "transactions", "revenue")]
ns <- ns[, c("date", "week", "sessions", "qualified_traffic", "transactions", "revenue")]
ns_email <- ns_email[, c("date", "week", "sessions", "qualified_traffic", "transactions", "revenue")]
nsi_app <- nsi_app[, c("date", "week", "sessions", "qualified_traffic", "transactions", "revenue")]
nsi_email <- nsi_email[, c("date", "week", "sessions", "qualified_traffic", "transactions", "revenue")]
referral <- referral[, c("date", "week", "sessions", "qualified_traffic", "transactions", "revenue")]
retargeting <- retargeting[, c("date", "week", "sessions", "qualified_traffic", "transactions", "revenue")]
sea <- sea[, c("date", "week", "sessions", "qualified_traffic", "transactions", "revenue")]
seo <- seo[, c("date", "week", "sessions", "qualified_traffic", "transactions", "revenue")]
total <- total[, c("date", "week", "sessions", "qualified_traffic", "transactions", "revenue")]
webvertising <- webvertising[, c("date", "week", "sessions", "qualified_traffic", "transactions", "revenue")]
other <- other[, c("date", "week", "sessions", "qualified_traffic", "transactions", "revenue")]

#summing daily data into weekly data
affiliates_weekly <- ddply(affiliates, "week", numcolwise(sum))
direct_weekly <- ddply(direct, "week", numcolwise(sum))
facebook_weekly <- ddply(facebook, "week", numcolwise(sum))
ns_weekly <- ddply(ns, "week", numcolwise(sum))
ns_email_weekly <- ddply(ns_email, "week", numcolwise(sum))
nsi_app_weekly <- ddply(nsi_app, "week", numcolwise(sum))
nsi_email_weekly <- ddply(nsi_email, "week", numcolwise(sum))
referral_weekly <- ddply(referral, "week", numcolwise(sum))
retargeting_weekly <- ddply(retargeting, "week", numcolwise(sum))
sea_weekly <- ddply(sea, "week", numcolwise(sum))
seo_weekly <- ddply(seo, "week", numcolwise(sum))
total_weekly <- ddply(total, "week", numcolwise(sum))
webvertising_weekly <- ddply(webvertising, "week", numcolwise(sum))
other_weekly <- ddply(other, "week", numcolwise(sum))

#creating a new variable conversion(%) by dividing transactions by session
affiliates_weekly$conversion <- affiliates_weekly$transactions/affiliates_weekly$sessions * 100
direct_weekly$conversion <- direct_weekly$transactions/direct_weekly$sessions * 100
facebook_weekly$conversion <- facebook_weekly$transactions/facebook_weekly$sessions * 100
ns_weekly$conversion <- ns_weekly$transactions/ns_weekly$sessions * 100
ns_email_weekly$conversion <- ns_email_weekly$transactions/ns_email_weekly$sessions * 100
nsi_app_weekly$conversion <- nsi_app_weekly$transactions/nsi_app_weekly$sessions * 100
nsi_email_weekly$conversion <- nsi_email_weekly$transactions/nsi_email_weekly$sessions * 100
referral_weekly$conversion <- referral_weekly$transactions/referral_weekly$sessions * 100
retargeting_weekly$conversion <- retargeting_weekly$transactions/retargeting_weekly$sessions * 100
sea_weekly$conversion <- sea_weekly$transactions/sea_weekly$sessions * 100
seo_weekly$conversion <- seo_weekly$transactions/seo_weekly$sessions * 100
total_weekly$conversion <- total_weekly$transactions/total_weekly$sessions * 100
webvertising_weekly$conversion <- webvertising_weekly$transactions/webvertising_weekly$sessions * 100
other_weekly$conversion <- other_weekly$transactions/other_weekly$sessions * 100

#creating a new variable qualified_conversion by dividing transactions by qualified traffic
affiliates_weekly$q_conversion <- affiliates_weekly$transactions/affiliates_weekly$qualified_traffic * 100
direct_weekly$q_conversion <- direct_weekly$transactions/direct_weekly$qualified_traffic * 100
facebook_weekly$q_conversion <- facebook_weekly$transactions/facebook_weekly$qualified_traffic * 100
ns_weekly$q_conversion <- ns_weekly$transactions/ns_weekly$qualified_traffic * 100
ns_email_weekly$q_conversion <- ns_email_weekly$transactions/ns_email_weekly$qualified_traffic * 100
nsi_app_weekly$q_conversion <- nsi_app_weekly$transactions/nsi_app_weekly$qualified_traffic * 100
nsi_email_weekly$q_conversion <- nsi_email_weekly$transactions/nsi_email_weekly$qualified_traffic * 100
referral_weekly$q_conversion <- referral_weekly$transactions/referral_weekly$qualified_traffic * 100
retargeting_weekly$q_conversion <- retargeting_weekly$transactions/retargeting_weekly$qualified_traffic * 100
sea_weekly$q_conversion <- sea_weekly$transactions/sea_weekly$qualified_traffic * 100
seo_weekly$q_conversion <- seo_weekly$transactions/seo_weekly$qualified_traffic * 100
total_weekly$q_conversion <- total_weekly$transactions/total_weekly$qualified_traffic * 100
webvertising_weekly$q_conversion <- webvertising_weekly$transactions/webvertising_weekly$qualified_traffic * 100
other_weekly$q_conversion <- other_weekly$transactions/other_weekly$qualified_traffic * 100

#creating a new variable aov by dividing revenue by transactions
affiliates_weekly$aov <- affiliates_weekly$revenue/affiliates_weekly$transactions
direct_weekly$aov  <- direct_weekly$revenue/direct_weekly$transactions
facebook_weekly$aov  <- facebook_weekly$revenue/facebook_weekly$transactions
ns_weekly$aov<- ns_weekly$revenue/ns_weekly$transactions
ns_email_weekly$aov <- ns_email_weekly$revenue/ns_email_weekly$transactions
nsi_app_weekly$aov <- nsi_app_weekly$revenue/nsi_app_weekly$transactions
nsi_email_weekly$aov <- nsi_email_weekly$revenue/nsi_email_weekly$transactions
referral_weekly$aov <- referral_weekly$revenue/referral_weekly$transactions
retargeting_weekly$aov <- retargeting_weekly$revenue/retargeting_weekly$transactions
sea_weekly$aov <- sea_weekly$revenue/sea_weekly$transactions
seo_weekly$aov <- seo_weekly$revenue/seo_weekly$transactions
total_weekly$aov <- total_weekly$revenue/total_weekly$transactions
webvertising_weekly$aov <- webvertising_weekly$revenue/webvertising_weekly$transactions
other_weekly$aov <- other_weekly$revenue/other_weekly$transactions

#replacing all NaN's by zero's
affiliates_weekly[is.nan(affiliates_weekly)] <- 0
direct_weekly[is.nan(direct_weekly)] <- 0
facebook_weekly[is.nan(facebook_weekly)] <- 0
ns_weekly[is.nan(ns_weekly)] <- 0
ns_email_weekly[is.nan(ns_email_weekly)] <- 0
nsi_app_weekly[is.nan(nsi_app_weekly)] <- 0
nsi_email_weekly[is.nan(nsi_email_weekly)] <- 0
referral_weekly[is.nan(referral_weekly)] <- 0
retargeting_weekly[is.nan(retargeting_weekly)] <- 0
sea_weekly[is.nan(sea_weekly)] <- 0
seo_weekly[is.nan(seo_weekly)] <- 0
total_weekly[is.nan(total_weekly)] <- 0
webvertising_weekly[is.nan(webvertising_weekly)] <- 0
other_weekly[is.nan(other_weekly)] <- 0

#creating a seperate data frame for the year 2016
#affiliates_weekly_16 <- affiliates_weekly[2:53,]
#direct_weekly_16 <-direct_weekly[2:53,]
#facebook_weekly_16 <- facebook_weekly[2:53,]
#ns_weekly_16 <- ns_weekly[2:53,]
#ns_email_weekly_16 <- ns_email_weekly[2:53,]
#nsi_app_weekly_16 <- nsi_app_weekly[2:53,]
#nsi_email_weekly_16 <- nsi_email_weekly[2:53,]
#referral_weekly_16 <- referral_weekly[2:53,]
#retargeting_weekly_16 <- retargeting_weekly[2:53,]
#sea_weekly_16 <- sea_weekly[2:53,]
#seo_weekly_16 <- seo_weekly[2:53,]
#total_weekly_16 <- total_weekly[2:53,]
#webvertising_weekly_16 <- webvertising_weekly[2:53,]
#other_weekly_16 <- other_weekly[2:53,]

affiliates_weekly_16 <- read.xlsx("ga_data_2016.xlsx", sheetName = "affiliates")
direct_weekly_16 <- read.xlsx("ga_data_2016.xlsx", sheetName = "direct")
facebook_weekly_16 <- read.xlsx("ga_data_2016.xlsx", sheetName = "facebook")
ns_weekly_16 <- read.xlsx("ga_data_2016.xlsx", sheetName = "ns")
ns_email_weekly_16 <- read.xlsx("ga_data_2016.xlsx", sheetName = "ns_email")
nsi_app_weekly_16 <- read.xlsx("ga_data_2016.xlsx", sheetName = "nsi_app")
nsi_email_weekly_16 <- read.xlsx("ga_data_2016.xlsx", sheetName = "nsi_email")
referral_weekly_16 <- read.xlsx("ga_data_2016.xlsx", sheetName = "referral")
retargeting_weekly_16 <- read.xlsx("ga_data_2016.xlsx", sheetName = "retargeting")
sea_weekly_16 <- read.xlsx("ga_data_2016.xlsx", sheetName = "sea")
seo_weekly_16 <- read.xlsx("ga_data_2016.xlsx", sheetName = "seo")
total_weekly_16 <- read.xlsx("ga_data_2016.xlsx", sheetName = "total")
webvertising_weekly_16 <- read.xlsx("ga_data_2016.xlsx", sheetName = "webvertising")
other_weekly_16 <- read.xlsx("ga_data_2016.xlsx", sheetName = "other")

#creating a seperate data frame for the year 2017
#affiliates_weekly_17 <- affiliates_weekly[-(1:53),]
#direct_weekly_17 <-direct_weekly[-(1:53),]
#facebook_weekly_17 <- facebook_weekly[-(1:53),]
#ns_weekly_17 <- ns_weekly[-(1:53),]
#ns_email_weekly_17 <- ns_email_weekly[-(1:53),]
#nsi_app_weekly_17 <- nsi_app_weekly[-(1:53),]
#nsi_email_weekly_17 <- nsi_email_weekly[-(1:53),]
#referral_weekly_17 <- referral_weekly[-(1:53),]
#retargeting_weekly_17 <- retargeting_weekly[-(1:53),]
#sea_weekly_17 <- sea_weekly[-(1:53),]
#seo_weekly_17 <- seo_weekly[-(1:53),]
#total_weekly_17 <- total_weekly[-(1:53),]
#webvertising_weekly_17 <- webvertising_weekly[-(1:53),]
#other_weekly_17 <- other_weekly[-(1:53),]

affiliates_weekly_17 <- affiliates_weekly
direct_weekly_17 <- direct_weekly
facebook_weekly_17<- facebook_weekly
ns_weekly_17 <- ns_weekly
ns_email_weekly_17 <- ns_email_weekly
nsi_app_weekly_17 <- nsi_app_weekly
nsi_email_weekly_17 <- nsi_email_weekly
referral_weekly_17 <- referral_weekly
retargeting_weekly_17 <- retargeting_weekly
sea_weekly_17 <- sea_weekly
seo_weekly_17 <- seo_weekly
total_weekly_17 <- total_weekly
webvertising_weekly_17 <- webvertising_weekly
other_weekly_17 <- other_weekly

#replacing dates with weeknumbers for 2016
weeknumbers <- seq(1,52,1)
#total_weekly_16$week<- weeknumbers
#affiliates_weekly_16$week <- weeknumbers
#direct_weekly_16$week <- weeknumbers
#facebook_weekly_16$week <- weeknumbers
#ns_weekly_16$week <- weeknumbers
#ns_email_weekly_16$week <- weeknumbers
#nsi_app_weekly_16$week <- weeknumbers
#nsi_email_weekly_16$week <- weeknumbers
#referral_weekly_16$week <- weeknumbers
#retargeting_weekly_16$week <- weeknumbers
#sea_weekly_16$week <- weeknumbers
#seo_weekly_16$week <- weeknumbers
#total_weekly_16$week <- weeknumbers
#webvertising_weekly_16$week <- weeknumbers
#other_weekly_16$week <- weeknumbers

#replacing dates with weeknumbers for 2017
total_weekly_17$week <- weeknumbers[1:length(total_weekly_17[,1])]
affiliates_weekly_17$week <- weeknumbers[1:length(affiliates_weekly_17[,1])]
direct_weekly_17$week <-weeknumbers[1:length(direct_weekly_17[,1])]
facebook_weekly_17$week <- weeknumbers[1:length(facebook_weekly_17[,1])]
ns_weekly_17$week <- weeknumbers[1:length(ns_weekly_17[,1])]
ns_email_weekly_17$week <- weeknumbers[1:length(ns_email_weekly_17[,1])]
nsi_app_weekly_17$week <- weeknumbers[1:length(nsi_app_weekly_17[,1])]
nsi_email_weekly_17$week <- weeknumbers[1:length(nsi_email_weekly_17[,1])]
referral_weekly_17$week <- weeknumbers[1:length(referral_weekly_17[,1])]
retargeting_weekly_17$week <- weeknumbers[1:length(retargeting_weekly_17[,1])]
sea_weekly_17$week <- weeknumbers[1:length(sea_weekly_17[,1])]
seo_weekly_17$week <- weeknumbers[1:length(seo_weekly_17[,1])]
total_weekly_17$week <- weeknumbers[1:length(total_weekly_17[,1])]
webvertising_weekly_17$week <- weeknumbers[1:length(webvertising_weekly_17[,1])]
other_weekly_17$week <- weeknumbers[1:length(other_weekly_17[,1])]

#reading in targets
targets <- read.xlsx("targets.xlsx", 2)

#putting all data in one list, so that it can be accessed through the selector in the ui
#endgame <- list("affiliates" = affiliates_weekly, "direct" = direct_weekly, "facebook" = facebook_weekly, "ns" = ns_weekly,
#                "ns_email" = ns_email_weekly, "nsi_app" = nsi_app_weekly, "nsi_email" = nsi_email_weekly,"referral" = referral_weekly,
#                "retargeting" = retargeting_weekly, "sea" = sea_weekly,"seo" = seo_weekly, "total" = total_weekly, "webvertising" = webvertising_weekly, "other" = other_weekly)

endgame_2017 <- list("affiliates" = affiliates_weekly_17, "direct" = direct_weekly_17, "facebook" = facebook_weekly_17, "ns" = ns_weekly_17,
                     "ns_email" = ns_email_weekly_17, "nsi_app"= nsi_app_weekly_17, "nsi_email"= nsi_email_weekly_17,"referral" = referral_weekly_17,
                     "retargeting" = retargeting_weekly_17, "sea" = sea_weekly_17,"seo" = seo_weekly_17, "total" = total_weekly_17, "webvertising" = webvertising_weekly_17, "other" = other_weekly_17)

endgame_2016 <- list("affiliates" = affiliates_weekly_16, "direct" = direct_weekly_16, "facebook" = facebook_weekly_16, "ns" = ns_weekly_16,
                     "ns_email" = ns_email_weekly_16, "nsi_app"= nsi_app_weekly_16, "nsi_email"= nsi_email_weekly_16,"referral" = referral_weekly_16,
                     "retargeting" = retargeting_weekly_16, "sea" = sea_weekly_16,"seo" = seo_weekly_16, "total" = total_weekly_16, "webvertising" = webvertising_weekly_16, "other" = other_weekly_16)


shinyServer(function(input, output) {
  output$traffic_plot <- renderPlot(
    if (input$traffic_channel == "total"){
      ggplot(data = endgame_2017[[input$traffic_channel]], aes(x = week, y = sessions, fill = "lightskyblue")) +
        geom_bar(stat =  "identity", color = "black") +
        geom_line(data = targets, aes(x = week, y = t_target, color = "red"), size = 1) +
        geom_line(data = endgame_2016[[input$traffic_channel]], aes(x = week, y = sessions, color = "green"), size = 1) +
        labs(x = "Week", y = "Verkeer") +
        scale_x_discrete(limits = total_weekly_16$week) +
        scale_y_continuous(breaks = seq(from = 0, to = max(endgame_2016[[input$traffic_channel]]) + 5000, by = 20000)) +
        scale_fill_identity(name = NULL, guide = 'legend', labels = c('2017')) +
        scale_colour_manual(name = NULL, values = c("red" = " red" , "green" = "green"), labels = c("PY", "Target"))
    } else{
      ggplot(data = endgame_2017[[input$traffic_channel]], aes(x = week, y = sessions, fill = "lightskyblue")) +
        geom_bar(stat =  "identity", color = "black") +
        geom_line(data = endgame_2016[[input$traffic_channel]], aes(x = week, y = sessions, color = "green"), size = 1) +
        scale_x_discrete(limits = total_weekly_16$week) +
        scale_y_continuous(breaks = seq(from = 0, to = max(endgame_2016[[input$traffic_channel]]) + 5000, by = 5000)) +
        scale_fill_identity(name = NULL, guide = 'legend', labels = c('2017')) +
        scale_colour_manual(name = NULL, values = c("green" = "green"), labels = c("PY"))
    }
  )
  
  output$sessions_targets <- renderValueBox({
    if (input$traffic_channel == "total"){
      week = tail(endgame_2017[[input$traffic_channel]]$week, n = 1)
      if (endgame_2017[[input$traffic_channel]]$sessions[week]/targets$t_target[week] < 0.995) {
        valueBox(
          round(endgame_2017[[input$traffic_channel]]$sessions[week]/targets$t_target[week] * 100),
          subtitle = "Realisatie vs. target",
          color = "red")
      } else{
        valueBox(
          round(endgame_2017[[input$traffic_channel]]$sessions[week]/targets$t_target[week] * 100),
          subtitle = "Realisatie vs. target",
          color = "green")
      }
    } else{
      valueBox("Geen target", "Realisatie vs. target")
    }
  })
  
  output$sessions_py <- renderValueBox({
    week = tail(endgame_2017[[input$traffic_channel]]$week, n = 1)
    if (endgame_2017[[input$traffic_channel]]$sessions[week]/endgame_2016[[input$traffic_channel]]$sessions[week] < 0.995){
      valueBox(
        round(endgame_2017[[input$traffic_channel]]$sessions[week]/endgame_2016[[input$traffic_channel]]$sessions[week]*100),
        subtitle = "Realisatie vs. vorig jaar",
        color = "red")
    }  else{
      valueBox(
        round(endgame_2017[[input$traffic_channel]]$sessions[week]/endgame_2016[[input$traffic_channel]]$sessions[week]*100),
        subtitle = "Realisatie vs. vorig jaar",
        color = "green")
    }
  })
  
  output$sessions_pw <- renderValueBox({
    week = tail(endgame_2017[[input$traffic_channel]]$week, n = 1)
    if (endgame_2017[[input$traffic_channel]]$sessions[week]/endgame_2017[[input$traffic_channel]]$sessions[week-1] < 0.995){
      valueBox(
        round(endgame_2017[[input$traffic_channel]]$sessions[week]/endgame_2017[[input$traffic_channel]]$sessions[week-1]*100),
        subtitle = "Realisatie vs. vorige week",
        color = "red")
    } else{
      valueBox(
        round(endgame_2017[[input$traffic_channel]]$sessions[week]/endgame_2017[[input$traffic_channel]]$sessions[week-1]*100),
        subtitle = "Realisatie vs. vorige week",
        color = "green")
    }
  })
  
  output$qt_plot <- renderPlot(
    if (input$qt_channel == "total"){
      ggplot(data = endgame_2017[[input$qt_channel]], aes(x = week, y = qualified_traffic, fill = "lightskyblue")) +
        geom_bar(stat =  "identity", color = "black") +
        geom_line(data = targets, aes(x = week, y = qt_target, color = "red"), size = 1) +
        geom_line(data = endgame_2016[[input$qt_channel]], aes(x = week, y = qualified_traffic, color = "green"), size = 1) +
        labs(x = "Week", y = "Qualified verkeer") +
        scale_x_discrete(limits = total_weekly_16$week) +
        scale_y_continuous(breaks = seq(from = 0, to = max(endgame_2016[[input$traffic_channel]]) + 5000, by = 20000)) +
        scale_fill_identity(name = NULL, guide = 'legend',labels = c('2017')) +
        scale_colour_manual(name = NULL, values =c('red'='red','green'='green'), labels = c("PY", "Target"))
    } else{
      ggplot(data = endgame_2017[[input$qt_channel]], aes(x = week, y = qualified_traffic, fill = "lightskyblue")) +
        geom_bar(stat =  "identity", color = "black") +
        geom_line(data = endgame_2016[[input$qt_channel]], aes(x = week, y = qualified_traffic, color = "green"), size = 1) +
        labs(x = "Week", y = "Qualified verkeer") +
        scale_x_discrete(limits = total_weekly_16$week) +
        scale_y_continuous(breaks = seq(from = 0, to = max(endgame_2016[[input$qt_channel]]) + 5000, by = 5000)) +
        scale_fill_identity(name = NULL, guide = 'legend',labels = c('2017')) +
        scale_colour_manual(name = NULL, values =c('green'='green'), labels = c("PY"))
    }
  )
  
  output$qt_targets <- renderValueBox({
    if (input$qt_channel == "total"){
      week = tail(endgame_2017[[input$qt_channel]]$week, n = 1)
      if (endgame_2017[[input$qt_channel]]$qualified_traffic[week]/targets$qt_target[week] < 0.995) {
        valueBox(
          round(endgame_2017[[input$qt_channel]]$qualified_traffic[week]/targets$qt_target[week] * 100),
          subtitle = "Realisatie vs. target",
          color = "red")
      } else{
        valueBox(
          round(endgame_2017[[input$qt_channel]]$qualified_traffic[week]/targets$qt_target[week] * 100),
          subtitle = "Realisatie vs. target",
          color = "green")
      }
    } else{
      valueBox("Geen target", "Realisatie vs. target")
    }
  })
  
  output$qt_py <- renderValueBox({
    week = tail(endgame_2017[[input$qt_channel]]$week, n = 1)
    if (endgame_2017[[input$qt_channel]]$qualified_traffic[week]/endgame_2016[[input$qt_channel]]$qualified_traffic[week] < 0.995){
      valueBox(
        round(endgame_2017[[input$qt_channel]]$qualified_traffic[week]/endgame_2016[[input$qt_channel]]$qualified_traffic[week]*100),
        subtitle = "Realisatie vs. vorig jaar",
        color = "red")
    }  else{
      valueBox(
        round(endgame_2017[[input$qt_channel]]$qualified_traffic[week]/endgame_2016[[input$qt_channel]]$qualified_traffic[week]*100),
        subtitle = "Realisatie vs. vorig jaar",
        color = "green")
    }
  })
  
  output$qt_pw <- renderValueBox({
    week = tail(endgame_2017[[input$qt_channel]]$week, n = 1)
    if (endgame_2017[[input$qt_channel]]$qualified_traffic[week]/endgame_2017[[input$qt_channel]]$qualified_traffic[week-1] < 0.995){
      valueBox(
        round(endgame_2017[[input$qt_channel]]$qualified_traffic[week]/endgame_2017[[input$qt_channel]]$qualified_traffic[week-1]*100),
        subtitle = "Realisatie vs. vorige week",
        color = "red")
    } else{
      valueBox(
        round(endgame_2017[[input$qt_channel]]$qualified_traffic[week]/endgame_2017[[input$qt_channel]]$qualified_traffic[week-1]*100),
        subtitle = "Realisatie vs. vorige week",
        color = "green")
    }
  })
  
  output$c_plot <- renderPlot(
    ggplot(data = endgame_2017[[input$c_channel]], aes(x = week, y = conversion, fill = "lightskyblue"))
    + geom_bar(stat =  "identity", color = "black")
    + geom_line(data = targets, aes(x = week, y = c_target*100, color = "red"), size = 1)
    + geom_line(data = endgame_2016[[input$c_channel]], aes(x = week, y = conversion, color = "green"), size = 1)
    + labs(x = "Week", y = "Conversie")
    + scale_x_discrete(limits = total_weekly_16$week)
    + scale_y_continuous(breaks = seq(from = 0, to = 8, by = 0.5))
    + scale_fill_identity(name = NULL, guide = 'legend',labels = c('2017'))
    + scale_colour_manual(name = NULL, values =c('red'='red','green'='green'), labels = c("PY", "Target"))
  )
  
  output$c_targets <- renderValueBox({
    if (input$c_channel == "total"){
      week = tail(endgame_2017[[input$c_channel]]$week, n = 1)
      if (endgame_2017[[input$c_channel]]$conversion[week]/targets$c_target[week] < 99.5) {
        valueBox(
          round(endgame_2017[[input$c_channel]]$conversion[week]/targets$c_target[week]),
          subtitle = "Realisatie vs. target",
          color = "red")
      } else{
        valueBox(
          round(endgame_2017[[input$c_channel]]$conversion[week]/targets$c_target[week]),
          subtitle = "Realisatie vs. target",
          color = "green")
      }
    } else{
      valueBox("Geen target", "Realisatie vs. target")
    }
  })
  
  output$c_py <- renderValueBox({
    week = tail(endgame_2017[[input$c_channel]]$week, n = 1)
    if (endgame_2017[[input$c_channel]]$conversion[week]/endgame_2016[[input$c_channel]]$conversion[week] < 0.995){
      valueBox(
        round(endgame_2017[[input$c_channel]]$conversion[week]/endgame_2016[[input$c_channel]]$conversion[week]*100),
        subtitle = "Realisatie vs. vorig jaar",
        color = "red")
    }  else{
      valueBox(
        round(endgame_2017[[input$c_channel]]$conversion[week]/endgame_2016[[input$c_channel]]$conversion[week]*100),
        subtitle = "Realisatie vs. vorig jaar",
        color = "green")
    }
  })
  
  output$c_pw <- renderValueBox({
    week = tail(endgame_2017[[input$c_channel]]$week, n = 1)
    if (endgame_2017[[input$c_channel]]$conversion[week]/endgame_2017[[input$c_channel]]$conversion[week-1] < 0.995){
      valueBox(
        round(endgame_2017[[input$c_channel]]$conversion[week]/endgame_2017[[input$c_channel]]$conversion[week-1]*100),
        subtitle = "Realisatie vs. vorige week",
        color = "red")
    } else{
      valueBox(
        round(endgame_2017[[input$c_channel]]$conversion[week]/endgame_2017[[input$c_channel]]$conversion[week-1]*100),
        subtitle = "Realisatie vs. vorige week",
        color = "green")
    }
  })
  
  output$qc_plot <- renderPlot(
    if (input$qc_channel != "nsi_app"){
      ggplot(data = endgame_2017[[input$qc_channel]], aes(x = week, y = q_conversion, fill = "lightskyblue")) +
        geom_bar(stat =  "identity", color = "black") +
        geom_line(data = targets, aes(x = week, y = qc_target*100, color = "red"), size = 1) +
        geom_line(data = endgame_2016[[input$qc_channel]], aes(x = week, y = q_conversion, color = "green"), size = 1) +
        labs(x = "Week", y = "Qualified conversie") +
        scale_x_discrete(limits = total_weekly_16$week) +
        scale_y_continuous(breaks= seq(from = 0, to = 40, by = 1)) +
        scale_fill_identity(name = NULL, guide = 'legend',labels = c('2017')) +
        scale_colour_manual(name = NULL, values =c('red'='red', 'green'='green'), labels = c("PY", "Target"))
    } else{
      ggplot(data = endgame_2017[[input$qc_channel]], aes(x = week, y = q_conversion, fill = "lightskyblue")) +
        geom_bar(stat =  "identity", color = "black") +
        geom_line(data = targets, aes(x = week, y = qc_target*100, color = "red"), size = 1) +
        geom_line(data = endgame_2016[[input$qc_channel]], aes(x = week, y = q_conversion, color = "green"), size = 1) +
        labs(x = "Week", y = "Qualified conversie") +
        scale_x_discrete(limits = total_weekly_16$week) +
        scale_y_continuous(breaks= seq(from = 0, to = 60, by = 2)) +
        scale_fill_identity(name = NULL, guide = 'legend',labels = c('2017')) +
        scale_colour_manual(name = NULL, values =c('red'='red', 'green'='green'), labels = c("PY", "Target"))
    }
  )
  output$qc_targets <- renderValueBox({
    if (input$qc_channel == "total"){
      week = tail(endgame_2017[[input$qc_channel]]$week, n = 1)
      if (endgame_2017[[input$qc_channel]]$q_conversion[week]/targets$qc_target[week] < 0.995) {
        valueBox(
          round(endgame_2017[[input$qc_channel]]$q_conversion[week]/targets$qc_target[week]),
          subtitle = "Realisatie vs. target",
          color = "red")
      } else{
        valueBox(
          round(endgame_2017[[input$qc_channel]]$q_conversion[week]/targets$qc_target[week]),
          subtitle = "Realisatie vs. target",
          color = "green")
      }
    } else{
      valueBox("Geen target", "Realisatie vs. target")
    }
  })
  
  output$qc_py <- renderValueBox({
    week = tail(endgame_2017[[input$qc_channel]]$week, n = 1)
    if (endgame_2017[[input$qc_channel]]$q_conversion[week]/endgame_2016[[input$qc_channel]]$q_conversion[week] < 0.995){
      valueBox(
        round(endgame_2017[[input$qc_channel]]$q_conversion[week]/endgame_2016[[input$qc_channel]]$q_conversion[week]*100),
        subtitle = "Realisatie vs. vorig jaar",
        color = "red")
    }  else{
      valueBox(
        round(endgame_2017[[input$qc_channel]]$q_conversion[week]/endgame_2016[[input$qc_channel]]$q_conversion[week]*100),
        subtitle = "Realisatie vs. vorig jaar",
        color = "green")
    }
  })
  
  output$qc_pw <- renderValueBox({
    week = tail(endgame_2017[[input$qc_channel]]$week, n = 1)
    if (endgame_2017[[input$qc_channel]]$q_conversion[week]/endgame_2017[[input$qc_channel]]$q_conversion[week-1] < 0.995){
      valueBox(
        round(endgame_2017[[input$qc_channel]]$q_conversion[week]/endgame_2017[[input$qc_channel]]$q_conversion[week-1]*100),
        subtitle = "Realisatie vs. vorige week",
        color = "red")
    } else{
      valueBox(
        round(endgame_2017[[input$qc_channel]]$q_conversion[week]/endgame_2017[[input$qc_channel]]$q_conversion[week-1]*100),
        subtitle = "Realisatie vs. vorige week",
        color = "green")
    }
  })
  
  output$trans_plot <- renderPlot(
    if (input$trans_channel == "total"){
      ggplot(data = endgame_2017[[input$trans_channel]], aes(x = week, y = transactions, fill = "lightskyblue")) +
        geom_bar(stat =  "identity", color = "black") +
        geom_line(data = targets, aes(x = week, y = trans_target, color = "red"), size = 1) +
        geom_line(data = endgame_2016[[input$trans_channel]], aes(x = week, y = transactions, color = "green"), size = 1) +
        labs(x = "Week", y = "Transacties") +
        scale_x_discrete(limits= total_weekly_16$week) +
        scale_y_continuous(breaks= seq(from = 0, to = 15000, by = 1000)) +
        scale_fill_identity(name = NULL, guide = 'legend',labels = c('2017')) +
        scale_colour_manual(name = NULL, values = c('red'='red', 'green'='green'), labels = c("PY", "Target"))
    }else{
      ggplot(data = endgame_2017[[input$trans_channel]], aes(x = week, y = transactions, fill = "lightskyblue")) +
        geom_bar(stat =  "identity", color = "black") +
        geom_line(data = endgame_2016[[input$trans_channel]], aes(x = week, y = transactions, color = "green"), size = 1) +
        labs(x = "Week", y = "Transacties") +
        scale_x_discrete(limits= total_weekly_16$week) +
        scale_y_continuous(breaks= seq(from = 0, to = 15000, by = 1000)) +
        scale_fill_identity(name = NULL, guide = 'legend',labels = c('2017')) +
        scale_colour_manual(name = NULL, values = c('green'='green'), labels = c("PY"))
    }
  )
  
  output$trans_targets <- renderValueBox({
    if (input$trans_channel == "total"){
      week = tail(endgame_2017[[input$trans_channel]]$week, n = 1)
      if (endgame_2017[[input$trans_channel]]$transactions[week]/targets$trans_target[week] < 0.995) {
        valueBox(
          round(endgame_2017[[input$trans_channel]]$transactions[week]/targets$trans_target[week] * 100),
          subtitle = "Realisatie vs. target",
          color = "red")
      } else{
        valueBox(
          round(endgame_2017[[input$trans_channel]]$transactions[week]/targets$trans_target[week] * 100),
          subtitle = "Realisatie vs. target",
          color = "green")
      }
    } else{
      valueBox("Geen target", "Realisatie vs target")
    }
  })
  
  output$trans_py <- renderValueBox({
    week = tail(endgame_2017[[input$trans_channel]]$week, n = 1)
    if (endgame_2017[[input$trans_channel]]$transactions[week]/endgame_2016[[input$trans_channel]]$transactions[week] < 0.995){
      valueBox(
        round(endgame_2017[[input$trans_channel]]$transactions[week]/endgame_2016[[input$trans_channel]]$transactions[week]*100),
        subtitle = "Realisatie vs. vorig jaar",
        color = "red")
    }  else{
      valueBox(
        round(endgame_2017[[input$trans_channel]]$transactions[week]/endgame_2016[[input$trans_channel]]$transactions[week]*100),
        subtitle = "Realisatie vs. vorig jaar",
        color = "green")
    }
  })
  
  output$trans_pw <- renderValueBox({
    week = tail(endgame_2017[[input$trans_channel]]$week, n = 1)
    if (endgame_2017[[input$trans_channel]]$transactions[week]/endgame_2017[[input$trans_channel]]$transactions[week-1] < 0.995){
      valueBox(
        round(endgame_2017[[input$trans_channel]]$transactions[week]/endgame_2017[[input$trans_channel]]$transactions[week-1]*100),
        subtitle = "Realisatie vs. vorige week",
        color = "red")
    } else{
      valueBox(
        round(endgame_2017[[input$trans_channel]]$transactions[week]/endgame_2017[[input$trans_channel]]$transactions[week-1]*100),
        subtitle = "Realisatie vs. vorige week",
        color = "green")
    }
  })
  
  output$aov_plot <- renderPlot(
    ggplot(data = endgame_2017[[input$aov_channel]], aes(x = week, y = aov, fill = "lightskyblue")) +
      geom_bar(stat = "identity", color = "black") +
      geom_line(data = targets, aes(x = week, y = aov_target, color = "red"), size = 1) +
      geom_line(data = endgame_2016[[input$aov_channel]], aes(x = week, y = aov, color = "green"), size = 1) +
      labs(x = "Week", y = "AOV") +
      scale_x_discrete(limits= total_weekly_16$week) +
      scale_y_continuous(breaks= seq(from = 0, to = 120, by = 5)) +
      scale_fill_identity(name = NULL, guide = 'legend',labels = c('2017')) +
      scale_colour_manual(name = NULL, values =c('red'='red', 'green'='green'), labels = c("PY", "Target"))
  )
  
  output$aov_targets <- renderValueBox({
    if (input$aov_channel == "total"){
      week = tail(endgame_2017[[input$aov_channel]]$week, n = 1)
      if (endgame_2017[[input$aov_channel]]$aov[week]/targets$aov_target[week] < 0.995) {
        valueBox(
          round(endgame_2017[[input$aov_channel]]$aov[week]/targets$aov_target[week] * 100),
          subtitle = "Realisatie vs. target",
          color = "red")
      } else{
        valueBox(
          round(endgame_2017[[input$aov_channel]]$aov[week]/targets$aov_target[week] * 100),
          subtitle = "Realisatie vs. target",
          color = "green")
      }
    } else{
      valueBox("Geen target", "Realisatie vs. target")
    }
  })
  
  output$aov_py <- renderValueBox({
    week = tail(endgame_2017[[input$aov_channel]]$week, n = 1)
    if (endgame_2017[[input$aov_channel]]$aov[week]/endgame_2016[[input$aov_channel]]$aov[week] < 0.995){
      valueBox(
        round(endgame_2017[[input$aov_channel]]$aov[week]/endgame_2016[[input$aov_channel]]$aov[week]*100),
        subtitle = "Realisatie vs. vorig jaar",
        color = "red")
    }  else{
      valueBox(
        round(endgame_2017[[input$aov_channel]]$aov[week]/endgame_2016[[input$aov_channel]]$aov[week]*100),
        subtitle = "Realisatie vs. vorig jaar",
        color = "green")
    }
  })
  
  output$aov_pw <- renderValueBox({
    week = tail(endgame_2017[[input$aov_channel]]$week, n = 1)
    if (endgame_2017[[input$aov_channel]]$aov[week]/endgame_2017[[input$aov_channel]]$aov[week-1] < 0.995){
      valueBox(
        round(endgame_2017[[input$aov_channel]]$aov[week]/endgame_2017[[input$aov_channel]]$aov[week-1]*100),
        subtitle = "Realisatie vs. vorige week",
        color = "red")
    } else{
      valueBox(
        round(endgame_2017[[input$aov_channel]]$aov[week]/endgame_2017[[input$aov_channel]]$aov[week-1]*100),
        subtitle = "Realisatie vs. vorige week",
        color = "green")
    }
  })
  
  output$rev_plot <- renderPlot(
    if (input$rev_channel == "total"){
      ggplot(data = endgame_2017[[input$rev_channel]], aes(x = week, y = revenue, fill = "lightskyblue")) +
        geom_bar(stat =  "identity", color = "black") +
        geom_line(data = targets, aes(x = week, y = rev_target, color = "red"), size = 1) +
        geom_line(data = endgame_2016[[input$rev_channel]], aes(x = week, y = revenue, color = "green"), size = 1) +
        labs(x = "Week", y = "Revenue") +
        scale_x_discrete(limits = total_weekly_16$week) +
        scale_y_continuous(breaks= seq(from = 0, to = 2000000, by = 100000)) +
        scale_fill_identity(name = NULL, guide = 'legend', labels = c('2017')) +
        scale_colour_manual(name = NULL, values =c('red'='red', 'green'='green'), labels = c("PY", "Target"))
    }else{
      ggplot(data = endgame_2017[[input$rev_channel]], aes(x = week, y = revenue, fill = "lightskyblue")) +
        geom_bar(stat =  "identity", color = "black") +
        geom_line(data = endgame_2016[[input$rev_channel]], aes(x = week, y = revenue, color = "green"), size = 1) +
        labs(x = "Week", y = "Revenue") +
        scale_x_discrete(limits = total_weekly_16$week) +
        scale_y_continuous(breaks= seq(from = 0, to = max(endgame_2017[[input$rev_channel]]) + 25000, by = 25000)) +
        scale_fill_identity(name = NULL, guide = 'legend',labels = c('2017')) +
        scale_colour_manual(name = NULL, values =c('green'='green'), labels = c("PY"))
    }
  )
  
  output$rev_targets <- renderValueBox({
    if (input$rev_channel == "total"){
      week = tail(endgame_2017[[input$rev_channel]]$week, n = 1)
      if (endgame_2017[[input$rev_channel]]$revenue[week]/targets$rev_target[week] < 0.995) {
        valueBox(
          round(endgame_2017[[input$rev_channel]]$revenue[week]/targets$rev_target[week] * 100),
          subtitle = "Realisatie vs. target",
          color = "red")
      } else{
        valueBox(
          round(endgame_2017[[input$rev_channel]]$revenue[week]/targets$rev_target[week] * 100),
          subtitle = "Realisatie vs. target",
          color = "green")
      }
    } else{
      valueBox("Geen target", "Realisatie vs. target")
    }
  })
  
  output$rev_py <- renderValueBox({
    week = tail(endgame_2017[[input$rev_channel]]$week, n = 1)
    if (endgame_2017[[input$rev_channel]]$revenue[week]/endgame_2016[[input$rev_channel]]$revenue[week] < 0.995){
      valueBox(
        round(endgame_2017[[input$rev_channel]]$revenue[week]/endgame_2016[[input$rev_channel]]$revenue[week]*100),
        subtitle = "Realisatie vs vorig jaar",
        color = "red")
    }  else{
      valueBox(
        round(endgame_2017[[input$rev_channel]]$revenue[week]/endgame_2016[[input$rev_channel]]$revenue[week]*100),
        subtitle = "Realisatie vs. vorig jaar",
        color = "green")
    }
  })
  
  output$rev_pw <- renderValueBox({
    week = tail(endgame_2017[[input$rev_channel]]$week, n = 1)
    if (endgame_2017[[input$rev_channel]]$revenue[week]/endgame_2017[[input$rev_channel]]$revenue[week-1] < 0.995){
      valueBox(
        round(endgame_2017[[input$rev_channel]]$revenue[week]/endgame_2017[[input$rev_channel]]$revenue[week-1]*100),
        subtitle = "Realisatie vs. vorige week",
        color = "red")
    } else{
      valueBox(
        round(endgame_2017[[input$rev_channel]]$revenue[week]/endgame_2017[[input$rev_channel]]$revenue[week-1]*100),
        subtitle = "Realisatie vs. vorige week",
        color = "green")
    }
  })
})



