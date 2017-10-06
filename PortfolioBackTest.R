# Andres Soto, September 2017
# Important note, trying this with a data file of 100 tickers, will need to to this with a a file of about ~5,000 tickers eventually

# TODO:
# This is a small cap version,
# Adjust as needed for all cap/large cap... or create other files....

# For general data manipulation
library(dplyr)
library(lubridate)
library(data.table)

# We will need to look at various time periods of analysis - change as desired
startDate <- checkDate(ymd("2010-01-01"))
endDate <- checkDate(ymd("2016-12-31"))
holdingInterval <- 4  # represents rebalancing every 4 weeks....


# read data from CSV file
# valid files are BackTestData and BackTestDataSmallCap
d <- fread("~/R/BackTestDataSmallCap2.csv")

# convert column of dates to date objects
#data$VCMYMD <- parse_date_time(data$VCMYMD, c("%y%m%d"))
d <- d %>% 
  mutate(VCMYMD = ymd(VCMYMD),
         VCMEquityIdentifier = trimws(VCMEquityIdentifier),
         ticker = VCMEquityIdentifier)
d$V1 <- NULL

# perhaps a list of tickers will come in handy....
#tickers <- unique(d$ticker)

# pull unique values out of column of dates....
# I can get away with this because the database is very well maintained.  Most dates are Fridays (exceptions for holidays....)
# dates <- unique(data$VCMYMD)
dates <- d %>% distinct(VCMYMD)


#######################################
############## FUNCTIONS ##############

# Checks dates and assigns closest valid date
checkDate <- function(testDate){
  # algorithm to find the closest valid date
  if (testDate %in% dates$VCMYMD) {
    return(testDate)
  } else {
    date.before <- dates %>%
      filter(VCMYMD < testDate) %>%
      summarize(the_date=max(VCMYMD)) 
    
    date.after <- dates %>%
      filter(VCMYMD > testDate) %>%
      summarize(the_date=min(VCMYMD)) 
    
    days.before <- as.numeric(testDate - date.before$the_date)
    days.after <- as.numeric(ymd(date.after$the_date) - testDate)
    
    if (days.before < days.after) {
      return(date.before$the_date)
    } else {
      return(date.after$the_date)    
    } # closes nested if...else
  } # closes original if...else
} # closes function


# Calculate the next date
nextDate <- function(dateStart){
  return(checkDate(dateStart + dweeks(holdingInterval)))
}


# Creates portfolio along RSR guideline
portBuild <- function(dateStart, portfolioType){
  stocks <- d[d$VCMYMD %in% dateStart,]
  stocks$V1 <- NULL  # I can't see why I need this so I am eliminating it...
  stocks$VCMEquityIdentifier <- NULL
  
  switch(portfolioType,
  a={
    portfolio <- stocks %>%
      filter(invRank >= 1,
             invRank <= 5,
             MARKET_VAL<1000) #small cap....
  },
  b={
    portfolio <- stocks %>%
      filter(invRank >= 6,
             invRank <= 10,
             MARKET_VAL<1000)
  },
  c={
    portfolio <- stocks %>%
      filter(invRank >= 11,
             invRank <= 15,
             MARKET_VAL<1000)
  },
  d={
    portfolio <- stocks %>%
      filter(invRank >= 16,
             invRank <= 20,
             MARKET_VAL<1000)
  },
  e={
    portfolio <- stocks %>%
      filter(invRank >= 21,
             invRank <= 25,
             MARKET_VAL<1000)
  },
  f={
    portfolio <- stocks %>%
      filter(invRank >= 26,
             invRank <= 30,
             MARKET_VAL<1000)
  },
  {
    print('Invalid portfolio type')
  }
  )
  
  # add weights, naive portfolio
  #interesting gotcha here, sometimes there are no stocks in a particular bucket
  if (nrow(portfolio)!=0){
    portfolio$weight <- 1/nrow(portfolio)
  } 
  
  return(portfolio)
}


returnCalc <- function(portfolio, dateStart, dateEnd){
  # put a catch in here... if portfolio==-1, then return 1
  if (nrow(portfolio)==0) {return(1)} else {
    temp <- copy(portfolio)
    tickers <- temp %>% distinct(ticker)
    stocks <- d[d$VCMYMD %in% dateEnd,]
    stocks$V1 <- NULL 
    stocks$VCMEquityIdentifier <- NULL
    stocks <- stocks[,c("ticker", "price")]
    colnames(stocks)[colnames(stocks)=="price"] <- "endPrice"
    
    temp <- merge(temp,stocks, all.x=TRUE)
    temp$return <- temp$endPrice/temp$price
    
    return(sum(temp$return*temp$weight))
  }
  
  
}


calcCAGR <- function(NAV){
  start <- head(NAV,1)
  end <- tail(NAV,1)
  years <- (as.numeric(end[[1,1]] - start[[1,1]]))/365
  start$date <- NULL
  end$date <- NULL
  
  result <- ((end/start)^(1/years)-1)*100
  result <- data.table(years=years, result)
  setnames(result, old=c("years","a","b","c","d","e","f"), new=c("years","1-5", "6-10", "11-15", "16-20", "21-25","26-30"))
  result <- result %>% mutate_each(funs(round(.,2)))
  return(result)
}


######### END OF FUNCTIONS ############
#######################################

# TODO:
# - We need to track 6 portfolios, equally weighted investment, based on RSR values
#       - RSR < 6
#       - 5 < RSR < 11 .... 10 < RSR < 16   ....   15 < RSR < 21
#       - 20 < RSR < 26   ...  25 < RSR < 31
# - the end game is to produce a panel with 6 lines, returns higher for RSR 1-5, returns lower for RSR 15-30
# - Define a function that when given a start date and an end date, calculates the return of the portfolios
# - Define a returns table that stores returns for the 6 portfolios
# - Define a function that calculates CAGR for the portfolios.  Input is returns table

# Using starting value to track return, assuming portX are global variables and functions will be able to modify....
# Changing tack here, going to create a list and use it like a Python dictionary....
#portfolios <- list(a = 10000, # RSR 1-5
#                   b = 10000, # RSR 6-10
#                   c = 10000, # RSR 11-15
#                   d = 10000, # RSR 16-20 
#                   e = 10000, # RSR 21-25
#                   f = 10000) # RSR 26-30

NAV <- data.table(date = startDate, a = 10000, b = 10000, c = 10000, d = 10000, e = 10000, f = 10000)

#TODO:
#  - Set up stacking returns on NAV data table
#  - Set up while loop to cycle though dates
#  - Set up method not to overshoot at the end....

# NAV1 <- data.table(date=currentDate, a=return, b=return, c=return, d=return, e=return, f=return)
# NAV <- rbind(NAV, NAV1)

currentDate <- startDate

while (currentDate < endDate) {
  if(nextDate(currentDate) >= endDate) {
    # populate last line of returns
    monthEnd <- endDate
    lastNAV <- tail(NAV,1)
    lastNAV$date <- NULL
    temp <- data.table(date=monthEnd, a=returnCalc(portBuild(currentDate,'a'), currentDate, monthEnd),
                       b=returnCalc(portBuild(currentDate,'b'), currentDate, monthEnd), 
                       c=returnCalc(portBuild(currentDate,'c'), currentDate, monthEnd), 
                       d=returnCalc(portBuild(currentDate,'d'), currentDate, monthEnd), 
                       e=returnCalc(portBuild(currentDate,'e'), currentDate, monthEnd),
                       f=returnCalc(portBuild(currentDate,'f'), currentDate, monthEnd)
                       ) 
    temp$date <- NULL
    tempNAV <- lastNAV*temp
    tempNAV$date <- monthEnd
    NAV <-rbind(NAV, tempNAV)
    
    currentDate <- endDate
  } else {
    # popluate next line of returns
    monthEnd <- nextDate(currentDate)
    lastNAV <- tail(NAV,1)
    lastNAV$date <- NULL
    temp <- data.table(date=monthEnd, a=returnCalc(portBuild(currentDate,'a'), currentDate, monthEnd),
                       b=returnCalc(portBuild(currentDate,'b'), currentDate, monthEnd), 
                       c=returnCalc(portBuild(currentDate,'c'), currentDate, monthEnd), 
                       d=returnCalc(portBuild(currentDate,'d'), currentDate, monthEnd), 
                       e=returnCalc(portBuild(currentDate,'e'), currentDate, monthEnd),
                       f=returnCalc(portBuild(currentDate,'f'), currentDate, monthEnd)
    ) 
    temp$date <- NULL
    tempNAV <- lastNAV*temp
    tempNAV$date <- monthEnd
    NAV <-rbind(NAV, tempNAV)
    
    currentDate <- nextDate(currentDate)
  } # closes if...else statement
} # closes while loop

# create log returns
temp <- data.table(NAV)
tempDate <- NAV$date

logRets <- data.table(date=tempDate, a=c(NA, diff(log(temp$a), lag = 1)),
                      b=c(NA, diff(log(temp$b), lag = 1)),
                      c=c(NA, diff(log(temp$c), lag = 1)),
                      d=c(NA, diff(log(temp$d), lag = 1)),
                      e=c(NA, diff(log(temp$e), lag = 1)),
                      f=c(NA, diff(log(temp$f), lag = 1))
                      )

logRets <- logRets[-1]
