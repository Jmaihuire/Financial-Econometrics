
## uncomment lines 3-7 and install R packages
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("readr")
# install.packages("TSA") 
# install.packages("ggpubr")

## load R packages
library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(TSA)
library(gridExtra)
library(scales)
library(stats)

## load and manipulate data 
# setwd("/Users/ouyangfu/Dropbox/Teaching/NKU FINA 2078_2018/data")        # set working directory (Mac OS) 
setwd("C:/Users/dell/Dropbox/Teaching/NKU FINA 2078_2018/data")            # set working directory (Windows)
rm(list = ls())                                                            # clear memory

sp500m <- read_csv("sp500month.csv")                                       # read S&P500 monthly data 
sp500m <- select(sp500m, Date, `Adj Close`)                                # keep variables "date" and "adj close" 
sp500m <- rename(sp500m, index = `Adj Close`)                              # rename "adj close" -> "price"
sp500m <- arrange(sp500m, Date)                                            # sort data (in ascending order) according to "date"                             
sp500m <- filter(sp500m, Date >= as.Date("1990-01-01"))                    # keep only post-1990 data

sp500d <- read_csv("sp500day.csv")
sp500d <- select(sp500d, Date, `Adj Close`)
sp500d <- rename(sp500d, index = `Adj Close`)
sp500d$Date <- as.Date(strptime(as.character(sp500d$Date), "%m/%d/%Y"))    # change the date-time format
sp500d <- arrange(sp500d, Date)
sp500d <- filter(sp500d, Date >= as.Date("1990-01-01"))

vix <- read_csv("vix.csv")
vix <- select(vix, Date, `Adj Close`)
vix <- rename(vix, vix = `Adj Close`)
vix <- arrange(vix, Date)
vix <- filter(vix, Date >= as.Date("1990-01-01"))

dailyData <- inner_join(sp500d, vix, by = "Date")                          # merge sp500d and vix data by "date"
# note: see also left_join(), full_join(), semi_join(), anti_join()

dailyData <- mutate(dailyData,                                             # create new variables
                    lvix = lag(vix, n = 1L),                               # lagged vix
                    lindex = lag(index, n = 1L),                           # lagged S&P500 index
                    Dvix = vix - lvix,                                     # changes in vix                                 
                    logRindex = log(index/lindex),                         # log returns of S&P500 index
                    sqrRindex = logRindex^2,                               # squared returns of S&P500 index
                    absRindex = abs(logRindex))                            # absolute returns of S&P500 index
sp500m <- mutate(sp500m,
                 lindex = lag(index, n = 1L),
                 logRindex = log(index/lindex),
                 sqrRindex = logRindex^2,                               
                 absRindex = abs(logRindex))                            

## graphics
# stationarity, volatility clustering
fig11 <- ggplot(dailyData, aes(Date)) +
  geom_line(aes(y = index), size = 0.3, color = "dodgerblue3") +
  labs(y = "S&P 500 Daily Price", x ="",
       title = "Time Series of the Daily S&P 500 Indices, 1990-2015") +    # time series plot of S&P 500 Daily Price
  scale_x_date(breaks = date_breaks("3 years"), date_labels = "%Y", 
               limits = c(dailyData$Date[1], NA)) +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, family = "serif"))
print(fig11)

fig12 <- ggplot(dailyData, aes(Date)) +
  geom_line(aes(y = logRindex), size = 0.3, color = "dodgerblue3") +
  labs(y = "S&P 500 Daily Log Return", x ="",
       title = "Time Series of the Daily S&P 500 Returns, 1990-2015") +    # time series plot of S&P 500 Daily Price
  scale_x_date(breaks = date_breaks("3 years"), date_labels = "%Y", 
               limits = c(dailyData$Date[1], NA)) +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, family = "serif"))
print(fig12)

# heavy tails, asymmetry, aggregational Gaussiananity
fig21 <- ggplot(dailyData, aes(logRindex)) +
  geom_histogram(aes(y = ..density..), bins = 100, color="white", 
                 fill="dodgerblue3", 
                alpha = 0.75, size = 0.1) + 
  stat_function(fun = dnorm, color = "darkred", size = 0.7,
                args = list(mean = mean(dailyData$logRindex, 
                                        na.rm = T), 
                            sd = sd(dailyData$logRindex, 
                                    na.rm = T))) +
  labs(title = "Empirical Distribution of S&P 500 Daily Log Return", 
       x = "S&P 500 Daily Log Return", y = "Density") +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, family = "serif"))
print(fig21)
  
fig22 <- ggplot(sp500m, aes(logRindex)) +
  geom_histogram(aes(y = ..density..), bins = 80, color="white", 
                 fill="dodgerblue3", 
                 alpha = 0.75, size = 0.1) + 
  stat_function(fun = dnorm, color = "darkred", size = 0.7,
                args = list(mean = mean(sp500m$logRindex, 
                                        na.rm = T), 
                            sd = sd(sp500m$logRindex, 
                                    na.rm = T))) +
  labs(title = "Empirical Distribution of S&P 500 Monthly Log Return", 
       x = "S&P 500 Monthly Log Return", y = "Density") +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, family = "serif"))
print(fig22)

y <- quantile(dailyData$logRindex, c(0.25, 0.75), na.rm = T) 
x <- qnorm(c(0.25, 0.75))         
slope <- diff(y)/diff(x)             
int   <- y[1] - slope*x[1] 

fig23 <- ggplot(dailyData, aes(sample = logRindex)) +
  geom_qq(alpha = 0.5, size = 3, color = "dodgerblue3") +
  geom_abline(intercept=int, slope=slope, color = "darkred", size = 1) +
  labs(title = "Q-Q Plot of S&P 500 Daily Log Return", 
       x = "Normal Quantile", y = "Quantile of S&P 500 Daily Log Return") +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, family = "serif"))
print(fig23)

y <- quantile(sp500m$logRindex, c(0.25, 0.75), na.rm = T) 
x <- qnorm(c(0.25, 0.75))         
slope <- diff(y)/diff(x)             
int   <- y[1] - slope*x[1] 

fig24 <- ggplot(sp500m, aes(sample = logRindex)) +
  geom_qq(alpha = 0.5, size = 3, color = "dodgerblue3") +
  geom_abline(intercept=int, slope=slope, color = "darkred", size = 1) +
  labs(title = "Q-Q Plot of S&P 500 Monthly Log Return", 
       x = "Normal Quantile", y = "Quantile of S&P 500 Monthly Log Return") +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, family = "serif"))
print(fig24)

# long range dependence
dlog <- acf(dailyData$logRindex, plot = F, na.action = na.pass)
alpha <- 0.95
conf.lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(dlog$n.used)

dlog$acf %>% 
  as_tibble() %>% mutate(lags = 1:n()) %>% 
  ggplot(aes(x = lags, y = V1)) + scale_x_continuous(breaks = seq(0,41,4)) +
  geom_hline(yintercept = conf.lims, lty = 2, col = "blue") +
  labs(y = "ACF", x = "Lag", title = "Daily Log Returns") +
  geom_segment(aes(xend = lags, yend = 0)) + geom_point(size = 1) +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, family = "serif")) +
  ylim(-0.125,0.35)

dlog <- acf(dailyData$sqrRindex, plot = F, na.action = na.pass)
alpha <- 0.95
conf.lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(dlog$n.used)

dlog$acf %>% 
  as_tibble() %>% mutate(lags = 1:n()) %>% 
  ggplot(aes(x = lags, y = V1)) + scale_x_continuous(breaks = seq(0,41,4)) +
  geom_hline(yintercept = conf.lims, lty = 2, col = "blue") +
  labs(y = "ACF", x = "Lag", title = "Daily Squared Returns") +
  geom_segment(aes(xend = lags, yend = 0)) + geom_point(size = 1) +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, family = "serif")) + 
  ylim(-0.125,0.35)

dlog <- acf(dailyData$absRindex, plot = F, na.action = na.pass)
alpha <- 0.95
conf.lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(dlog$n.used)

dlog$acf %>% 
  as_tibble() %>% mutate(lags = 1:n()) %>% 
  ggplot(aes(x = lags, y = V1)) + scale_x_continuous(breaks = seq(0,41,4)) +
  geom_hline(yintercept = conf.lims, lty = 2, col = "blue") +
  labs(y = "ACF", x = "Lag", title = "Daily Absolute Returns") +
  geom_segment(aes(xend = lags, yend = 0)) + geom_point(size = 1) +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, family = "serif")) + 
  ylim(-0.125,0.35)
 
dlog <- acf(sp500m$logRindex, plot = F, na.action = na.pass)
alpha <- 0.95
conf.lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(dlog$n.used)

dlog$acf %>% 
  as_tibble() %>% mutate(lags = 1:n()) %>% 
  ggplot(aes(x = lags, y = V1)) + scale_x_continuous(breaks = seq(0,41,4)) +
  geom_hline(yintercept = conf.lims, lty = 2, col = "blue") +
  labs(y = "ACF", x = "Lag", title = "Monthly Log Returns") +
  geom_segment(aes(xend = lags, yend = 0)) + geom_point(size = 1) +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, family = "serif")) +
  ylim(-0.125,0.35)

dlog <- acf(sp500m$sqrRindex, plot = F, na.action = na.pass)
alpha <- 0.95
conf.lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(dlog$n.used)

dlog$acf %>% 
  as_tibble() %>% mutate(lags = 1:n()) %>% 
  ggplot(aes(x = lags, y = V1)) + scale_x_continuous(breaks = seq(0,41,4)) +
  geom_hline(yintercept = conf.lims, lty = 2, col = "blue") +
  labs(y = "ACF", x = "Lag", title = "Monthly Squared Returns") +
  geom_segment(aes(xend = lags, yend = 0)) + geom_point(size = 1) +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, family = "serif")) +
  ylim(-0.125,0.35)

dlog <- acf(sp500m$absRindex, plot = F, na.action = na.pass)
alpha <- 0.95
conf.lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(dlog$n.used)

dlog$acf %>% 
  as_tibble() %>% mutate(lags = 1:n()) %>% 
  ggplot(aes(x = lags, y = V1)) + scale_x_continuous(breaks = seq(0,41,4)) +
  geom_hline(yintercept = conf.lims, lty = 2, col = "blue") +
  labs(y = "ACF", x = "Lag", title = "Monthly Absolute Returns") +
  geom_segment(aes(xend = lags, yend = 0)) + geom_point(size = 1) +
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, family = "serif")) + 
  ylim(-0.125,0.35)

# leverage effect
fig41 <- ggplot(subset(dailyData, Date >= as.Date("2000-01-01")), 
                aes(Date)) + 
  geom_line(aes(y = vix, color = "VIX"), size = 0.3) +
  geom_line(aes(y = index/50, color = "S&P 500"), size = 0.3) +
  labs(title = "S&P 500 Index and VIX, 2000-2015", 
       x = "Dates", y = "VIX (left) and S&P 500 Index (right)") +        
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, family = "serif"),
        legend.position = c(0.15, 0.75),
        legend.title = element_blank(),
        legend.background = element_rect(fill=alpha("grey", 0), 
                                         color = "grey", size = 0.8, 
                                         linetype="solid"),
        legend.text = element_text(family = "serif", face = "bold"),
        legend.key = element_rect(color = "transparent")) +
  scale_y_continuous(sec.axis = sec_axis(~.*50))
print(fig41)

fig42 <- ggplot(dailyData, aes(logRindex*100, Dvix)) + 
  geom_point(alpha = .5, size = 2, color = "dodgerblue3") + 
  labs(title = "S&P 500 Returns vs Changes of VIX, 1990-2015", 
       x = "S&P 500 Returns", y = "Changes of VIX") + 
  geom_smooth(method = "lm", level = 0.9, color = "violetred3") +        
  theme(axis.title = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, family = "serif"))
print(fig42)

