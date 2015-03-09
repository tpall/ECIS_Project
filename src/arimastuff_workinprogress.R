library(forecast)
library(magrittr)
library(plyr)
library(dplyr)
library(lubridate)
imp$Date %>% unique %>% sample(.,3)

ar <- imp %>% filter(Date %in% c("20140113", "20131218", "20140311")) %>% 
  dlply(.,.(Date,Well,Param,Freq)) %>% "["(sample(1:5040,5))
ar[[2]] %>% summary
fit <- ar[[5]] %>% {time <- use_series(.,Time) %>% (function(x) now() + dhours(x))
            value <- use_series(.,value)
            zoo(value,time)} %>% arima(order=c(1,0,0)) %>% summary
tsdiag(fit)
Box.test(fit$residuals,lag=1)
acf(fit$residuals)
ar[[2]] %>% use_series(.,Time) %>% (function(x) now() + dhours(x))
c <- ar[[2]] %>% {time <- use_series(.,Time) %>% (function(x) now() + dhours(x))
             value <- use_series(.,value)
             zoo(value,time)}

lm(c~time(c)) %>% summary
lm(c~time(c)) %>% summary
head(c)

