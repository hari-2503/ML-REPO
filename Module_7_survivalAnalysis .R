# survival analysis in R
#https://www.emilyzabor.com/survival-analysis-in-r.html

#***********************************************************************************
#. How Survival Analysis is done
#*************************************************************************************
#*There are two methods that can be used to perform survival analysis in R programming language: 
#     Kaplan-Meier method
#.    Cox Proportional hazard model

# Installing package
install.packages("survival")

# Loading package
library(survival)

#***********************************************************************************
#. "Lung" dataset
#*************************************************************************************
# Dataset information
?lung

lung <- 
  lung |> 
  mutate(
    status = recode(status, `1` = 0, `2` = 1)
  )

head(lung[, c("time", "status", "sex")])

#***********************************************************************************
#. "Calculating Survival Times"
#*************************************************************************************
date_ex <- 
  tibble(
    sx_date = c("2007-06-22", "2004-02-13", "2010-10-27"), 
    last_fup_date = c("2017-04-15", "2018-07-04", "2016-10-31")
  )

date_ex
# the output reveals. that these are both character variables, but we need them to be formatted as dates.
#.   We will use the {lubridate} package to work with dates. 
#.  In this case, we need to use the ymd() function to change the format, 
#.   since the dates are currently in the character format where the year comes first, followed by the month, and followed by the day.

install.packages("lubridate")
library(lubridate)

date_ex <-
  date_ex |> 
  mutate(
    sx_date = ymd(sx_date), 
    last_fup_date = ymd(last_fup_date)
  )
date_ex. #. we can see the dates are formatted.

# Now, e need to calculate the difference between start and end dates in some units, usually months or years. 
##. Using the {lubridate} package, the operator %--% designates a time interval, 
##.  which is then converted to the number of elapsed seconds using as.duration() and finally converted to years by dividing by dyears(1)
date_ex <-
  date_ex |> 
  mutate(
    os_yrs = as.duration(sx_date %--% last_fup_date) / dyears(1)
  )
date_ex

#***********************************************************************************
#. "Creating survival objects and curves"
#*************************************************************************************
# The Kaplan-Meier method is the most common way to estimate survival times and probabilities
#.  The Surv() function from the {survival} package creates a survival object for use as the response in a model formula.
Surv(lung$time, lung$status)[1:10]
#. the output We see that subject 1 had an event at time 306 days, subject 2 had an event at time 455 days, 
##.   subject 3 was censored at time 1010 days, etc

## The survfit() function creates survival curves using the Kaplan-Meier method based on a formula
s1 <- survfit(Surv(time, status) ~ 1, data = lung)
str(s1)

#***********************************************************************************
#.    "Kaplan-Meier plots"
#*************************************************************************************
install.packages("ggsurvfit")
library(ggsurvfit)
survfit2(Surv(time, status) ~ 1, data = lung) |> 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )

survfit2(Surv(time, status) ~ 1, data = lung) |> 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval(fill = "lightblue", alpha = 0.4) +
  add_risktable()

#***********************************************************************************
#.    "Estimating -year survival"
#*************************************************************************************

## One quantity often of interest in a survival analysis is the probability of surviving beyond a certain number of years, .
## For example, to estimate the probability of surviving to  year, use summary with the times argument 
##. (Note: the time variable in the lung data is actually in days, so we need to use times = 365.25)

summary(survfit(Surv(time, status) ~ 1, data = lung), times = 365.25)

install.packages("gtsummary")
library(gtsummary)
survfit(Surv(time, status) ~ 1, data = lung) |> 
  tbl_survfit(
    times = 365.25,
    label_header = "**1-year survival (95% CI)**"
  )







