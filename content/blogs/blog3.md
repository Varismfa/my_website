---
categories:
- ""
- ""
date: "2017-10-31T22:26:09-05:00"
description: Lorem Etiam Nullam
draft: false
image: Yieldcurve.png
keywords: ""
slug: magna
title: Macroeconomic Analysis
---

This is one of the assignment that I was assigned in the Data for Analytic couse at LBS and it is very interesting the show the R coding process and the analysis of the chart. 

**Code in R Studio for the above pictures:** 

**Step 1**

I start by downloading the yield curve data in excel and import to R Studio

```{r}
yield_curve <- read_csv(here::here("data", "yield_curve.csv"))

glimpse(yield_curve)

```

**Step 2**

The data frame does not include the yield spread between 10 years and 3 months ;therefore, I manipulate the dataframe to create the spread (yield curve between 3 month and 10 years). Moreover, I create the recession dates that happen in the US to use for shading the graph in the next step. Moreover , I create the column up and down which will be used in the graph shading condition in the next step. 

```{r setup_US-recessions, warning=FALSE}

# get US recession dates after 1946 from Wikipedia 
# https://en.wikipedia.org/wiki/List_of_recessions_in_the_United_States

recessions <- tibble(
  from = c("1948-11-01", "1953-07-01", "1957-08-01", "1960-04-01", "1969-12-01", "1973-11-01", "1980-01-01","1981-07-01", "1990-07-01", "2001-03-01", "2007-12-01","2020-02-01"),  
  to = c("1949-10-01", "1954-05-01", "1958-04-01", "1961-02-01", "1970-11-01", "1975-03-01", "1980-07-01", "1982-11-01", "1991-03-01", "2001-11-01", "2009-06-01", "2020-04-30") 
  )  %>% 
  mutate(From = ymd(from), 
         To=ymd(to),
         duration_days = To-From)


recessions
# Manipulate the data to find the difference of yield between 3m and 10 y maturity 
yield_curve_diff <- yield_curve%>%
  filter(maturity %in% c("3m","10y"))%>%
  pivot_wider(id_cols = c(date), names_from = maturity, values_from = value)%>%
  rename(y_10 = `10y`, m_3 = `3m`) %>% 
  mutate(diff = y_10 - m_3)%>%
  mutate(
  up = ifelse(y_10>m_3, diff, 0),down = ifelse(y_10<m_3, diff, 0)) %>% 
  mutate(true_false=ifelse(y_10>m_3, "Yes", "No")) 

```

**Step 3**

Plot the graph using ggplot function. Please note that the when the graph shade in blue mean 10 year - 3 months US treasury rate is positive and shade in red mean  10 year - 3 months US treasury rate is positive. The grey shade in the graph represent the recession period in the US economy. 

```{r}
ggplot(yield_curve_diff) + 
  geom_rect(aes(x=date,xmin=as.Date("1960-04-01","%Y-%m-%d"),xmax=as.Date("1961-02-01","%Y-%m-%d"),ymin = -5,ymax = 5, group=date), fill = "grey69", alpha = 0.01)+
  geom_rect(aes(x=date,xmin=as.Date("1969-12-01","%Y-%m-%d"),xmax=as.Date("1970-11-01","%Y-%m-%d"),ymin = -5,ymax = 5, group=date), fill = "grey69", alpha = 0.01)+
  geom_rect(aes(x=date,xmin=as.Date("1973-11-01","%Y-%m-%d"),xmax=as.Date("1975-03-01","%Y-%m-%d"),ymin = -5,ymax = 5, group=date), fill = "grey69", alpha = 0.01)+
  geom_rect(aes(x=date,xmin=as.Date("1980-01-01","%Y-%m-%d"),xmax=as.Date("1980-07-01","%Y-%m-%d"),ymin = -5,ymax = 5, group=date), fill = "grey69", alpha = 0.01)+
  geom_rect(aes(x=date,xmin=as.Date("1981-07-01","%Y-%m-%d"),xmax=as.Date("1982-11-01","%Y-%m-%d"),ymin = -5,ymax = 5, group=date), fill = "grey69", alpha = 0.01)+
  geom_rect(aes(x=date,xmin=as.Date("1990-07-01","%Y-%m-%d"),xmax=as.Date("1991-03-01","%Y-%m-%d"),ymin = -5,ymax = 5, group=date), fill = "grey69", alpha = 0.01)+
  geom_rect(aes(x=date,xmin=as.Date("2001-03-01","%Y-%m-%d"),xmax=as.Date("2001-11-01","%Y-%m-%d"),ymin = -5,ymax = 5, group=date), fill = "grey69", alpha = 0.01)+
  geom_rect(aes(x=date,xmin=as.Date("2007-12-01","%Y-%m-%d"),xmax=as.Date("2009-06-01","%Y-%m-%d"),ymin = -5,ymax = 5, group=date), fill = "grey69", alpha = 0.01)+
  geom_rect(aes(x=date,xmin=as.Date("2020-02-01","%Y-%m-%d"),xmax=as.Date("2020-04-30","%Y-%m-%d"),ymin = -5,ymax = 5, group=date), fill = "grey69", alpha = 0.01)+
  geom_line(aes(x=date,y=diff))+
  geom_point(aes(x=date,y=diff), color = "transparent")+
  geom_rug(aes(x=date,color = true_false))+
  geom_ribbon(aes(x=date,ymin=down,ymax=0),fill="#CB454A",alpha=0.4)+
  geom_ribbon(aes(x=date,ymin=0,ymax=up),fill="steelblue3",alpha=0.4)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title= "Yield Curve Inversion: 10-year minus 3-month U.S. Treasury rates",
       subtitle="Difference in % points, monthly averages.
Shaded areas cirrespond to recessions",
       x="",
       y="Difference (10 year-3 month) yield in %",
       caption="Source.FRED.Federal Reserve Bank of St.Louis")
  
```


** Graph Analysis**

Almost everytime before the recession happen, we will see the inverted bond yield ( spread between 10 year and 3 months is negative or the red shade area time frame in the graph).Theoretically speaking, we know that a yield with a longer maturity should always be higher than a yield with a smaller maturity, as there is a risk premium over longer maturities - it should always be safer to buy right now than to buy in an uncertain futures. However, for short period of history, the yield curve inverted with means the 3-month yield was higher than the 10-year yield. When investors think it is riskier to buy today rather than in an uncertain future, it means the situation today is worse than it will be in the years to come. It is usually a signal of an impending crisis. And as predicted, each inversion yield curve was followed by a financial crisis. In 2000 and 2006, the yield curve inverted. If you look at the central banks speeches following those inverted yield curves, they always try to reassure the markets... In 2019, there was another inversion of the yield curve. The market could not have predicted the covid crisis, but there were issues at the time that could also have led to a crisis. The very low rate environment which ended up on equities sky valuations could have unveiled a financial bubble if rates had increased. Today, after 2 years of pandemic, we are faced with similar issues. The potential rise in yields poses a serious threat to the current excessive valuation of tech stocks, and could unveil a financial bubble. To quote Warren Buffet, “only when the tide goes out do you discover who's been swimming naked”. This will surely be one of the critical issues of the coming months. A yield curve flattening can really mean a recession is coming in the US. A research study from Deutsche Bank actually calculated the regression between inverted yield curves and recessions and found a very small p-value, which proved the statistical relationship of the variables. As a conclusion, we see that inverted yield curves are a good indicator of potential recessions. As we mentioned earlier, theoretically speaking, it does not make any sense to think it is more riskier to buy today than in a future we don't know. We see that an inversion was systematically followed by a crisis even when it happened in 2019, when it was impossible to predict what will happen. We need to stay tune and always follow the spread.



