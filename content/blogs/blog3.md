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

```{r}
yield_curve <- read_csv(here::here("data", "yield_curve.csv"))

glimpse(yield_curve)

```

```{r}
yield_curve %>%
  ggplot(aes(x = date, y = value, colour = duration)) + 
  facet_wrap (~duration, nrow= 6) + geom_line() +
  labs(title="Yields on U.S. Treasury rates since 1960",
       x="",
       caption="Source: St. Louis Federal Reserve Economic Database (FRED)")+
  theme(legend.position = "none")

```
