## ----setup, include=FALSE, eval=TRUE--------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)


## ----read in data, eval=TRUE, cache=TRUE, warning=FALSE-------------
download.file("https://covidtracking.com/data/download/national-history.csv", destfile = "national-history.csv")
cv <- fread("national-history.csv")

# Getting CA covid history data
download.file("https://covidtracking.com/data/download/california-history.csv", destfile = "california-history.csv")
dat <- fread("california-history.csv")

# Getting covid data per county in CA
download.file("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", destfile = "us-counties.csv", method="libcurl", timeout = 60)
counties <- fread("us-counties.csv")


## -------------------------------------------------------------------
# selecting columns we need
cv <- cv[, .(date, states, death, deathIncrease, hospitalizedCurrently, positive, positiveIncrease, recovered, totalTestResults)] %>%
  rename(hospitalized = hospitalizedCurrently, newcases = positiveIncrease, cases = positive, tests = totalTestResults)
cv$date <- as.Date(cv$date)

#
cv <- cv %>%
  mutate(CFR = round((death*100/(death+recovered)), 2)) %>%
  mutate(naive_CFR= round((death*100/cases), 2))
#
dat <- dat[, .(date, state, death, deathIncrease, hospitalizedCurrently, positive, positiveIncrease, totalTestResults)] %>%
  rename(hospitalized = hospitalizedCurrently, newcases = positiveIncrease, cases = positive, tests = totalTestResults)

dat$date <- as.Date(dat$date)

# Creating Quarter variable
dat[date>="2020-04-01" & date<="2020-06-30", Quarter := "Q1"]
dat[date>="2020-07-01" & date<="2020-09-30", Quarter := "Q2"]

covid_ca <- dat[date>="2020-04-01" & date<="2020-09-30"][order(date, decreasing = FALSE)]

# Creating new cases variable and filtering CA data
counties$date <- as.Date(counties$date)
counties <- counties %>%
  mutate(newcases = cases - lag(cases, 1)) %>%
  mutate(naive_CFR= round((deaths*100/cases), 2))
  
# Validating naive CFR
counties[newcases < 0, newcases := 0]
counties[naive_CFR == Inf, naive_CFR := 0]
counties[deaths > cases, naive_CFR := 0]

# Filtering CA data
ca_county <- counties %>%
  filter(state == "California")


## ----overview, message=FALSE----------------------------------------
ggplot(dat)+
  geom_line(mapping = aes(x = date, y = cases), col="red", na.rm = TRUE)+
  geom_line(mapping = aes(x = date, y = death), col = "darkred", na.rm = TRUE)+
  ggtitle("Total cases and deaths in California")+
  theme_bw()

ggplot(dat)+
  geom_line(mapping = aes(x = date, y = death), col = "darkred", na.rm = TRUE)+
  ggtitle("Total deaths in California")+
  theme_bw()

ggplot(dat)+
  geom_point(mapping = aes(x = date, y = hospitalized), na.rm = TRUE, col="skyblue", alpha = 0.5)+
  ggtitle("Hospitalized cases in California")+
  theme_classic()

ggplot(dat, mapping = aes(x = date))+
  geom_bar(aes(y = newcases), position = "stack", stat = "identity", col="#69b3a2", show.legend = TRUE, na.rm = TRUE)+
  stat_smooth(aes(date, newcases), method = "gam", color = "red", size = 0.5, se = FALSE)+
  ggtitle("New cases in California")+
  theme_classic()


## ----trend by quarters, message=FALSE-------------------------------
# New cases by quarters
ggplot(covid_ca)+
  geom_boxplot(mapping = aes(group = Quarter, y = newcases, color = Quarter, fill = Quarter, alpha = 0.5))+
  ggtitle("New cases in California by quarters")+
  theme_classic()

# Take a look at different month
covid_ca1 = mutate(dat, month=substr(dat$date, 6, 7))
covid_ca2 = mutate(covid_ca, month=substr(covid_ca$date, 6, 7))

ggplot(covid_ca2)+
  geom_point(mapping = aes(x = date, y = newcases, color = month))+
  stat_smooth(aes(x = date, y = newcases, color = month), method = lm, se = FALSE)+
  ggtitle("New cases in California by month")+
  theme_classic()

# Statistical summary graph
ggplot(covid_ca1, mapping = aes(x = month, y = newcases))+
  stat_summary(fun.data = "mean_sdl")+
  ggtitle("Statistical summary graphs of new cases by month")+
  theme_bw()


## ----top ten counties-----------------------------------------------
# Getting the counties with top ten cases and deaths
top_cases <- ca_county %>%
  filter(date == max(date)) %>%
  select(county, cases) %>%
  arrange(desc(cases))
top_10_cases <- top_cases[1:10,]

top_deaths <- ca_county %>%
  filter(date == max(date)) %>%
  select(county, deaths) %>%
  arrange(desc(deaths))
top_10_deaths <- top_deaths[1:10,]

# plot
ggplot(top_10_cases, aes(x = reorder(county, -cases)))+
  geom_bar(aes(y = cases), position = "stack", stat = "identity", fill = "skyblue")+
  labs(title = "Counties in CA with top ten cases", x = "County", y = "Cases")+
  theme_bw()

ggplot(top_10_deaths, aes(x = reorder(county, -deaths)))+
  geom_bar(aes(y = deaths), position = "stack", stat = "identity", fill = "salmon")+
  labs(title = "Counties in CA with top ten deaths", x = "County", y = "Deaths")+
  theme_bw()




## ----usmap----------------------------------------------------------
# Plotting a state map
library(usmap)
map <- ca_county %>%
  filter(date == max(date)) %>%
  select(fips, cases, deaths)

p2 <- plot_usmap("counties", data = map, values = "cases", include = "CA")+
  scale_fill_continuous(low = "white", high = "coral")+
  labs(title = "Covid cases across California today", fill = "cases")

p3 <- plot_usmap("counties", data = map, values = "deaths", include = "CA")+
  scale_fill_continuous(low = "white", high = "darkred")+
  labs(title = "Covid deaths across California today", fill = "deaths")



## ----Los Angeles County, warning=FALSE, message=FALSE---------------
lac <- counties %>%
  filter(county == "Los Angeles") %>%
  arrange(desc(cases))

ggplot(lac)+
  geom_line(mapping = aes(x = date, y = newcases, color = "newcases"), position = "stack", size = 0.7, na.rm = TRUE)+
  stat_smooth(aes(date, newcases), method = "gam", color = "red", size = 0.5, se = FALSE)+
  geom_line(mapping = aes(x = date, y = deaths, color = "deaths"), size = 0.7, na.rm = TRUE)+
  stat_smooth(aes(x = date, y = deaths), size = 0.5)+
  ggtitle("New cases and deaths in LA County")+
  theme_bw()


## ----tables---------------------------------------------------------
# Getting the day with the most number of new cases and new deaths
newcases <- dat %>%
  filter(newcases == max(newcases)) %>%
  select(date, newcases) 

deaths <- dat %>%
  filter(deathIncrease == max(deathIncrease)) %>%
  select(date, deathIncrease) 

knitr::kable(
  list(newcases, deaths),
  caption = "Day with the most number of new cases and deaths")

# Getting counties with top ten cases and deaths
knitr::kable(
  list(top_10_cases, top_10_deaths),
  caption = 'Counties with top ten cases and deaths.',
  booktabs = TRUE, valign = 't'
)

# Latest summary
dat[!is.na(cases), positiveprop := cases/tests]
latest <- dat[1, .(date, state, death, deathIncrease, hospitalized, cases, newcases, tests, positiveprop)]

knitr::kable(x = latest, caption = "Latest Covid Summary in CA")

lat_lac <- lac[1, .(date, county, cases, deaths, newcases)]
knitr::kable(x = lat_lac, caption = "Latest Covid Summary in LAC")

