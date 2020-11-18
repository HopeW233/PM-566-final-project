## ----setup, include=FALSE, eval=TRUE----------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(DT)


## ----read in data, eval=TRUE, cache=TRUE, warning=FALSE---------------------------
download.file("https://covidtracking.com/data/download/national-history.csv", destfile = "national-history.csv")
cv_national <- fread("national-history.csv")

# Getting CA covid history data
download.file("https://covidtracking.com/data/download/california-history.csv", destfile = "california-history.csv")
cv_ca <- fread("california-history.csv")

# Getting covid data per county in CA
download.file("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", destfile = "us-counties.csv", method="libcurl", timeout = 60)
counties <- fread("us-counties.csv")

download.file("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv", destfile = "us-states.csv", method="libcurl", timeout = 60)
states <- fread("us-states.csv")

#
download.file("https://raw.githubusercontent.com/COVID19Tracking/associated-data/master/us_census_data/us_census_2018_population_estimates_states.csv", destfile = "us_census_2018_population_estimates_states.csv")
pop <- fread("us_census_2018_population_estimates_states.csv")

#
download.file("https://covid19-lockdown-tracker.netlify.com/lockdown_dates.csv", destfile = "lockdown_dates.csv")
lockdown <- fread("lockdown_dates.csv")

#
cv_states <- merge(
  x = states,
  y = pop,
  by.x = "state",
  by.y = "state_name",
  all.x = TRUE, all.y = FALSE
)

lockdown <- lockdown %>%
  filter(Country == "United States")

cv_states <- merge(
  x = cv_states,
  y = lockdown,
  by.x = "state",
  by.y = "Place",
  all.x = TRUE, all.y = FALSE
)

dat <- cv_states %>%
  select(state, date, cases, deaths, population, pop_density, `Start date`, `End date`)


## ---------------------------------------------------------------------------------
# selecting columns we need
cv_national <- cv_national[, .(date, states, death, deathIncrease, positive, recovered, totalTestResults)] %>%
  rename(newdeaths = deathIncrease, tests = totalTestResults)
cv_national$date <- as.Date(cv_national$date)

#
cv_national <- cv_national %>%
  mutate(CFR = round((death*100/(death+recovered)), 2)) %>%
  mutate(IFR = round((death*100/positive), 2))

cv_national[CFR == Inf, CFR := 0]
cv_national[IFR == Inf, IFR := 0]
cv_national[death > positive, CFR := 0]
# format
dat$date <- as.Date(dat$date)
dat$`Start date` <- as.Date(dat$`Start date`)
dat$`End date` <- as.Date(dat$`End date`)

state_list <- unique(dat$state)
dat$state <- factor(dat$state, levels = state_list)

summary(dat)
# Creating new variables
dat <- dat %>%
  mutate(newcases = cases - lag(cases, 1)) %>%
  mutate(newdeaths = deaths - lag(deaths, 1)) %>%
  mutate(death_rate = round((deaths*100/(cases)), 2)) %>%
  mutate(month = substr(dat$date, 6, 7))
  

dat[newcases < 0, newcases := 0]
dat[newdeaths <0, newdeaths := 0]
dat[death_rate == Inf, death_rate := 0]
dat[deaths > cases, death_rate := 0]

dat$per100k =  as.numeric(format(round(dat$cases/(dat$population/100000),1),nsmall=1))
dat$newper100k =  as.numeric(format(round(dat$newcases/(dat$population/100000),1),nsmall=1))
dat$deathsper100k =  as.numeric(format(round(dat$deaths/(dat$population/100000),1),nsmall=1))
dat$newdeathsper100k =  as.numeric(format(round(dat$newdeaths/(dat$population/100000),1),nsmall=1))

# Creating Quarter variable
counties$date <- as.Date(counties$date)

ca_county <- counties %>%
  filter(state == "California") %>%
  mutate(newcases = cases - lag(cases, 1)) %>%
  mutate(newdeaths = deaths - lag(deaths, 1)) %>%
  mutate(death_rate = round((deaths*100/(cases)), 2))
  

ca_county[newcases < 0, newcases := 0]
ca_county[newdeaths <0, newdeaths := 0]
ca_county[death_rate == Inf, death_rate := 0]
ca_county[deaths > cases, death_rate := 0]


## ---------------------------------------------------------------------------------
table <- dat %>%
  select(state, date, cases, deaths, newcases, newdeaths, death_rate, per100k, deathsper100k) %>%
 arrange(state, desc(date))

datatable(table)


## ----overview---------------------------------------------------------------------
fig1 <- cv_national %>%
  ggplot()+
  geom_line(aes(date, CFR), color = "skyblue", size = 0.8, na.rm = TRUE)+
  geom_line(aes(date, IFR), color = "#69b3a2", size = 0.8, na.rm = TRUE)+
  geom_vline(aes(xintercept=as.numeric(ymd("2020-03-25"))), linetype = "dashed", color = "red", size = 0.5)+
  geom_vline(aes(xintercept=as.numeric(ymd("2020-02-26"))), linetype = "dashed", color = "darkred", size = 0.5)+
  labs(title = "Overview of COVID 19 CFR and IFR in US", y = "CFR/IFR")+
  theme_bw()

ggplotly(fig1)


## ---------------------------------------------------------------------------------
# death rate 
dat_today <- dat %>%
  filter(date == max(date)) %>%
  mutate(Mortality = deaths / cases) %>% 
  mutate(Std = sqrt((Mortality * (1 - Mortality))/sqrt(cases)))

mean_mortality <- sum(dat_today$deaths) / sum(dat_today$cases)

fig2 <- dat_today %>%
  ggplot(aes(reorder(state, Mortality), color = state, `Death Rate` = round(Mortality*100,2)))+
  geom_point(aes(y = Mortality))+
  geom_hline(yintercept = mean_mortality, linetype = "dashed", color = "red")+
  geom_errorbar(aes(ymin = Mortality - 2 * Std, ymax = Mortality + 2 * Std))+
  coord_flip()+
  theme(text = element_text(size = 18), legend.position = "none")+
  labs(x = "State", y = "Death Rate", subtitle = "Hypothesis Test of Covid Death Rate")+
  theme_bw()

ggplotly(fig2, tooltip = c("state", "Death Rate"))

# pop_density vs. death rate
fig3 <- dat_today %>%
  filter(state!="District of Columbia") %>%
  plot_ly(x = ~pop_density, y = ~death_rate,
          type = "scatter", mode = "markers", color = ~state, 
          size = ~population, sizes = c(5, 70), marker = list(sizemode='diameter', opacity=0.5),
          hoverinfo = "text",
          text = ~paste(paste(state, ":", sep=""), paste("Cases per 100k: ", per100k, sep="") , paste("Deaths per 100k: ", deathsper100k, sep=""), paste("Death Rate: ", death_rate, sep=""), sep = "<br>"))
fig3


## ---------------------------------------------------------------------------------
fig4 <- dat %>%
  filter(state == "California") %>%
  ggplot()+
  geom_point(aes(date, newcases, color = month))+
  stat_smooth(aes(date, newcases, color = month), method = lm, se = FALSE)+
  geom_vline(aes(xintercept=`Start date`), linetype = "dashed", color = "red", size = 0.5)+
  geom_vline(aes(xintercept=`End date`), linetype = "dashed", color = "darkred", size = 0.5)+
  ggtitle("New cases in California by month")+
  theme_classic()

ggplotly(fig4)



## ----trend by quarters, message=FALSE, eval=FALSE---------------------------------
## # New cases by quarters
## ggplot(covid_ca)+
##   geom_boxplot(mapping = aes(group = Quarter, y = newcases, color = Quarter, fill = Quarter, alpha = 0.5))+
##   ggtitle("New cases in California by quarters")+
##   theme_classic()
## 
## # Take a look at different month
## covid_ca1 = mutate(dat, month=substr(dat$date, 6, 7))
## covid_ca2 = mutate(covid_ca, month=substr(covid_ca$date, 6, 7))
## 
## ggplot(covid_ca2)+
##   geom_point(mapping = aes(x = date, y = newcases, color = month))+
##   stat_smooth(aes(x = date, y = newcases, color = month), method = lm, se = FALSE)+
##   ggtitle("New cases in California by month")+
##   theme_classic()
## 
## # Statistical summary graph
## ggplot(covid_ca1, mapping = aes(x = month, y = newcases))+
##   stat_summary(fun.data = "mean_sdl")+
##   ggtitle("Statistical summary graphs of new cases by month")+
##   theme_bw()


## ----top ten counties-------------------------------------------------------------
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




## ----usmap------------------------------------------------------------------------
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



## ----Los Angeles County, warning=FALSE, message=FALSE, eval=FALSE-----------------
## lac <- counties %>%
##   filter(county == "Los Angeles") %>%
##   arrange(desc(cases))
## 
## ggplot(lac)+
##   geom_line(mapping = aes(x = date, y = newcases, color = "newcases"), position = "stack", size = 0.7, na.rm = TRUE)+
##   stat_smooth(aes(date, newcases), method = "gam", color = "red", size = 0.5, se = FALSE)+
##   geom_line(mapping = aes(x = date, y = deaths, color = "deaths"), size = 0.7, na.rm = TRUE)+
##   stat_smooth(aes(x = date, y = deaths), size = 0.5)+
##   ggtitle("New cases and deaths in LA County")+
##   theme_bw()


## ----tables, eval=FALSE-----------------------------------------------------------
## # Getting the day with the most number of new cases and new deaths
## newcases <- dat %>%
##   filter(newcases == max(newcases)) %>%
##   select(date, newcases)
## 
## deaths <- dat %>%
##   filter(newdeaths == max(newdeaths)) %>%
##   select(date, newdeaths)
## 
## knitr::kable(
##   list(newcases, deaths),
##   caption = "Day with the most number of new cases and deaths")
## 
## # Getting counties with top ten cases and deaths
## knitr::kable(
##   list(top_10_cases, top_10_deaths),
##   caption = 'Counties with top ten cases and deaths.',
##   booktabs = TRUE, valign = 't'
## )
## 
## # Latest summary
## dat[!is.na(cases), positiveprop := cases/tests]
## latest <- dat[1, .(date, state, death, deathIncrease, hospitalized, cases, newcases, tests, positiveprop)]
## 
## knitr::kable(x = latest, caption = "Latest Covid Summary in CA")
## 
## lat_lac <- lac[1, .(date, county, cases, deaths, newcases)]
## knitr::kable(x = lat_lac, caption = "Latest Covid Summary in LAC")

