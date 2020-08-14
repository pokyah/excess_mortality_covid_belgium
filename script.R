##---- load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(ggplot2)
library(plotly)
library(ggpubr)
library(htmlwidgets)


##---- data source
# from https://ourworldindata.org/excess-mortality-covid we are direct towards eurostat portal :
# https://appsso.eurostat.ec.europa.eu/nui/show.do?query=BOOKMARK_DS-1166948_QID_-65C2C7D1_UID_-3F171EB0&layout=TIME,C,X,0;GEO,L,Y,0;UNIT,L,Z,0;SEX,L,Z,1;AGE,L,Z,2;INDICATORS,C,Z,3;&zSelection=DS-1166948INDICATORS,OBS_FLAG;DS-1166948SEX,T;DS-1166948UNIT,NR;DS-1166948AGE,TOTAL;&rankName1=TIME_1_0_0_0&rankName2=UNIT_1_2_-1_2&rankName3=GEO_1_2_0_1&rankName4=AGE_1_2_-1_2&rankName5=INDICATORS_1_2_-1_2&rankName6=SEX_1_2_-1_2&sortC=ASC_-1_FIRST&rStp=&cStp=&rDCh=&cDCh=&rDM=true&cDM=true&footnes=false&empty=false&wai=false&time_mode=ROLLING&time_most_recent=false&lang=EN&cfo=%23%23%23%2C%23%23%23.%23%23%23
# data were downloaded as csv files present in this repo

data = readr::read_csv("demo_r_mweek3_1_Data.csv")

##---- data computation
data = data %>%
  dplyr::filter(!AGE == "Total") %>%
  dplyr::filter(!AGE == "Unknown") %>%
  dplyr::filter(GEO == "Belgium") %>%
  dplyr::mutate_at(.vars = c("AGE", "GEO"), .funs = as.factor) %>%
  dplyr::mutate_at(.vars = c("Value"), .funs = str_replace, ",", ".") %>%
  dplyr::mutate_at(.vars = c("Value"), .funs = as.numeric) %>%
  dplyr::mutate(year = str_extract(as.character(TIME), "^.{4}")) %>%
  dplyr::mutate(week = str_sub(as.character(TIME), start=-2)) %>%
  dplyr::mutate_at(.vars = c("week"), .funs = as.numeric)  %>%
  dplyr::mutate_at(.vars = c("week", "year"), .funs = as.numeric) %>%
  dplyr::mutate(date = stringr::str_remove(TIME, "W")) %>%
  dplyr::mutate_at(.vars = "date", .funs = paste0, 1) %>%
  dplyr::mutate_at(.vars = "date", .funs = as.Date, "%Y%W%u")

dates = data %>%
  dplyr::filter(year == 2020) %>%
  dplyr::select(date, week) %>%
  unique()

AGE =  unique(data$AGE)
age_num = as.factor(seq(5, 95, by = 5)) 
age = data.frame(AGE, age_num)

data = data %>%
  dplyr::left_join(age, by = "AGE")

data = data %>%
  dplyr::select(c("year", "week", "Value", "age_num")) %>%
  tidyr::pivot_wider(names_from = year, values_from = Value,
              names_prefix = "year") %>%
  dplyr::mutate(normal_expected = rowMeans(select(., c("year2015", "year2016", "year2017", "year2018", "year2019")), na.rm = TRUE)) %>%
  dplyr::mutate(excess_mortality =  year2020 - normal_expected) %>%
  dplyr::filter(!is.na(excess_mortality)) %>%
  dplyr::left_join(dates, by =("week")) 

##---- making the plot
# https://stackoverflow.com/questions/3777174/plotting-two-variables-as-lines-using-ggplot2-on-the-same-graph

plot =  ggplot2::ggplot(
    data = data,
    aes(x = date, y = excess_mortality)) +
  geom_line(aes(color = age_num), size = 0.5) +
  scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
  scale_y_continuous(breaks = seq(0, max(data$excess_mortality), by = 50)) +
  ggpubr::rotate_x_text() +
  geom_hline(yintercept = 0) +
  theme_minimal()

plot = plotly::ggplotly(plot)

htmlwidgets::saveWidget(plot, file = "./index.html", selfcontained = T)


