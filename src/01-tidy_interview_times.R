# get interview times

library(tidyverse)
library(stringr)
library(lubridate)

raw_interviews <- read_csv("data/raw/interview_times.csv") %>%
    rename(first_name = `First Name`,
           last_name = `Last Name`,
           email = Email) %>%
    extract(`Sign Up Items`,
            c("interview_date", "interview_time"),
            "(02/[0-9]{2}/2017) (8|11)") %>%
    dmap_at("interview_date", mdy)

