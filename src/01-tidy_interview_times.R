# get interview times

library(tidyverse)
library(stringr)
library(lubridate)

interview_times <- read_csv("data/raw/interview_times.csv") %>%
    rename(first_name = `First Name`,
           last_name = `Last Name`,
           email = Email) %>%
    extract(`Sign Up Items`,
            c("interview_date", "pm"),
            "(02/[0-9]{2}/2017) (8|11)") %>%
    dmap_at("interview_date", mdy) %>%
    dmap_at("pm", ~ .x == "11") %>%
    group_by(interview_date, pm) %>%
    mutate(assignment = seq(n()))

write_rds(interview_times, "data/tidy/interview_times.Rds", "gz")
