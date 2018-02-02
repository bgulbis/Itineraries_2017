# get interview times

library(tidyverse)
library(lubridate)

interview_times <- read_csv("data/raw/interview_times.csv") %>%
    rename(first_name = `First Name`,
           last_name = `Last Name`,
           email = Email,
           lcep = LCEP,
           interview_date = `Interview Date`) %>%
    mutate_at("interview_date", mdy_hm) %>%
    mutate(pm = hour(interview_date) == "11",
           day = as.Date(interview_date)) %>%
    group_by(interview_date, pm) %>%
    arrange(last_name, .by_group = TRUE) %>%
    mutate(assignment = seq(n()),
           credentials = "PharmD Candidate")

write_rds(interview_times, "data/tidy/interview_times.Rds", "gz")
write_csv(interview_times, "data/external/interview_schedule.csv")
