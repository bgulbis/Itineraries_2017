# create schedule for each interview group

library(tidyverse)
library(lubridate)

interview_times <- read_rds("data/tidy/interview_times.Rds") %>%
    arrange(interview_date, pm, assignment)

interview_sessions <- read_csv("data/raw/interview_sessions.csv") %>%
    filter(session_id >= 3, session_id <= 6)

sched <- tibble(
    session_id = 0,
    session = "",
    start_time = mdy_hms("1/1/2017 08:00"),
    interview_date = mdy("1/1/2017"),
    pm = FALSE,
    last_name = "",
    first_name = ""
)

for (i in 1:nrow(interview_times)) {
# for (i in 1:2) {
    df <- interview_times[i, ]

    if (df$pm) {
        start_time <- hm("3:30")
    } else {
        start_time <- hm("1:30")
    }

    sessions <- mutate(interview_sessions, interview_order = session_id)
    sid <- 3:6
    sessions$interview_order <- sid[c(df$assignment:length(sid), 1:(df$assignment - 1))][1:4]

    sessions <- arrange(sessions, interview_order)

    times <- sessions %>%
        mutate(cum_duration = cumsum(duration),
               tmp_start = as.POSIXct(df$interview_date + start_time),
               stop_time = tmp_start + minutes(cum_duration),
               start_time = stop_time - minutes(duration),
               itinerary_time = paste(format(start_time, "%I:%M %p"),
                                      format(stop_time, "%I:%M %p"),
                                      sep = " - "))

    group_sched <- times %>%
        select(session_id, session, start_time) %>%
        mutate(interview_date = df$day[1],
               pm = df$pm[1],
               last_name = df$last_name[1],
               first_name = df$first_name[1])

    sched <- bind_rows(sched, group_sched)
}

all_scheds <- sched %>%
    filter(session_id != 0) %>%
    arrange(session_id, start_time) %>%
    mutate(session_time = format(start_time, "%I:%M %p")) %>%
    select(session, interview_date, session_time, first_name, last_name)

x <- split(all_scheds, all_scheds$session)
walk(x, ~ write_csv(.x, path = paste0("data/external/", .x$session[1], ".csv")))
