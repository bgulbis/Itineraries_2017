# create itineraries

library(tidyverse)
library(stringr)
library(lubridate)
library(ReporteRs)



interview_times <- read_rds("data/tidy/interview_times.Rds")
interview_sessions <- read_csv("data/raw/interview_sessions.csv")
interviewers <- read_csv("data/raw/interviewers.csv")

interview_days <- list("day1" = "2017/02/13",
                       "day2" = "2017/02/17",
                       "day3" = "2017/02/20")

interviewer_assignments <- read_csv("data/raw/interviewer_assignments.csv") %>%
    gather(key, value, day1:day3) %>%
    dmap_at("key", str_replace_all, pattern = interview_days) %>%
    dmap_at("key", ymd)

for (i in 1:nrow(interview_times)) {
# for(i in c(1:2, 7:9)) { # for testing
    df <- interview_times[i, ]

    # need to replace credentials with a variable
    candidate <- paste(df$first_name, df$last_name)
    interview_date <- format(df$interview_date, "%A, %B %e, %Y")

    if (!df$pm) {
        start_time <- hm("08:00")
        sid <- 3:7
        sessions <- mutate(interview_sessions, interview_order = session_id)
    } else {
        if (df$lcep) {
            start_time <- hm("11:30")
        } else {
            start_time <- hm("11:15")
        }

        sessions <- interview_sessions %>%
            filter(!is.na(pm)) %>%
            mutate(interview_order = pm) %>%
            arrange(interview_order)
        sid <- 6:10
    }

    assignments <- interviewer_assignments %>%
        inner_join(df["interview_date"], by = c("key" = "interview_date")) %>%
        filter(value)

    if (df$lcep) {
        sessions <- filter(sessions, lcep | is.na(lcep))
    } else {
        sessions <- filter(sessions, !lcep | is.na(lcep))
    }

    sessions$interview_order[sid] <- sid[c(df$assignment:length(sid), 1:(df$assignment - 1))][1:5]

    sessions <- arrange(sessions, interview_order)

    times <- sessions %>%
        mutate(cum_duration = cumsum(duration),
               tmp_start = as.POSIXct(df$interview_date + start_time),
               stop_time = tmp_start + minutes(cum_duration),
               start_time = stop_time - minutes(duration),
               itinerary_time = paste(format(start_time, "%I:%M %p"),
                                      format(stop_time, "%I:%M %p"),
                                      sep = " - "))

    attendees <- left_join(sessions, assignments, by = "session_id") %>%
        left_join(interviewers, by = "initials") %>%
        group_by(session_id) %>%
        summarize(attendee = str_c(interviewer, collapse = "; "))

    sessions <- left_join(sessions, attendees, by = "session_id")

    tbl_itinerary <- tibble(
        Time = times$itinerary_time,
        `Interview Activities` = sessions$session,
        Attendees = sessions$attendee
    ) %>%
        FlexTable(
            body.cell.props = cellProperties(padding = 2),
            body.par.props = parProperties(text.align = "center"),
            body.text.props = textProperties(font.family = "Times"),
            header.cell.props = cellProperties(padding = 2),
            header.par.props = parProperties(text.align = "center"),
            header.text.props = textProperties(font.weight = "bold", font.family = "Times")
        ) %>%
        setFlexTableWidths(c(1.9, 2.2, 2.5)) %>%
        setZebraStyle(odd = "light blue", even = "white")

    mydoc <- docx(template = "ref/template_itinerary.docx") %>%
        map_title(stylenames = "Candidate") %>%
        addParagraph(candidate, stylename = "Candidate", bookmark = "Name") %>%
        addParagraph(interview_date, stylename = "Candidate", bookmark = "Date") %>%
        addFlexTable(tbl_itinerary, par.properties = parProperties(text.align = "center"))

    file_name <- paste0("report/itineraries/", df$interview_date, "_", df$last_name, "_", df$first_name, ".docx")
    writeDoc(mydoc, file = file_name)
}
