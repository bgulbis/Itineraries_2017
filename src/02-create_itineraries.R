# create itineraries

library(tidyverse)
library(lubridate)
library(ReporteRs)

interview_times <- read_rds("data/tidy/interview_times.Rds")
interview_sessions <- read_csv("data/raw/interview_sessions.csv")

# for (i in 1:nrow(interview_times)) {
for(i in 1:2) { # for testing
    df <- interview_times[i, ]

    # need to replace credentials with a variable
    candidate <- paste(df$first_name, paste0(df$last_name, ",")) #, "PharmD Candidate")
    interview_date <- format(df$interview_date, "%A, %B %e, %Y")

    if (!df$pm) {
        start_time <- hm("08:00")
        sessions <- interview_sessions
    } else {
        start_time <- hm("11:15")
        sessions <- interview_sessions %>%
            filter(!is.na(pm)) %>%
            arrange(pm)
    }

    assignments <- tibble(
        assignment = rep(1:5, each = 5),
        session_order = rep(1:5, times = 5)
    )

    times <- sessions %>%
        mutate(cum_duration = cumsum(duration),
               tmp_start = as.POSIXct(df$interview_date + start_time),
               stop_time = tmp_start + minutes(cum_duration),
               start_time = stop_time - minutes(duration),
               itinerary_time = paste(format(start_time, "%I:%M %p"),
                                      format(stop_time, "%I:%M %p"),
                                      sep = " - "))

    activities <- sessions$session

    df_itinerary <- tibble(
        Time = times$itinerary_time,
        `Interview Activities` = activities,
        Attendees = "attendees"
    )

    tbl_itinerary <- vanilla.table(df_itinerary) %>%
        setFlexTableWidths(c(1.85, 1.85, 2.9)) %>%
        setZebraStyle(odd = "light blue", even = "white")
    tbl_itinerary[, to = "header"] <- textProperties(font.weight = "bold")
    tbl_itinerary[, to = "header"] <- parCenter()
    tbl_itinerary[] <- parCenter()

    mydoc <- docx(template = "ref/template_itinerary.docx") %>%
        map_title(stylenames = "Candidate") %>%
        addParagraph(candidate, stylename = "Candidate", bookmark = "Name") %>%
        addParagraph(interview_date, stylename = "Candidate", bookmark = "Date") %>%
        addFlexTable(tbl_itinerary, par.properties = parProperties(text.align = "center"))

    file_name <- paste0("report/itineraries/", df$interview_date, "_", df$last_name, "_", df$first_name, ".docx")
    writeDoc(mydoc, file = file_name)
}
