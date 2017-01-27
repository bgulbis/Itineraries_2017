# create itineraries

library(tidyverse)
library(ReporteRs)

interview_times <- read_rds("data/tidy/interview_times.Rds")

# for (i in 1:nrow(interview_times)) {
for(i in 1:2) { # for testing
    df <- interview_times[i, ]

    candidate <- paste(df$first_name, paste0(df$last_name, ","), "PharmD Candidate")
    interview_date <-

    mydoc <- docx(template = "ref/template_itinerary.docx") %>%
        map_title(stylenames = "Candidate") %>%
        addParagraph(candidate, stylename = "Candidate", bookmark = "Name")

    file_name <- paste0("report/itineraries/", df$interview_date, "_", df$last_name, "_", df$first_name, ".docx")
    writeDoc(mydoc, file = file_name)
}
