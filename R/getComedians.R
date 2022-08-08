#Importing and cleaning
library(tidyverse)
library(visdat)
library(rstudioapi)

#set working directory to /R
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

comics_raw <- read_csv("..data/IsraeliComics2022_EDIT.csv", #edited file
                       col_names  = TRUE,
                       col_types = "cfcff") #c = char; f = factor
levels(comics_raw$sex) <- c("נקבה", "זכר")
comics_clean <- comics_raw[!is.na(comics_raw$name),] #fuck NA
comics_f <- comics_clean[comics_clean$sex == "נקבה",]
comics_m <- comics_clean[comics_clean$sex == "זכר",]

#splits stage into list of chr vectors, and finds unique values
comics_clean$stages <- str_split(comics_clean$stage, pattern = ",")

#get stages as new df and write to stages file
unique_stages <- comics_clean$stages %>% 
  unlist() %>% 
  str_trim() %>% 
  unique()

stages_raw <- data.frame(unique_stages)
write.csv(stages_raw, file = "..output/stages.csv", row.names = FALSE)

#Analysis

#locating missing values (stage NA possibly related to status)
comics_clean %>% 
  arrange(status) %>% 
  vis_miss()

#status by sex bar plots
comics_clean %>% 
  ggplot(aes(x = status, fill = sex), na.rm = TRUE)+
  geom_bar(position = "dodge")

#aggregated by sex
comics_clean %>% 
  ggplot(aes(x = sex, fill = status), na.rm = TRUE)+
  geom_bar()


#general status pie
status_count <- comics_clean %>% count(status)
status_cent <- round(status_count$n/sum(status_count$n)*100)
pie(status_count$n, labels = paste0(status_count$status, " (", status_cent, "%)"),
    main = "קומיקאים בישראל, לפי סטטוס")

#female status pie
status_count_f <- comics_f %>%  count(status)
status_cent_f <- round(status_count_f$n/sum(status_count_f$n)*100)
pie(status_count_f$n, labels = paste0(status_count_f$status, " (", status_cent_f, "%)"),
    main = paste0("(n = ", sum(status_count_f$n),")  נשים"))

#male status pie
status_count_m <- comics_m %>%  count(status)
status_cent_m <- round(status_count_m$n/sum(status_count_m$n)*100)
pie(status_count_m$n, labels = paste0(status_count_m$status, " (", status_cent_f, "%)"),
    main = paste0("(n = ", sum(status_count_m$n),")  גברים"))
