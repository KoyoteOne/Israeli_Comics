#Importing and cleaning
library(tidyverse)
library(visdat)
library(rstudioapi)
library(rstatix)

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
comics_clean$stages <- str_split(comics_clean$stage, pattern = ",", simplify = T)
comics_clean$stages[,1:2] <- str_trim(comics_clean$stages[,1:2])


#get stages as new df and write to stages file
unique_stages <- comics_clean$stages[,1:2] %>%
  unlist() %>%
  c() %>% 
  unique()

stages_raw <- tibble(unique_stages)
write.csv(stages_raw, file = "..output/stages.csv", row.names = FALSE)

#Analysis

#locating missing values (stage NA possibly related to status)
comics_clean %>% 
  arrange(status) %>% 
  vis_miss()

#Gender analysis

#status by sex bar plots
comics_clean %>% 
  ggplot(aes(x = status, fill = sex), na.rm = TRUE)+
  geom_bar(position = position_dodge(width = 0.7), alpha = 0.8)+
  theme_classic()+
  labs(x = "סטטוס", y = "קומיקאים פעילים", fill = "מין")


#aggregated by sex
comics_clean %>% 
  ggplot(aes(x = sex, fill = status), na.rm = TRUE)+
  geom_bar(position = "fill")+
  theme_classic()+
  labs(x = "מין", y = "קומיקאים פעילים", fill = "סטטוס")


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



#looks like gender is associated with status. let's chisquare:
#H0: Gender unassociated (proportions of status unnaffected by gender)
#H1: Gender associated

alpha <- 0.05
chisq_test(comics_clean$sex, comics_clean$status, correct = F)

#Well, ha.

##################################################
#IN DEV: regulars per line/club
#BUG? Same people count twice

dirty_regulars1 <- comics_clean %>% 
  filter(status == "קביעות במועדונים") %>%
  group_by(stages[,1], sex) %>% 
  summarize(regulars = n())

dirty_regulars2 <- comics_clean %>% 
  filter(status == "קביעות במועדונים") %>%
  group_by(stages[,2], sex) %>% 
  summarize(regulars = n())

dirty_regulars <- full_join(dirty_regulars1, dirty_regulars2, by = c("stages[, 1]" = "stages[, 2]", "sex"))

clean_regulars <- dirty_regulars %>% 
  replace_na(list(regulars.x = 0, regulars.y = 0)) %>% 
  transmute(regulars = regulars.x + regulars.y, sex = sex) %>% 
  arrange(desc(regulars)) %>% 
  filter(`stages[, 1]` != "")

clean_regulars %>% 
  ggplot(aes(`stages[, 1]`, regulars, fill = sex))+
  geom_col()+
  coord_flip()+
  theme_classic()+
  labs(x = "במות והרכבים", y = "משתתפים.ות קבועים.ות", fill = "מין")

##################################################



