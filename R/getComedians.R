#Importing and cleaning
library(tidyverse)
comics_raw <- read_csv("data/IsraeliComics2022_EDIT.csv", #edited file
                       col_names  = TRUE,
                       col_types = "cfcff") #c = char; f = factor
levels(comics_raw$sex) <- c("נקבה", "זכר")
comics_clean <- comics_raw[!is.na(comics_raw$name),] #fuck NA
comics_f <- comics_clean[comics_clean$sex == "נקבה",]
comics_m <- comics_clean[comics_clean$sex == "זכר",]
#splits stage into list of chr vectors, and finds unique values
comics_clean$stages <- str_split(comics_clean$stage, pattern = ",")
unique_stages <- unique(unlist(comics_clean$stages))


#Analysis
comics_clean %>% 
  ggplot(aes(x = status, fill = sex), na.rm = TRUE)+
  geom_bar(position = "dodge")

#general pie
status_tab <- table(comics_clean$status)
status_cent <- round(status_tab/sum(status_tab)*100)
pie(status_tab[1:7], labels = paste0(names(status_tab), " (", status_cent, "%)"),
    main = "קומיקאים בישראל, לפי סטטוס")

#female pie
status_tab_f <- table(comics_f$status)
status_cent_f <- round(status_tab_f/sum(status_tab_f)*100)
pie(status_tab_f[1:7], labels = paste0(names(status_tab_f), " (", status_cent_f, "%)"),
    main = "נשים")

#male pie
status_tab_m <- table(comics_m$status)
status_cent_m <- round(status_tab_m/sum(status_tab_m)*100)
pie(status_tab_m[1:7], labels = paste0(names(status_tab_m), " (", status_cent_m, "%)"),
    main = "גברים")