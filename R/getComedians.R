#Importing and cleaning
library(tidyverse)
comics_raw <- read_csv("data/IsraeliComics2022_EDIT.csv", #edited file
                       col_names  = TRUE,
                       col_types = "cfcff") #c = char; f = factor
levels(comics_raw$sex) <- c("נקבה", "זכר")
comics_clean <- comics_raw[!is.na(comics_raw$name),] #fuck NA


#splits stage into list of chr vectors, and finds unique values
comics_clean$stages <- str_split(comics_clean$stage, pattern = ",")
unique_stages <- unique(unlist(comics_clean$stages))


#Analysis
comics_clean %>% 
  ggplot(aes(x = status, fill = sex), na.rm = TRUE)+
  geom_bar(position = "dodge")


status_tab <- table(comics_clean$status)
status_cent <- round(status_tab/sum(status_tab)*100)
pie(status_tab[1:7], labels = paste0(names(status_tab), " (", status_cent, "%)"))

