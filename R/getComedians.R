#Importing and cleaning
library(tidyverse)
comics_raw <- read_csv("data/IsraeliComics2022_EDIT.csv",
                       col_names  = TRUE,
                       col_types = "cfcff")
levels(comics_raw$sex) <- c("נקבה", "זכר")
comics_clean <- comics_raw[!is.na(comics_raw$name),]

#Analysis
comics_clean %>% 
  ggplot(aes(x = status, fill = sex), na.rm = TRUE)+
  geom_bar(position = "dodge")


status_tab <- table(comics_clean$status)
pie(status_tab[1:7], labels = paste0(names(status_tab), " (" ,round(status_tab/sum(status_tab)*100), "%)"))
status_tab
