watershed_data <- read_csv("Data/combined_data_clean3.csv")

new_data <- read_excel("Data/IndianCreek2012-2021.xlsx", na=c("", "NA", "BDL")) %>%
  filter(!is.na(Site)) %>%
  mutate(Watershed="Indian Creek") %>%
  mutate(Time=NA) %>%
  mutate(Turb=ifelse(Turb==">1000", 1000, Turb)) %>%
  relocate(Time, .before=DO) %>%
  relocate(Watershed, .before=Site)

names(new_data) <- names(watershed_data)
new_data$E_coli <- as.numeric(new_data$E_coli)
new_data$Turb <- as.numeric(new_data$Turb)


data <- rbind(watershed_data, new_data)

write_csv(data, "Data/combined_data_clean4.csv")
write_csv(data, "watershed_app/data/combined_data_clean4.csv")