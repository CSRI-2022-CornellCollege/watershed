watershed_data <- read_csv("Data/combined_data_clean2.csv")

temp <- read_excel("Data/newdata.xlsx")[c(1,2,4,5,6,7,8)]

data <- left_join(watershed_data, temp, by=c("Site", "Date"), suffix=c("", ".y")) %>%
  mutate(DO=ifelse(is.na(DO), DO.y, DO)) %>%
  mutate(Temp=ifelse(is.na(Temp), Temp.y, Temp)) %>%
  mutate(pH=ifelse(is.na(pH), pH.y, pH)) %>%
  mutate(Cond=ifelse(is.na(Cond), Cond.y, Cond)) %>%
  mutate(Turb=ifelse(is.na(Turb), Turb.y, Turb)) %>%
  dplyr::select(1:15)

write_csv(data, "Data/combined_data_clean3.csv")
write_csv(data, "watershed_app/data/combined_data_clean3.csv")