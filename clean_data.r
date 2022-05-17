data <- read_csv("Data/combined_data.csv")

data[4:14] <- sapply(data[4:14], as.numeric)
names(data)[c(12,14)] <- c("NO3_N", "E_coli")

NBear <- c("North Bear", "N. Bear", "N Bear", "NBear")
LC <- c("Tile", "Lime240", "Lime250", "Lime290", "LimeFin", "LimeHam", "Lime230", "Lime270", "Lime270E", "Lime270W", "Lime280E", "Lime280W", "Lime295", "Lime310", "LimeFrost", "Lime 240", "Lime 250", "Lime 290", "Lime Fin", "Lime Ham")
IC <- c("Dry", "ICLM", "ICS", "ICThom", "ICN")
MC <- c("MCJ", "MC42", "MC100", "MCKenn", "MCNoel")

data <- data %>%
  mutate(Watershed = Site) %>%
  mutate(Watershed = if_else(Watershed %in% NBear, "North Bear", Watershed)) %>%
  mutate(Watershed = if_else(Watershed %in% LC, "Lime", Watershed)) %>%
  mutate(Watershed = if_else(Watershed %in% IC, "Indian Creek", Watershed))%>%
  mutate(Watershed = if_else(Watershed=="Otter 2", "Otter", Watershed)) %>%
  mutate(Watershed = if_else(Watershed %in% MC, "Morgan", Watershed)) %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30")) %>%
  relocate(Watershed, .before=Site)

write.csv(data, "Data/combined_data_clean.csv", row.names=F)