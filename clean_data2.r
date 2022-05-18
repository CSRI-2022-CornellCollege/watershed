# This data cleaning script creates a clean version of the dataset without the McLoud Run sites.

# Read in data
data <- read_csv("Data/combined_data.csv")

# Make appropriate data numeric and standardize names of columns
data[4:14] <- sapply(data[4:14], as.numeric)
names(data)[c(12,14)] <- c("NO3_N", "E_coli")

# Create lists of names that are used to generate the watershed column
NBear <- c("North Bear", "N. Bear", "N Bear", "NBear")
LC <- c("Tile", "Lime240", "Lime250", "Lime290", "LimeFin", "LimeHam", "Lime230", "Lime270", "Lime270E", "Lime270W", "Lime280E", "Lime280W", "Lime295", "Lime310", "LimeFrost", "Lime 240", "Lime 250", "Lime 290", "Lime Fin", "Lime Ham")
IC <- c("Dry", "ICLM", "ICS", "ICThom", "ICN")
MR <- c("MCJ", "MC42", "MC100", "MCKenn", "MCNoel")
no_include <- c("BF1", "BF2", "BF3", "BF4", "Pond")

# Create watershed column, fix date column, remove unneeded data
data <- data[-3703,] %>%
  mutate(Watershed = Site) %>%
  mutate(Watershed = if_else(Watershed %in% NBear, "North Bear", Watershed)) %>%
  mutate(Site = if_else(Site %in% NBear, "North Bear", Site)) %>%
  mutate(Watershed = if_else(Watershed %in% LC, "Lime", Watershed)) %>%
  mutate(Watershed = if_else(Watershed %in% IC, "Indian Creek", Watershed))%>%
  mutate(Watershed = if_else(Watershed=="Otter 2", "Otter", Watershed)) %>%
  filter(!(Site %in% no_include)) %>%
  filter(!(Site %in% MR)) %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30")) %>%
  relocate(Watershed, .before=Site)

# Convert time column to string
data$Time[c(510, 2760)] <- NA
x <- data$Time*24 
minutes <- sapply((x %% 1)*60, round)
hours <- sapply(x-(minutes/60), round)
for (i in 1:length(minutes)){
  if (!(is.na(minutes[i])) & (as.numeric(minutes[i]) < 10)){
    minutes[i] = paste0("0", minutes[i])
  }
}
time <- paste0(hours, ":", minutes)
data$Time <- time
data$Time[which(is.na(minutes))] <- NA

# Write clean data to new CSV file
write.csv(data, "Data/combined_data_clean2.csv", row.names=F)

# Need more information on sites