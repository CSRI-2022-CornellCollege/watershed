bear_rain <- read_csv("Data/rain_data/bear_rain.csv")
blue_rain <- read_csv("Data/rain_data/blue_rain.csv")
indian_rain <- read_csv("Data/rain_data/indian_rain.csv")
lime_rain <- read_csv("Data/rain_data/lime_rain.csv")
morgan_rain <- read_csv("Data/rain_data/morgan_rain.csv")
mud_rain <- read_csv("Data/rain_data/mud_rain.csv")
nbear_rain <- read_csv("Data/rain_data/nbear_rain.csv")
otter_rain <- read_csv("Data/rain_data/otter_rain.csv")

watershed_rain_data <- data.frame(Date=bear_rain$Date,
                                  Bear=bear_rain$Estimate,
                                  Blue=blue_rain$Estimate,
                                  Indian=indian_rain$Estimate,
                                  Lime=lime_rain$Estimate,
                                  Morgan=morgan_rain$Estimate,
                                  Mud=mud_rain$Estimate,
                                  NBear=nbear_rain$Estimate,
                                  Otter=otter_rain$Estimate)

write_csv(watershed_rain_data, "Data/rain_data/watershed_rain_data.csv")
write_csv(watershed_rain_data, "watershed_app/data/rain_data/watershed_rain_data.csv")