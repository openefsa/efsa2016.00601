source("./combine.R")


##efsa + Magarey
base_layer() +
    nuts0_layer()+
    worldCountries_layer(world.eu) +
    nuts3_layer() +
    citrusSurface_layer(alpha=0.5) +
    koppen_layer("med",palette = "Greens")

readline(prompt="Press [enter] to continue")


## citrus ha + magarey suitability
png("surfaceMaghery.png",width = 1400,height = 800)
base_layer() +
    nuts0_layer()+
    worldCountries_layer(world.eu) +
    nuts3_layer() +
    citrusSurface_layer(alpha=0.5) +
    magarey_layer("asco_days_average","pyc_infection_years") +
    tm_format_Europe()
dev.off()



infection_layer("./spores2/Asco_3_15_Model_AVG.xlsx","Sep",
                "Proportion of infection events (%)") +
    koppen2_layer(alpha = 0.5)


koppen2_layer(alpha = 1) +
    infection_layer("./spores2/Asco_3_15_Model_AVG.xlsx","Sep",
                    "Proportion of infection events (%)",alpha = 0.2) 


