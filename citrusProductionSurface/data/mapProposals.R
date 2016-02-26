

source("./combine.R")

usePng <- T

nextPlot <- function(name) {
    if (usePng & !names(dev.cur())=="null device") {
        dev.off()
    }
    if (usePng) {
        png(paste0("./output/",name,".png"),width = 1366,height=768)
    } else {
        readline(prompt=paste0(name,": Press [enter] to continue"))
        
    }

}

system("rm output/*.png")

nextPlot("surface_ha")
## surface

base_layer() +
    worldCountries_layer(world.eu) +
    citrusSurface_layer(column="ha",alpha=1) +
    nuts0_layer()+
    nuts3_layer() +
    
    tm_format_Europe()

nextPlot("surface_density")
## surface 

base_layer() +
    worldCountries_layer(world.eu) +
    
    citrusSurface_layer(column="citrus_density",alpha=1) +
    
    nuts3_layer() +
    nuts0_layer()+

    tm_format_Europe() 




## surface + magarey, ascospores
nextPlot("surface_MagAsco")
##OK
base_layer() +
    worldCountries_layer(world.eu) +
    citrusSurface_layer(column="ha") +
    nuts0_layer()+
    nuts3_layer() +
    magarey_layer("asco_days_average","asco_suit_years",
                  title.size = "Ascospores \ninfection score",
                  title.col = "Ascospores \n(% suitable years)",
                  style = "pretty") +
    tm_format_Europe()




## surface + magarey, pycnidiospores
nextPlot("surface_MagPycnidio")
##OK
base_layer() +
    worldCountries_layer(world.eu) +
    citrusSurface_layer(alpha=1,column="ha") +
    nuts3_layer() +
    nuts0_layer()+
    magarey_layer("pyc_average","pyc_suit_years",title.size = "Pycnidiospores \ninfection score",title.col = "Pycnidiospores \n(% suitable years)",style="cat") +
    tm_format_Europe()




##surface + EFSA 2014
nextPlot("surface_efsa2014")
## renove grid borders
## try schraffierung 
base_layer() +
    nuts0_layer()+
    worldCountries_layer(world.eu) +
    nuts3_layer() +
    infection_layer("./spores2/Asco_3_15_Model_AVG.xlsx","Sep",
                    "Proportion of infection events (%)",
                    1) +
                                        #citrusSurface_outline_layer() +
    citrusSurface_dots_layer() +
    tm_format_Europe()




##surface + Martinez2015(koppen)
nextPlot("surface_MartinezKoppenBskBsh")

base_layer() +
    nuts0_layer()+
    worldCountries_layer(world.eu) +
    nuts3_layer() +
    combined_koppen_layer(whichToShow = c(5,6))+
    citrusSurface_outline_layer() 


##surface + Martinez2015(koppen)
nextPlot("surface_MartinezKoppen")

base_layer() +
    nuts0_layer()+
    worldCountries_layer(world.eu) +
    nuts3_layer() +
    combined_koppen_layer()+
    citrusSurface_outline_layer()



##surface + Martinez2015(aschmann)
nextPlot("surface_MartinezAschmann")
base_layer() +
    nuts0_layer()+
    worldCountries_layer(world.eu) +
    nuts3_layer() +
    aschmann_layer() +
    citrusSurface_outline_layer() 


nextPlot("surfaceDots_MartinezAschmann")
base_layer() +
    nuts0_layer()+
    worldCountries_layer(world.eu) +
    nuts3_layer() +
    aschmann_layer() +
    citrusSurface_dots_layer()

nextPlot("surfaceDots_MartinezKoppen")
base_layer() +
    nuts0_layer()+
    worldCountries_layer(world.eu) +
    nuts3_layer() +
    combined_koppen_layer() +
    citrusSurface_dots_layer()

nextPlot("surfaceDots_efsa2014")
base_layer() +
    nuts0_layer()+
    worldCountries_layer(world.eu) +
    nuts3_layer() +
    infection_layer("./spores2/Asco_3_15_Model_AVG.xlsx","Sep",
                    "Proportion of infection events (%)",
                    1) +
    
    citrusSurface_dots_layer()


dev.off()



## todo
## sent both (ha + density) maps to working group
## later: overlays
