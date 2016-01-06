library(pacman)
p_load(readxl)
p_load(dplyr)

readCitrusHectar_france_<- function() {
  
                                        # workarround for https://github.com/hadley/readxl/issues/154
    df <- read_excel("france/original/verger2015T6bsva.xls",2,F,skip=8) %>%
        tbl_df() %>%
        rename(region=X2) %>% 
        filter(region %in% c("Provence-Alpes-Côte d'Azur","Corse")) %>%
        select(region,X32,X35,X38,X41) %>%
        group_by(region) %>%
        mutate(ha=sum(X32,X35,X38,X41)) %>%
        select(region,ha) %>%
        mutate(country="France", year=2013) %>%
        ungroup() %>%
        mutate(nuts2=c("FR82","FR83"))

    nuts_coteDAzur <- nuts %>% filter(grepl("FR82.+",NUTS.Code)) %>%
        mutate(nuts2="FR82")
    nuts_corse <- nuts %>% filter(grepl("FR83.+",NUTS.Code)) %>%
        mutate(nuts2="FR83")

    nuts_both <- rbind(nuts_coteDAzur,nuts_corse) %>%
        left_join(nuts2013@data,by=c("NUTS.Code"="NUTS_ID"))


    nuts_both %>% group_by(nuts2) %>%
        mutate(nuts3_area = sum(Shape_Area),
               nuts2_area_perc = Shape_Area / nuts3_area ) %>%
        ungroup() %>%
        left_join(df,by=c("nuts2"="nuts2")) %>%
        mutate(impHa = ha * nuts2_area_perc) %>%
        select(country,year,Description,impHa) %>%
        rename(name=Description,
               ha=impHa) %>%
        mutate(comment="Remark: Total area (6 ha) of citrus production in Provence-Alpes-Côte d’Azur (FR82) was distributed to regions of NUTS level 3 (FR821-826) proportional to the area of the regions.",
               source="http://agreste.agriculture.gouv.fr/enquetes/productions-vegetales-528/vergers-et-fruits/",
               link=" http://www.agreste.agriculture.gouv.fr/IMG/xls/verger2015T6bsva.xls",
               date="06/01/2015")
        
        
}
readCitrusHectar_france<- function() {
    dummy <- capture.output(data <- readCitrusHectar_france_())
    data
}
