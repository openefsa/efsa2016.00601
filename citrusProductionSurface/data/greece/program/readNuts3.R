p_load(dplyr)
p_load(stringr)
p_load(readxl)
p_load(tidyr)
source("utils.R")
read_greekNuts3 <- function() {

    nuts3Change2013 <- read_nuts3Change2013()

    data <- read_excel(paste0(getwd(),"/greece/original/Κουφάκης/5α.Δενδρ.καλ.,εκτ.συνεχ.κανον.δενδρ.,Περιφέρεια, Π.Ε.,2012.xls"),skip=6,
                      col_types=rep("text",24),
                      col_names= letters[1:24])

   
    data <- data %>% select(a,c,u) %>%
        slice(1:100) %>%
        rename(name=u,
               ha=c,
               greekName=a) %>%
        mutate(ha=ifelse(ha=="—",NA,ha),
               name=str_trim(name),
               greekName=str_trim(greekName)) %>%
        filter(!is.na(ha))

    nuts3=readNutsLevels(2010)
    nuts.greece <-  nuts3 %>% filter(grepl("EL",NUTS.Code)) %>%
        separate(Description,into=c("Description.greek","Description.latin"),remove=F,sep=" \\(",fill="right") %>%
        mutate(Description.latin=gsub(")","",Description.latin,fixed=T))

    namesCorrespondents <- read_excel(paste0(getwd(),"/greece/original/nutsNamesCorrespondents.xls"))
    names(namesCorrespondents) <- make.names(names(namesCorrespondents))

    data <- data %>%
        left_join(namesCorrespondents,by=c("greekName"="Stats.from.greece")) %>%
        select(greekName,ha,name,Nuts3...2010,Correspondence.to.column..nuts.seq.) %>%
        filter(!is.na(Correspondence.to.column..nuts.seq.)) %>%
        left_join(namesCorrespondents,by=c("Correspondence.to.column..nuts.seq."="Nuts.seq")) %>%
        select(greekName, ha,name, Nuts3...2010.y)

    levels2010=readNutsLevels(2010) %>%
        filter(grepl("^EL",NUTS.Code)) %>%
        separate(Description,into=c("greekName","latinName"),extra="drop")

    data <- data %>%
        left_join(levels2010,by=c("Nuts3...2010.y"="greekName")) %>%
        select(Nuts3...2010.y,NUTS.Code,ha) %>%
        left_join(nuts3Change2013,by=c("NUTS.Code"="Code_2010")) %>%
        select(Nuts3...2010.y,Code_2013,ha) %>%
        rename(name=Nuts3...2010.y,
               NUTS.Code.Country=Code_2013) %>%
        mutate(year=2013,
               country="GR",
               ha=as.numeric(ha),
               date="18/01/2016",
               source="Via email on 15/01/2016 from ELSTAT",
               sourceFile="Κουφάκης/5α.Δενδρ.καλ.,εκτ.συνεχ.κανον.δενδρ.,Περιφέρεια, Π.Ε.,2012.xls")
    data

}
