p_load(dplyr)
p_load(stringr)
p_load(readxl)
p_load(tidyr)
source("utils.R")
read_greekNuts3 <- function() {

    nuts3Change2013 <- read_nuts3Change2013()
    nonNuts3 <- c("Greece Total",
                 "Region of Eastern Macedonia and Thrace",
                 "Region of Central Macedonia",
                 "Region of Epirus",
                 "Region of Thessally",
                 "Region of Central Greece",
                 "Region of Ionian Islands",
                 "Region of Western Greece",
                 "Region of Peloponnese",
                 "Region of Attica",
                 "Region of Northern Aegean",
                 "Region of Crete")

    namesCorrespondents <- read_excel(paste0(getwd(),"/greece/original/nutsNamesCorrespondents.xls"))
    names(namesCorrespondents) <- make.names(names(namesCorrespondents))
    
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
        filter(!is.na(ha)) %>%
        filter(!name %in% nonNuts3)


    
    nuts3=readNutsLevels(2010)
    nuts.greece <-  nuts3 %>% filter(grepl("EL",NUTS.Code)) %>%
        separate(Description,into=c("Description.greek","Description.latin"),remove=F,sep=" \\(",fill="right") %>%
        mutate(Description.latin=gsub(")","",Description.latin,fixed=T))

   
    namesCorrespondents <- namesCorrespondents %>%
        filter(!is.na(Correspondence.to.column..nuts.seq.)) 
  

    data <- data %>%
        left_join(namesCorrespondents,by=c("greekName"="Stats.from.greece")) %>%
        select(greekName,ha,name,Nuts3...2010,Correspondence.to.column..nuts.seq.) %>%
        filter(!is.na(Correspondence.to.column..nuts.seq.)) %>%
        left_join(namesCorrespondents,by=c("Correspondence.to.column..nuts.seq."="Nuts.seq")) %>%
        select(greekName, ha,name, Nuts3...2010.y)

    levels2010=readNutsLevels(2010) %>%
        filter(grepl("^EL",NUTS.Code)) %>%
        separate(Description,into=c("greekName","latinName"),extra="drop",sep="\\(") %>%
        mutate(greekName=str_trim(greekName))
    

    data <- data %>%
        left_join(levels2010,by=c("Nuts3...2010.y"="greekName")) %>%
        select(Nuts3...2010.y,NUTS.Code,ha) %>%
        filter(!is.na(NUTS.Code)) %>%
        left_join(nuts3Change2013,by=c("NUTS.Code"="Code_2010")) %>%
        select(Nuts3...2010.y,Code_2013,ha) %>%
        rename(name=Nuts3...2010.y,
               NUTS.Code.Country=Code_2013) %>%
        mutate(year=2012,
               country="EL",
               ha=as.numeric(ha),
               date="18/01/2016",
               comment="132944 out of 549833 ha from the data could not be mapped to the official NUTS 2013 regions and are omitted from the data.",
               source="Via email on 15/01/2016 from ELSTAT",
               sourceFile="Κουφάκης/5α.Δενδρ.καλ.,εκτ.συνεχ.κανον.δενδρ.,Περιφέρεια, Π.Ε.,2012.xls")
    data

}
