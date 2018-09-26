#TidyTuesday Invasive Species
library(ggplot2)
library(readr)
library(dplyr)
library(rgeos)
library(maptools)
library(viridis)
library(scales)
library(geomapdata)
library(extrafont)
library(ggalt)

africa_species <- read_csv("./africa_species.csv")
africa_species$country[africa_species$country == "Democratic Republic of the Congo"] <- "Democratic Republic of Congo"
africa_species$country[africa_species$country == "United Republic of Tanzania"] <- "Tanzania"
africa_species$country[africa_species$country == "Gambia (the)"] <- "Gambia"

africa_species$genus <- vapply(strsplit(africa_species$species, " "), `[`, 1, FUN.VALUE=character(1))
africa_species$epithet <- vapply(strsplit(africa_species$species, " "), `[`, 2, FUN.VALUE=character(1))

topgenera <- africa_species %>% group_by(genus) %>% count() %>% arrange(desc(n))

topleth <- africa_species %>% filter(kingdom == 'Plantae') %>% group_by(country) %>% count()

highest <- topleth[topleth$n>400,]
#map
countries <- readShapeSpatial("./Africa.shp")
countries <- fortify(countries,region='COUNTRY')


highestcoord <- countries %>% filter(id %in% highest$country) %>% group_by(id) %>% summarize(cenlong = mean(long), cenlat = mean(lat))
highestcoord$n <- c(404,528,1447)
sa <- highestcoord[3,]
highestcoord <- highestcoord[-3,] #need to add South Africa and Seychelles separately

###theme
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

loadfonts(device = "win")

#pretty scale breaks
pretty <- pretty_breaks(n=9)(topleth$n)

minVal <- min(topleth$n, na.rm = T)
maxVal <- max(topleth$n, na.rm = T)

labels <- c()
brks <- c(minVal, pretty)

for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]

topleth$brks <- cut(topleth$n, breaks = brks, include.lowest = TRUE, labels = labels)

brks_scale <- levels(topleth$brks)
labels_scale <- rev(brks_scale)

#final map
ggplot() + geom_map(data = topleth,aes(map_id = country, fill = brks), map=countries) +
  geom_path(data = countries, aes(x = long, y = lat, group = group), color = "sienna4", size = 0.1)+
  expand_limits(x = countries$long, y=countries$lat) + 
  annotate("text", x = highestcoord$cenlong, y = highestcoord$cenlat, label=paste(highestcoord$id,"\n",highestcoord$n))+
  annotate("text", x = sa$cenlong, y = (sa$cenlat - 8), label=paste(sa$id,"\n 1447"))+
  annotate("text", x = 55.45, y = -4.61, label = "Seychelles \n 911") + 
  theme_map() + labs(caption="Source: Paini et al. 2016",title="Invasive Plant Species in Africa") +
  theme(plot.title=element_text(size=16,  family="Calibri",hjust=0.5),legend.position='bottom') +
  scale_fill_manual(values = rev(plasma(10)[1:9]),
    breaks = rev(brks_scale),
    name = "Count of invasive plants",
    drop = FALSE,
    labels = rev(brks_scale),
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2,units="mm"),
      keywidth = unit(70 / length(labels),units="mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"))


###threat/source of each country
threat <- read_csv("./table_2.csv")
source <- read_csv("./table_4.csv")

#convert to billions
threat <- threat %>% mutate(tcostbil = invasion_cost/1000000000)
source <- source %>% mutate(scostbil = invasion_cost/1000000000)

threattrunc <- threat %>% slice(c(1:5,120:125))

threatsrc <- inner_join(threattrunc,source,by="country")
colnames(threatsrc) <- c("country","threatcost","threat_rank","tcostbil","sourcecost","source_rank","scostbil")
threatsrc$country <- factor(threatsrc$country,levels=rev(threatsrc$country))

dumbbell <- ggplot() + geom_segment(data=threatsrc,aes(y=country,yend=country,x=0,xend=124),color="#b2b2b2",size=0.15) +
  geom_dumbbell(data=threatsrc,aes(y=country,x=source_rank,xend=threat_rank),size=1.5,color="#b2b2b2", size_x=5, size_xend=5,
                colour_x="#0b4a58", colour_xend="#e0375a")+
  geom_text(data=filter(threatsrc, country=="China"),aes(x=15, y=country, label="Source"), #above
            color="#0b4a58", size=3, vjust=-2, fontface="bold", family="Calibri") +
  geom_text(data=filter(threatsrc, country=="China"),aes(x=threat_rank, y=country, label="Threat"), #above
            color="#e0375a", size=3, vjust=-2, fontface="bold", family="Calibri")+
  geom_text(data=threatsrc, aes(x=threat_rank, y=country, label=threat_rank), #below
            color="#e0375a", size=2.75, vjust=2.5, family="Calibri") +
  geom_text(data=threatsrc, aes(x=source_rank, y=country, label=source_rank), #below
            color="#0b4a58", size=2.75, vjust=2.5, family="Calibri") +
  labs(x=NULL, y=NULL, title="Invasive plant threats and sources",
       subtitle="Country ranks of total invasion cost, top and bottom 5 threats",
       caption="Source: Paini et al.2016") +
  theme_bw(base_family="Calibri") +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_blank(),plot.title=element_text(face="bold"))