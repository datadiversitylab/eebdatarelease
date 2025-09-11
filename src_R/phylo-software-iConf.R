# # # # # # # # # # # # # # # # # # # # # # # # # 
#
# Phylo software analysis for iConf
# 9/11/2025
# sarah bratt 
#
# # # # # # # # # # # # # # # # # # # # # # # # #
setwd("C:/Users/sebratt/Documents/phylo_software")
p <- read.csv2("Final data with software and doi (1).csv", sep=",", header = TRUE)

p<- p[which(!duplicated(p)),]
View(head(p))
df <- as.data.frame(table(p$language))



tab <- as.data.frame(table(p$Term)) 
write.csv(tab, "table_Term_freq.csv")

fr <- data.frame(p$id, p$language)
fr <- fr[which(!duplicated(fr)),]
t <- as.data.frame(table(fr$p.language))
write.csv(t,"table_language_freq2.csv")

# AUTHOR AFFILIATION 

c <- read.csv2("phylo_soft_Author_Country_affiliations.csv", sep=",", header=TRUE)
colnames(p)[3] <-"author_id"

m <- merge(c,p,by="author_id")

country <- as.data.frame(table(p$institution_country_code))
write.csv(country, "table_author_country.csv", row.names = FALSE)
barplot(country$Freq, country$Var1)

tax <- read.csv("Countries_Territories_Taxonomy_HDX.csv")
colnames(country)[1] <- "ISO.3166.1.Alpha.2.Codes"
country2 <-merge(country, tax, by = "ISO.3166.1.Alpha.2.Codes")
colnames(country2)

country3 <-country2[,c(1,29,13,2,39,41,43)]

write.csv(country3, "table_country_with_region2.csv", row.names=FALSE)

hist <- aggregate(country3$Freq, by = list(country3$Sub.region.Name), FUN=sum)
hist2 <- aggregate(country3$Freq, by = list(country3$Intermediate.Region.Name), FUN=sum)
hist3 <- aggregate(country3$Freq, by = list(country3$Region.Name), FUN=sum)


# Install and load required packages
install.packages(c("ggplot2", "rnaturalearth", "rnaturalearthdata","sf"))
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
install.packages("sf")
library(sf)


# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Example dataset with ISO2 country codes
data <- country3
colnames(country3)[1] <- "iso_a2"
cntry <-country3[-(c(1:7)),c(3,4)]
data<-cntry

# Merge map data with your dataset
colnames(data)[1] <- "iso3"
colnames(world)
world_data2 <- merge(world, data, by.x = "iso_a3", by.y = "iso3", all.x = TRUE)

# Plot the map
ggplot(data = world_data2) +
  geom_sf(aes(fill = Freq)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  theme_minimal() +
  labs(title = "Earth Map of Phylogenetic Software (ISO3 Codes) 1990-2024", fill = "Freq")



