install.packages("rgeos")

library(rgeos)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

lise<-readxl::read_xlsx("latlongs1.xlsx",sheet = 2)
plastic<-readxl::read_xlsx("latlongs1.xlsx",sheet = 3)
flowmeter<-readxl::read_xlsx("flowmeter_plastics1.xlsx")

flowmeter$concentration <- flowmeter$`counts total`/flowmeter$`m3 per trawl`
flowmeter$conc <- as.numeric(flowmeter$concentration)

## Fix coordinates
coo2dec<-function(coordinate){
  if(is.vector(coordinate)) coordinate<-matrix(coordinate,ncol=4)
  measure<-coordinate[,1]+coordinate[,2]/60+coordinate[,3]/3600
  letter<-coordinate[,4]
  if(any(letter=="N"|letter=="S")){
    sign<-(coordinate[,4]!="N")*-1+(coordinate[,4]=="N")*+1
  }else{
    sign<-(coordinate[,4]!="E")*-1+(coordinate[,4]=="E")*+1
  }
  
  return(sign*measure)
}

slat<-coo2dec(lise[,1:4])
slat<-slat$`Start Lattitude`
slon<-coo2dec(lise[,5:8])
slon<-slon$`Start Longitude`

df <- cbind(slat, slon, flowmeter[,c("conc")])

## included in GMM analyses - upper half more or less
df$INC <- 0
df[which(df$slat > 22),]$INC <- "Included"
df[which(df$slat < 22),]$INC <- "Excluded"
df[which(df$conc == 0),]$INC <- "No plastic"

###########################
## Figure 1
world = ne_countries(scale = "medium", returnclass = "sf")

gg <- ggplot(data = world) + geom_sf(fill = "grey80", color = "grey80") + xlab("Longitude") + ylab("Latitude") +
  geom_point(data = df, aes(x = slon, y = slat, size = conc*1000, color = INC, shape = INC), alpha = 0.6) + theme_bw() +
  xlim(-40, 15) + ylim(-35, 60)

gg <- gg + scale_color_manual(values = c("firebrick2", "chartreuse3", "black"), guide = FALSE)
gg <- gg + guides(size = guide_legend(title = "Concentration (Particle/L)"))
gg <- gg + scale_size_continuous(breaks = c(0, 250, 500, 750, 1000))
gg <- gg + scale_shape_manual(values = c(19,19,1), guide = FALSE) 
gg

ggsave(file = "Map_Fig1.tiff", plot = gg, width = 14, height = 18, units = "cm", dpi = 300)
ggsave(file = "Map_Fig1.pdf", plot = gg, width = 14, height = 18, units = "cm", dpi = 300)



