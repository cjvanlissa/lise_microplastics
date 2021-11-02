rm(list = ls())

library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

lise<-readxl::read_xlsx("latlongs2.xlsx",sheet = 2)
plastic<-readxl::read_xlsx("latlongs2.xlsx",sheet = 3)
flowmeter<-readxl::read_xlsx("flowmeter_plastics2.xlsx")

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


## calculate concentrations, number and mass-based
flowmeter$concentration <- flowmeter$`counts total`/flowmeter$`m3 per trawl`
flowmeter$conc.num <- as.numeric(flowmeter$concentration)

flowmeter$concentration.mass <- flowmeter$`grams total`/flowmeter$`m3 per trawl`
flowmeter$conc.mass <- as.numeric(flowmeter$concentration.mass)

## onde dataframe for coordinates & concentrations
df <- cbind(slat, slon, flowmeter[,c("conc.num", "conc.mass")])

## get separate concentrations for number small and large
## sorry - very ugly part of script
test <- cbind(plastic, flowmeter[,c(6,9,10)])
test$below5 <- as.numeric(test$`Count Density <5mm`)/test$`m3 per trawl`
test$above5 <- as.numeric(test$`Count Density >5mm`)/test$`m3 per trawl`

test2 <- as.data.frame(cbind(test[,("below5")], slat))
test2$size <- "Less than 5mm"
test3 <- as.data.frame(cbind(test[,("above5")], slat))
test3$size <- "More than 5mm"

test4 <- rbind(test2, test3)

##########################
## Figure 2


coeff <- 0.01
gg <- ggplot() + geom_bar(data = test4, aes(x = slat, y = V1, fill = size),
                          stat = "identity",width = 1) 
gg <- gg + scale_fill_manual(values = c("dodgerblue4", "cadetblue3"))

gg <- gg + xlab("Latitude") + ylab("Concentration (Particle/M3)")                     
gg <- gg + theme_bw()
gg <- gg + geom_line(data = df, aes(x = slat, y = conc.mass/coeff),
                     color = "orange1") +
  geom_point(data = df, aes(x = slat, y = conc.mass/coeff), 
             color = "orange1") 

gg <- gg + scale_y_continuous(name = "Concentration(Particle/M3)", 
                              sec.axis = sec_axis(trans ~.*coeff, name = "Concentration(g/M3)"))


gg
ggsave(file = "Bar_Fig2.tiff", plot = gg, width = 20, height = 12, units = "cm", dpi = 300)
ggsave(file = "Bar_Fig1.pdf", plot = gg, width = 20, height = 12, units = "cm", dpi = 300)




