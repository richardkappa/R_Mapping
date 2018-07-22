require("rgdal")
require("rgeos")
require("maptools")
require("ggplot2")
require("plyr")
require("dplyr")
require("igraph")

setwd("h:/desktop/Learning Networks/Shapefile eg/")

#Get the shapefile
shape <- readOGR(dsn = ".", layer = "EX_Sample")

shape2 <- shape[]

Postcodes <- shape@data
Postcodes$ID <- seq.int(nrow(Postcodes))

#Plot it
plot(shape)

#Find which postcodes border which other postcodes
tBoundaries <- gTouches(shape, byid=TRUE, returnDense = FALSE)

#Set up a blank data frame
Boundaries <- data.frame(PC1=integer(), PC2=integer())

#Switch the boundaries from a list into pairs
for (row in 1:length(tBoundaries)){
  
  PC <- as.data.frame(tBoundaries[row])
  PC$tmp <- row
  
  PC$PC1 <- apply(PC, 1, FUN=min)
  PC$PC2 <- apply(PC, 1, FUN=max)
  
  PC <- PC[c("PC1", "PC2")]
  
  Boundaries <- rbind(Boundaries, PC)
}

rm(tBoundaries)
rm(PC)

#Find the duplicate rows
dups <- duplicated(Boundaries)

#Drop the duplicate rows
Boundaries <- Boundaries[!duplicated(Boundaries), ]

#Merge on the postcode names rather than their IDs
Boundaries <- inner_join(Boundaries, Postcodes[c("ID", "POSTCODE")], by=c("PC1" = "ID"))
Boundaries <- Boundaries[c("PC2", "POSTCODE")]
names(Boundaries) <- c("PC2", "Postcode1")

Boundaries <- inner_join(Boundaries, Postcodes[c("ID", "POSTCODE")], by=c("PC2" = "ID"))
Boundaries <- Boundaries[c("Postcode1", "POSTCODE")]
names(Boundaries) <- c("Postcode1", "Postcode2")

#Convert the postcodes and neighbours into a network graph
net <- graph_from_data_frame(d=Boundaries, vertices=Postcodes, directed=F) 

#plot the network
plot(net, vertex.label=NA, vertex.size=1)

