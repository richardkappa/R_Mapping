library("rgdal")
library("rgeos")
library("maptools")
library("dplyr")


#remove holes from https://stackoverflow.com/questions/44025069/merging-polygons-of-a-spatialpolygondataframe
RemoveHoles <- function(SPol,limitHoles=+Inf){
  fn <- function(subPol){
    if(subPol@hole && subPol@area < limitHoles) 
      keep <- FALSE
    else
      keep <- TRUE
    return(keep)
  }
  nPol <- length(SPol)
  newPols <- list()
  for(iPol in 1:nPol){
    subPolygons <- list()
    pol <- SPol@polygons[[iPol]]
    for(iSubPol in 1:length(pol@Polygons)){
      subPol <- pol@Polygons[[iSubPol]]
      
      if(fn(subPol))
        subPolygons[[length(subPolygons)+1]] <- subPol
    }
    newPols[[length(newPols)+1]] <- Polygons(subPolygons,pol@ID)
  }
  newSPol <- SpatialPolygons(newPols,proj4string=CRS(proj4string(SPol)))
  # SPolSimple <- gSimplify(newSPol,tol=0.01)
  newSPol <- createSPComment(newSPol)
  return(newSPol)
}

#Disolve down to postcode area
Area1 <- gUnaryUnion(PC_Shape,id=PC_Shape$PC_AREA)

#Remove any holes
Area <- RemoveHoles(Area1)
rm(Area1)

#Postcode district and sector
District <- RemoveHoles(gUnaryUnion(PC_Shape, id=PC_Shape$District))
Sector <- RemoveHoles(gUnaryUnion(PC_Shape, id=PC_Shape$Sector))

#Plot in random colours
randCol <- floor(runif(100, min=1, max=100))

plot(Area, col=colors()[randCol])
plot(District, col=colors()[randCol])
plot(Sector, col=colors()[randCol])
plot(PC_Shape, col=colors()[randCol])

#Add some data onto the new shapefiles
#Based on code from here https://philmikejones.wordpress.com/2015/09/03/dissolve-polygons-in-r/

  #First, get the data to add back on
Sector.LU <- Postcodes %>%
  group_by(Sector) %>%
  summarize(n())

names(Sector.LU) <- c("Sector", "Postcodes")

District.LU <- Postcodes %>%
  group_by(District) %>%
  summarize(n())

names(District.LU) <- c("District", "Postcodes")

Area.LU <- Postcodes %>%
  group_by(PC_AREA) %>%
  summarize(n())

names(Area.LU) <- c("PC_AREA", "Postcodes")

# make sure row names match
row.names(Area) <- as.character(1:length(Area))
row.names(District) <- as.character(1:length(District))
row.names(Sector) <- as.character(1:length(Sector))

# And add the data back in
Area <- SpatialPolygonsDataFrame(Area, Area.LU)
District <- SpatialPolygonsDataFrame(District, District.LU)
Sector <- SpatialPolygonsDataFrame(Sector, Sector.LU)

#Plot, colouring by the number of postcodes
spplot(Area, "Postcodes", main = "Postcodes per Area", col = "transparent")
spplot(District, "Postcodes", main = "Postcodes per District", col = "transparent")
spplot(Sector, "Postcodes", main = "Postcodes per Sector", col = "transparent")
