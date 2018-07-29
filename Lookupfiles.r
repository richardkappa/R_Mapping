#install.packages('readr')
#install.packages('dplyr')

library('readr')
library('dplyr')

#Get the ONS postcode file
nspl.root <- "h:/desktop/Learning Networks/ONS"

nspl.nm <- "Data/NSPL_MAY_2018_UK.csv"
nspl.nm <- paste(nspl.root,nspl.nm, sep="/")

PC.Lookup <- read_csv(nspl.nm)

#Create new postcode column without spaces
PC.Lookup$Postcode_ns <- gsub(" ","",PC.Lookup$pcd,fixed=TRUE)

Keep <- c("Postcode_ns", "ru11ind", "oseast1m", "osnrth1m", "ctry", "rgn", "laua", "pcon", "ttwa", "park", "lsoa11", "msoa11", "pfa", "imd")
PC.Lookup <- PC.Lookup[Keep]

#Import and merge on the lookup files
Country.nm <- "Documents/Country names and codes UK as at 08_12.csv"
Country.nm <- paste(nspl.root,Country.nm, sep="/")
Country.Lookup <- read_csv(Country.nm)
PC.Lookup <- left_join(PC.Lookup, Country.Lookup[c("CTRY12CD", "CTRY12NM")], by=c("ctry" = "CTRY12CD"))

Region.nm <- "Documents/Region names and codes EN as at 12_10 (GOR).csv"
Region.nm <- paste(nspl.root,Region.nm, sep="/")
Region.Lookup <- read_csv(Region.nm)
PC.Lookup <- left_join(PC.Lookup, Region.Lookup[c("GOR10CD", "GOR10NM")], by=c("rgn" = "GOR10CD"))

Laua.nm <- "Documents/LA_UA names and codes UK as at 12_16.csv"
Laua.nm <- paste(nspl.root,Laua.nm, sep="/")
Laua.Lookup <- read_csv(Laua.nm)
PC.Lookup <- left_join(PC.Lookup, Laua.Lookup[c("LAD16CD", "LAD16NM")], by=c("laua" = "LAD16CD"))

West.nm <- "Documents/Westminster Parliamentary Constituency names and codes UK as at 12_14.csv"
West.nm <- paste(nspl.root,West.nm, sep="/")
West.Lookup <- read_csv(West.nm)
PC.Lookup <- left_join(PC.Lookup, West.Lookup[c("PCON14CD", "PCON14NM")], by=c("pcon" = "PCON14CD"))

Park.nm <- "Documents/National Park names and codes GB as at 08_16.csv"
Park.nm <- paste(nspl.root,Park.nm, sep="/")
Park.Lookup <- read_csv(Park.nm)
PC.Lookup <- left_join(PC.Lookup, Park.Lookup[c("NPARK16CD", "NPARK16NM")], by=c("park" = "NPARK16CD"))

LSOA.nm <- "Documents/LSOA (2011) names and codes UK as at 12_12.csv"
LSOA.nm <- paste(nspl.root,LSOA.nm, sep="/")
LSOA.Lookup <- read_csv(LSOA.nm)
PC.Lookup <- left_join(PC.Lookup, LSOA.Lookup[c("LSOA11CD", "LSOA11NM")], by=c("lsoa11" = "LSOA11CD"))

MSOA.nm <- "Documents/MSOA (2011) names and codes UK as at 12_12.csv"
MSOA.nm <- paste(nspl.root,MSOA.nm, sep="/")
MSOA.Lookup <- read_csv(MSOA.nm)
PC.Lookup <- left_join(PC.Lookup, MSOA.Lookup[c("MSOA11CD", "MSOA11NM")], by=c("msoa11" = "MSOA11CD"))

Police.nm <- "Documents/PFA names and codes GB as at 12_15.csv"
Police.nm <- paste(nspl.root,Police.nm, sep="/")
Police.Lookup <- read_csv(Police.nm)
PC.Lookup <- left_join(PC.Lookup, Police.Lookup[c("PFA15CD", "PFA15NM")], by=c("pfa" = "PFA15CD"))

TTWA.nm <- "Documents/TTWA names and codes UK as at 12_11 v5.csv"
TTWA.nm <- paste(nspl.root,TTWA.nm, sep="/")
TTWA.Lookup <- read_csv(TTWA.nm)
PC.Lookup <- left_join(PC.Lookup, TTWA.Lookup[c("TTWA11CD", "TTWA11NM")], by=c("ttwa" = "TTWA11CD"))

Keep <- c("Postcode_ns", "ru11ind", "oseast1m", "osnrth1m", "CTRY12NM", "GOR10NM", "LAD16NM", "PCON14NM", "TTWA11NM", "NPARK16NM", "LSOA11NM", "MSOA11NM", "PFA15NM")
Names <- c("Postcode_ns", "RuralIndicator", "Easting", "Northing", "Country", "Region", "LocalAuthority", "WestminsterConstituencies", "TravelToWorkArea", "NationalPark", "LowerLayerSuperOutputArea", "MiddleLayerSuperOutputArea", "PoliceForceArea")

PC.Lookup<-PC.Lookup[, Keep]
names(PC.Lookup) <- Names

#Clean up the files a bit
rm(nspl.nm, nspl.root, Keep, Names ,Country.nm, Region.nm, Laua.nm, West.nm, Park.nm, LSOA.nm, MSOA.nm, Police.nm, TTWA.nm,
   Country.Lookup, Region.Lookup, Laua.Lookup, West.Lookup, Park.Lookup, LSOA.Lookup, MSOA.Lookup, Police.Lookup, TTWA.Lookup)

#Now for the Land registry data
ds <- "http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-2016.csv"
Names <- c('Transaction_unique_identifier', 'Price', 'Date_of_Transfer', 'Postcode', 'Property_Type', 'Old_New', 'Duration', 'PAON', 'SAON', 'Street', 'Locality', 'Town_City', 'District', 'County', 'SaleType', 'Record_Status_monthly_file_only')
Lreg2016  <- read_csv(ds, col_names=Names)

ds <- "http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-2017.csv"
Names <- c('Transaction_unique_identifier', 'Price', 'Date_of_Transfer', 'Postcode', 'Property_Type', 'Old_New', 'Duration', 'PAON', 'SAON', 'Street', 'Locality', 'Town_City', 'District', 'County', 'SaleType', 'Record_Status_monthly_file_only')
Lreg2017  <- read_csv(ds, col_names=Names)

ds <- "http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-2018.csv"
Names <- c('Transaction_unique_identifier', 'Price', 'Date_of_Transfer', 'Postcode', 'Property_Type', 'Old_New', 'Duration', 'PAON', 'SAON', 'Street', 'Locality', 'Town_City', 'District', 'County', 'SaleType', 'Record_Status_monthly_file_only')
Lreg2018  <- read_csv(ds, col_names=Names)

Lreg <- rbind(Lreg2016, Lreg2017, Lreg2018)

Lreg$Postcode_ns <- gsub(" ", "", Lreg$Postcode, fixed = TRUE)

#Remove the hour and minutes from the date variable and create a month variable
Lreg$Transfer_Date <- as.Date(as.POSIXct(Lreg$Date_of_Transfer,tz="", "%Y-%m-%d %H:%M"))
Lreg$Transfer_Month <- as.Date(paste((format(Lreg$Transfer_Date, "%Y-%m")),"-01",sep=""))
Lreg$Transfer_Year <- as.numeric(format(Lreg$Transfer_Date, "%Y"))

#I want just sales from in or after 2016 and the postcode needs to exist
Lreg <- Lreg[ which(Lreg$Transfer_Year>=2016), ]
Lreg <- Lreg[ !is.na(Lreg$Postcode), ]

#Remove rows where the value is over £2M, houses can cost this much but so can supermarkets and office blocks
Lreg <- Lreg[!(Lreg$Price>2000000),]

#Keep only the variables I want to use
columns <- c('Postcode', 'Postcode_ns', 'Price', 'Transfer_Month', 'Property_Type', 'Old_New', 'Duration', "SaleType")
Lreg <- subset(Lreg,select=columns)

#Create some new factors
Lreg$Flat <- ifelse(Lreg$Property_Type=="F" & Lreg$SaleType == "A",1,0)
Lreg$Detached <- ifelse(Lreg$Property_Type=="D" & Lreg$SaleType == "A",1,0)
Lreg$SemiDetached <- ifelse(Lreg$Property_Type=="S" & Lreg$SaleType == "A",1,0)
Lreg$Terraced <- ifelse(Lreg$Property_Type=="T" & Lreg$SaleType == "A",1,0)
Lreg$OtherHouse <- ifelse(Lreg$Property_Type=="O" & Lreg$SaleType == "A",1,0)
Lreg$NewBuild <- ifelse(Lreg$Old_New=="Y" & Lreg$SaleType == "A",1,0)
Lreg$Leasehold <- ifelse(Lreg$Duration=="L" & Lreg$SaleType == "A",1,0)
Lreg$Freehold <- ifelse(Lreg$Duration=="F" & Lreg$SaleType == "A",1,0)
Lreg$AdditionalPricePaid <- ifelse(Lreg$SaleType == "B",1,0)

Lreg$Flat.Price <- Lreg$Flat * Lreg$Price
Lreg$Detached.Price <- Lreg$Detached * Lreg$Price
Lreg$SemiDetached.Price <- Lreg$SemiDetached * Lreg$Price
Lreg$Terraced.Price <- Lreg$Terraced * Lreg$Price
Lreg$OtherHouse.Price <- Lreg$OtherHouse * Lreg$Price
Lreg$NewBuild.Price <- Lreg$NewBuild * Lreg$Price
Lreg$Leasehold.Price <- Lreg$Leasehold * Lreg$Price
Lreg$Freehold.Price <- Lreg$Freehold * Lreg$Price
Lreg$AdditionalPricePaid.Price <- Lreg$AdditionalPricePaid * Lreg$Price

#Summarise the data by postcode

#Number and average sale price of normal sales
Lreg.Lookup <- group_by(Lreg[ which(Lreg$SaleType=='A'), ], Postcode_ns) %>%
  summarise(Sales = n(),
            Flat = sum(Flat),
            Detached = sum(Detached),
            SemiDetached = sum(SemiDetached),
            Terraced = sum(Terraced),
            OtherHouse = sum(OtherHouse),
            NewBuild = sum(NewBuild),
            Leasehold = sum(Leasehold),
            Freehold = sum(Freehold),
            AdditionalPricePaid = sum(AdditionalPricePaid),
            
            Flat.PriceT = sum(Flat.Price),
            Detached.PriceT = sum(Detached.Price),
            SemiDetached.PriceT = sum(SemiDetached.Price),
            Terraced.PriceT = sum(Terraced.Price),
            OtherHouse.PriceT = sum(OtherHouse.Price),
            NewBuild.PriceT = sum(NewBuild.Price),
            Leasehold.PriceT = sum(Leasehold.Price),
            Freehold.PriceT = sum(Freehold.Price),
            AdditionalPricePaid.PriceT = sum(AdditionalPricePaid.Price),
            
            AveragePrice = mean(Price))


Lreg.Lookup$Flat.AvPrice <- Lreg.Lookup$Flat.PriceT / Lreg.Lookup$Flat
Lreg.Lookup$Detached.AvPrice <- Lreg.Lookup$Detached.PriceT / Lreg.Lookup$Detached
Lreg.Lookup$SemiDetached.AvPrice <- Lreg.Lookup$SemiDetached.PriceT / Lreg.Lookup$SemiDetached
Lreg.Lookup$Terraced.AvPrice <- Lreg.Lookup$Terraced.PriceT / Lreg.Lookup$Terraced
Lreg.Lookup$OtherHouse.AvPrice <- Lreg.Lookup$OtherHouse.PriceT / Lreg.Lookup$OtherHouse
Lreg.Lookup$NewBuild.AvPrice <- Lreg.Lookup$NewBuild.PriceT / Lreg.Lookup$NewBuild
Lreg.Lookup$Leasehold.AvPrice <- Lreg.Lookup$Leasehold.PriceT / Lreg.Lookup$Leasehold
Lreg.Lookup$Freehold.AvPrice <- Lreg.Lookup$Freehold.PriceT / Lreg.Lookup$Freehold
Lreg.Lookup$AdditionalPricePaid.AvPrice <- Lreg.Lookup$AdditionalPricePaid.PriceT / Lreg.Lookup$AdditionalPricePaid

Keep <- c("Postcode_ns", "Sales", "AveragePrice", "Flat", "Flat.AvPrice",
          "Detached", "Detached.AvPrice", "SemiDetached", "SemiDetached.AvPrice",
          "Terraced", "Terraced.AvPrice", "OtherHouse", "OtherHouse.AvPrice",
          "NewBuild", "NewBuild.AvPrice", "Leasehold", "Leasehold.AvPrice",
          "Freehold", "Freehold.AvPrice", "AdditionalPricePaid.AvPrice", "AdditionalPricePaid")

Lreg.Lookup <- Lreg.Lookup[,Keep]

PC.Lookup <- left_join(PC.Lookup, Lreg.Lookup, by="Postcode_ns")
