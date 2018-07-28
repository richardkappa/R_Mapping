#install.packages('readr')
#install.packages('dplyr')

library('readr')
library('dplyr')

#Get the ONS postcode file
PC.Lookup <- read_csv("~/NSPL_MAY_2018_UK.csv")

#Create new postcode column without spaces
PC.Lookup$Postcode_ns <- gsub(" ","",PC.Lookup$pcd,fixed=TRUE)

Keep <- c("Postcode_ns", "ru11ind", "oseast1m", "osnrth1m", "ctry", "rgn", "laua", "pcon", "ttwa", "park", "lsoa11", "msoa11", "pfa", "imd")
Names <- c("Postcode_ns", "RuralIndicator", "Easting", "Northing", "CountryCode", "RegionCode", "LAUA", "PCON", "TTWA", "PARK", "LSOA11", "MSOA11", "PFA", "IMD")
PC.Lookup <- PC.Lookup[Keep]
names(PC.Lookup) <- Names



# The URL of the Land Registry data
ds <- "http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-2017.csv"
Names <- c('Transaction_unique_identifier', 'Price', 'Date_of_Transfer', 'Postcode', 'Property_Type', 'Old_New', 'Duration', 'PAON', 'SAON', 'Street', 'Locality', 'Town_City', 'District', 'County', 'SaleType', 'Record_Status_monthly_file_only')
Lreg2017  <- read_csv(ds, col_names=Names)

ds <- "http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-2018.csv"
Names <- c('Transaction_unique_identifier', 'Price', 'Date_of_Transfer', 'Postcode', 'Property_Type', 'Old_New', 'Duration', 'PAON', 'SAON', 'Street', 'Locality', 'Town_City', 'District', 'County', 'SaleType', 'Record_Status_monthly_file_only')
Lreg2018  <- read_csv(ds, col_names=Names)

Lreg <- rbind(Lreg2017, Lreg2018)

Lreg$Postcode_ns <- gsub(" ", "", Lreg$Postcode, fixed = TRUE)

#Remove the hour and minutes from the date variable and create a month variable
Lreg$Transfer_Date <- as.Date(as.POSIXct(Lreg$Date_of_Transfer,tz="", "%Y-%m-%d %H:%M"))
Lreg$Transfer_Month <- as.Date(paste((format(Lreg$Transfer_Date, "%Y-%m")),"-01",sep=""))
Lreg$Transfer_Year <- as.numeric(format(Lreg$Transfer_Date, "%Y"))

#I want just sales from in or after 2017 and the postcode needs to exist
Lreg <- Lreg[ which(Lreg$Transfer_Year>=2017), ]
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

PC.Lookup2 <- left_join(PC.Lookup, Lreg.Lookup, by="Postcode_ns")
