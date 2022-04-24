link<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
fn<-"HW2_Data.zip"
download.file(link,fn)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
Total <- aggregate(Emissions ~ year,NEI, sum)
barplot((Total$Emissions)/10^6,names.arg=Total$year,xlab="Year",ylab="PM2.5 Emissions (10^6 Tons)",main="Total PM2.5 Emissions From All US Sources")
library(ggplot2)



TotalBaltimore <- aggregate(Emissions ~ year, baltimoreNEI,sum)
barplot(TotalBaltimore$Emissions,names.arg=TotalBaltimore$year,xlab="Year",ylab="PM2.5 Emissions (Tons)",main="Total PM2.5 Emissions From all Baltimore City Sources")



ggplot(baltimoreNEI,aes(factor(year),Emissions,fill=type)) 
geom_bar(stat="identity") 
theme_bw() + guides(fill=FALSE)
facet_grid(.~type,scales = "free",space="free") 
labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)"))  
labs(title=expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 by Source Type"))



combustionRelated <- grepl("comb", SCC$SCC.Level.One, ignore.case=TRUE)
coalRelated <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE) 
coalCombustion <- (combustionRelated & coalRelated)
combustionSCC <- SCC[coalCombustion,]$SCC
combustionNEI <- NEI[NEI$SCC %in% combustionSCC,]

ggplot(combustionNEI,aes(factor(year),Emissions/10^5)) 
geom_bar(stat="identity",fill="grey",width=0.75) 
theme_bw() +  guides(fill=FALSE) 
labs(x="year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) +
labs(title=expression("PM"[2.5]*" Coal Combustion Source Emissions Across US from 1999-2008"))


vehicles <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
vehiclesSCC <- SCC[vehicles,]$SCC
vehiclesNEI <- NEI[NEI$SCC %in% vehiclesSCC,]

ggplot(baltimoreVehiclesNEI,aes(factor(year),Emissions)) 
geom_bar(stat="identity",fill="grey",width=0.75) 
theme_bw() +  guides(fill=FALSE) 
labs(x="year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) 
labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore from 1999-2008"))


vehiclesBaltimoreNEI <- vehiclesNEI[vehiclesNEI$fips=="24510",]
vehiclesBaltimoreNEI$city <- "Baltimore City"
vehiclesLANEI <- vehiclesNEI[vehiclesNEI$fips=="06037",]
vehiclesLANEI$city <- "Los Angeles County"
bothNEI <- rbind(vehiclesBaltimoreNEI,vehiclesLANEI)

ggplot(bothNEI, aes(x=factor(year), y=Emissions, fill=city)) 
geom_bar(aes(fill=year),stat="identity") 
facet_grid(scales="free", space="free", .~city) 
guides(fill=FALSE) + theme_bw() 
labs(x="year", y=expression("Total PM"[2.5]*" Emission (Kilo-Tons)"))
labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore & LA, 1999-2008"))
