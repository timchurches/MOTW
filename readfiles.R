# readfiles.R
# Tim Churches, 14 April 2013

require(reshape2)
require(ggplot2)
require(gridExtra)
library(scales)

# Read each CSV file from current directory, files are named motwYYYY.csv where YYYY is year of Census
for(f in list.files(pattern=glob2rx("motw????.csv"))) {
  a <- read.csv(f,header=T)
  # lowercase all methods of travel
  a$Method.of.Travel.to.Work <- tolower(a$Method.of.Travel.to.Work)  
  # Fix the unfortunate use of a comma in two sole modes of travel (elsewhere comma delimits multiple modes!)
  a$Method.of.Travel.to.Work <- sub("car, as driver","car as driver",a$Method.of.Travel.to.Work)
  a$Method.of.Travel.to.Work <- sub("car, as passenger","car as passenger",a$Method.of.Travel.to.Work)
  # Fix double commas in 1976 data
  a$Method.of.Travel.to.Work <- sub(",,", ",", a$Method.of.Travel.to.Work)
  # Make some strings canonical
  a$Method.of.Travel.to.Work <- sub("m.bike/m.scooter","motorbike/motor scooter",a$Method.of.Travel.to.Work,fixed=T)
  a$Method.of.Travel.to.Work <- sub("motorbike/scooter","motorbike/motor scooter",a$Method.of.Travel.to.Work,fixed=T)
  a$Method.of.Travel.to.Work <- sub("motor bike/motor scooter","motorbike/motor scooter",a$Method.of.Travel.to.Work,fixed=T)
  a$Method.of.Travel.to.Work <- sub("walked only","walked",a$Method.of.Travel.to.Work,fixed=T)
  a$Method.of.Travel.to.Work <- sub("bycycle","bicycle",a$Method.of.Travel.to.Work,fixed=T)
  # Fix label where a comma is missing
  a$Method.of.Travel.to.Work <- sub("motorbike/scooter bicycle","motorbike/motor scooter,bicycle",a$Method.of.Travel.to.Work,fixed=T)  
  a$Method.of.Travel.to.Work <- sub("motorbike/motor scooter bicycle","motorbike/motor scooter,bicycle",a$Method.of.Travel.to.Work,fixed=T)  
  assign(strsplit(f,".",fixed=T)[[1]][1],a)
  rm(a)
}

# Define a regex to trim leading and trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Clean up data frames from previous runs (logic depends on data frames named methodsYYYY not existing prior to running)
for (f in ls(pattern=glob2rx("methods????"))) rm(f)

# Process each Census year data frame
for (c in ls(pattern=glob2rx("motw????"))) {
  char.year <- substr(c,5,8) # Census year as character
  new.df.name <- paste("methods",char.year,sep="") # name for processed data frame
  census <- as.integer(char.year) # Census year as integer
  print(new.df.name) # tell us where we are up to
  df <- get(c)
  for (r in 1:nrow(df)) {
    original.row <- df[r,]
    # Split the travel modes in ech row on comma delimiter
    travel.modes <- unlist(strsplit(as.character(original.row$Method.of.Travel.to.Work),",",fixed=T))
    mode.cardinality <- length(travel.modes)
    mode.number <- 0
    # Now create a new duplicated row for each travel mode
    for (travel.mode in travel.modes) {
      mode.number <- mode.number + 1
      this.row <- df[r,]
      this.row[c("census", "travel.mode", "mode.cardinality", "mode.number")] <- c(census, trim(travel.mode), mode.cardinality, mode.number)
      if (!exists(new.df.name)) {
        assign(new.df.name,this.row)
      } else {
        assign(new.df.name,rbind(get(new.df.name),this.row))
      }
    }
  }
}

# Clean up
rm(df, original.row, this.row, c, census, char.year, f, mode.cardinality, mode.number, new.df.name, r, travel.mode, travel.modes)
rm(list=ls(pattern=glob2rx("motw????")))

try(rm("motw"),silent=T) # Logic depends on this object not existing when run
final.df.name <- "motw"
id.vars=c("Method.of.Travel.to.Work", "census", "travel.mode", "mode.cardinality", "mode.number")

for (f in ls(pattern=glob2rx("methods????"))) {
  melted.df <- melt(get(f),id.vars=id.vars,variable.name="region",value.name="count")
  if (!exists(final.df.name)) {
    assign(final.df.name, melted.df)
  } else {
    assign(final.df.name, rbind(get(final.df.name), melted.df))
  } 
} 

# Clean up
rm(melted.df, f, final.df.name, id.vars)
rm(list=ls(pattern=glob2rx("methods????")))

# Combine some travel modes
motw[motw$travel.mode %in% c("ferry","tram"),"travel.mode"] <- "ferry/tram"
motw[motw$travel.mode %in% c("car as driver","car as passenger","taxi","truck"),"travel.mode"] <- "car/taxi/truck"
motw[motw$travel.mode %in% c("motorbike","motorbike/motor scooter"),"travel.mode"] <- "motorcycle"
motw[motw$travel.mode == "not stated/could not be determined","travel.mode"] <- "not stated"

# Fix the cardinality of "all other 3 methods"
motw[motw$travel.mode == "all other 3 methods","mode.cardinality"] <- 3

motw$count <- gsub(",", "", motw$count,fixed=T)
motw$count <- as.integer(motw$count)
motw$region <- as.character(motw$region)

balance.nsw <- c("Hunter.NSW", "Illawarra.NSW", "Richmond.Tweed.NSW", "Mid.North.Coast.NSW", "Northern.NSW", "North.Western.NSW", "Central.West.NSW", "South.Eastern.NSW", "Murrumbidgee.NSW", "Murray.NSW", "Far.West.NSW", "Off.Shore.Areas...Migratory.NSW", "No.Usual.Address.NSW")
motw[motw$region %in% balance.nsw,"region"] <- "Balance of NSW"

balance.vic <- c("Barwon.VIC", "Western.District.VIC", "Central.Highlands.VIC", "Wimmera.VIC", "Mallee.VIC", "Loddon.VIC", "Goulburn.VIC", "Ovens.Murray.VIC", "East.Gippsland.VIC", "Gippsland.VIC", "Off.Shore.Areas...Migratory.VIC", "No.Usual.Address.VIC")
motw[motw$region %in% balance.vic,"region"] <- "Balance of Victoria"

balance.qld <- c("Gold.Coast.QLD", "Sunshine.Coast.QLD", "West.Moreton.QLD", "Wide.Bay.Burnett.QLD", "Darling.Downs.QLD", "South.West.QLD", "Fitzroy.QLD", "Central.West.QLD", "Mackay.QLD", "Northern.QLD", "Far.North.QLD", "North.West.QLD", "Off.Shore.Areas...Migratory.QLD", "No.Usual.Address.QLD")
motw[motw$region %in% balance.qld,"region"] <- "Balance of Queensland"

balance.sa <- c("Yorke.and.Lower.North.SA", "Murray.Lands.SA", "South.East.SA", "Eyre.SA", "Northern.SA", "Off.Shore.Areas...Migratory.SA", "No.Usual.Address.SA")
motw[motw$region %in% balance.sa,"region"] <- "Balance of SA"

balance.wa <- c("South.West.WA", "Lower.Great.Southern.WA", "Upper.Great.Southern.WA", "Midlands.WA", "South.Eastern.WA", "Central.WA", "Pilbara.WA", "Kimberley.WA", "Off.Shore.Areas...Migratory.WA", "No.Usual.Address.WA")
motw[motw$region %in% balance.wa,"region"] <- "Balance of WA"

balance.tas <- c("Southern.TAS", "Northern.TAS", "Mersey.Lyell.TAS", "Off.Shore.Areas...Migratory.TAS", "No.Usual.Address.TAS")
motw[motw$region %in% balance.tas,"region"] <- "Balance of Tasmania"

balance.nt <- c("NT...Bal","Northern-Territory...Bal", "Off.Shore.Areas...Migratory.NT", "No.Usual.Address.NT")
motw[motw$region %in% balance.nt,"region"] <- "Balance of NT"

balance.act <- c("ACT...Bal","Australian.Capital.Territory...Bal", "No.Usual.Address.ACT")
motw[motw$region %in% balance.act,"region"] <- "Balance of ACT"

other.aust <- c("Other.Territories.AUST","Off.Shore.Areas...Migratory.AUST","No.Usual.Address.AUST","Other.Territories.c.","Other.Territories")
motw[motw$region %in% other.aust,"region"] <- "Other - Australia"

canberra.parts <- c("Canberra.Statistical.District..ACT.Part.","Canberra..ACT.Part.")
motw[motw$region %in% canberra.parts,"region"] <- "Canberra"
motw[motw$region == "Australian.Capital.Territory","region"] <- "ACT"

motw$region <- sub("Balance.of.","Balance of ",motw$region,fixed=T)
motw$region <- sub("..SD.","",motw$region,fixed=T)
motw$region <- sub("Greater.","",motw$region,fixed=T)
motw$region <- sub("Rest.of.","Balance of ",motw$region,fixed=T)
motw$region <- sub("Tas.","Tasmania",motw$region,fixed=T)
motw$region <- sub("Vic.","Victoria",motw$region,fixed=T)
motw$region <- sub("Qld","Queensland",motw$region,fixed=T)
motw$region <- sub("QLD","Queensland",motw$region,fixed=T)
motw$region <- sub("South.Australia","SA",motw$region,fixed=T)
motw$region <- sub("Western.Australia","WA",motw$region,fixed=T)
motw$region <- sub("New.South.Wales","NSW",motw$region,fixed=T)
motw$region <- sub("Northern.Territory","NT",motw$region,fixed=T)
motw$region <- sub("Australian.Capital.Territory","ACT",motw$region,fixed=T)
motw$region <- sub("A.C.T.","ACT",motw$region,fixed=T)
motw$region <- sub("Migratory...Offshore...Shipping..","Other ",motw$region,fixed=T)
motw$region <- sub("No.Usual.Address..","Other ",motw$region,fixed=T)

motw[motw$region =="Not.Stated","region"] <- "Other - Australia"
motw[motw$region =="Canberra..ACT.Part.","region"] <- "Canberra"
motw[motw$region %in% c("Total", "Total.Australia"),"region"] <- "Australia"
motw[motw$region %in% c("Other.Territories.c.","Other OT."),"region"] <- "Other - Australia"
motw[motw$region =="NT...Bal","region"] <- "Balance of NT"
motw[motw$region =="Other Victoria.","region"] <- "Balance of Victoria"
motw[motw$region =="Other Queensland.","region"] <- "Balance of Queensland"
motw[motw$region =="Other SA.","region"] <- "Balance of SA"
motw[motw$region =="Other WA.","region"] <- "Balance of WA"
motw[motw$region =="Other Tasmania.","region"] <- "Balance of Tasmania"
motw[motw$region =="Other NT.","region"] <- "Balance of NT"
motw[motw$region =="Other ACT.","region"] <- "Balance of ACT"
motw[motw$region =="Other NSW.","region"] <- "Balance of NSW"

# Not sure whether Outer Adelaide should be with Adelaide or the rest of the state - it looks pretty rural, not metropolitan.
motw[motw$region =="Outer.Adelaide","region"] <- "Balance of SA"

# Clean up
rm(balance.act,balance.nsw,balance.nt,balance.qld,balance.sa,balance.tas,balance.vic,balance.wa,canberra.parts,other.aust)

unique(motw$region)

str(motw)

# Need to add summary rows for each state for 2006 and 2011 data
motw0611 <- motw[motw$census %in% c(2006,2011),]
motw0611[motw0611$region %in% c("Sydney","Balance of NSW"),"region"] <- "NSW"
motw0611[motw0611$region %in% c("Melbourne","Balance of Victoria"),"region"] <- "Victoria"
motw0611[motw0611$region %in% c("Brisbane","Balance of Queensland"),"region"] <- "Queensland"
motw0611[motw0611$region %in% c("Hobart","Balance of Tasmania"),"region"] <- "Tasmania"
motw0611[motw0611$region %in% c("Perth","Balance of WA"),"region"] <- "WA"
motw0611[motw0611$region %in% c("Adelaide","Balance of SA"),"region"] <- "SA"
motw0611[motw0611$region %in% c("Darwin","Balance of NT"),"region"] <- "NT"
motw0611[motw0611$region %in% c("Canberra","Balance of ACT"),"region"] <- "ACT"
motw <- rbind(motw,motw0611)
rm(motw0611)

region.order = c("Sydney","Balance of NSW","NSW","Melbourne","Balance of Victoria","Victoria","Brisbane","Balance of Queensland","Queensland","Hobart","Balance of Tasmania","Tasmania","Adelaide","Balance of SA","SA","Perth","Balance of WA","WA","Darwin","Balance of NT","NT","Canberra", "Balance of ACT","Other - Australia","Australia")
region.labels = c("Sydney","Rest of NSW","NSW","Melbourne","Rest of Vic.","Victoria","Brisbane","Rest of Qld","Queensland","Hobart","Rest of Tas.","Tasmania","Adelaide","Rest of SA","SA","Perth","Rest of WA","WA","Darwin","Rest of NT","NT","Canberra","Rest of ACT","Other - Australia","Australia")
# motw$region <- factor(motw$region,levels=region.order,labels=region.labels,ordered=T)
motw$region <- factor(motw$region,levels=region.order,ordered=T)

excluded.modes <-  c("not applicable","worked at home","did not go to work","not stated","total")
excluded.regions <- c("Other - Australia","Australia")

# Select and aggregate the numerators
motw.subset <- motw[motw$mode.cardinality==1 & !(motw$travel.mode %in% excluded.modes) & !(motw$region %in% excluded.regions),]
motw.num <- aggregate(formula=count ~ census + travel.mode + region, data=motw.subset, FUN=sum, na.rm=T)

# Now get the denominators
motw.denom <- aggregate(formula=count ~ region + census, data=motw.subset, FUN=sum, na.rm=T)
motw.denom$commuting.workforce <- motw.denom$count
motw.denom <- motw.denom[,c("region","census","commuting.workforce")]
motwp <- merge(motw.num,motw.denom,by=c("region","census"))
motwp$prop <- with(motwp, (count / commuting.workforce)*100)

# Add some attributes 
motwp[motwp$region %in% c("Sydney","Melbourne","Brisbane","Adelaide","Perth","Hobart","Canberra","Darwin"),"region.cat"] <- "capital"
motwp[motwp$region %in% c("NSW","Victoria","Queensland","SA","WA","Tasmania","ACT","NT"),"region.cat"] <- "whole"
motwp[motwp$region %in% c("Balance of NSW","Balance of Victoria","Balance of Queensland","Balance of SA","Balance of WA","Balance of Tasmania","Balance of ACT","Balance of NT"),"region.cat"] <- "balance"

motwp[motwp$region %in% c("Sydney","Balance of NSW","NSW"),"jurisdiction"] <- "NSW"
motwp[motwp$region %in% c("Melbourne","Balance of Victoria","Victoria"),"jurisdiction"] <- "Victoria"
motwp[motwp$region %in% c("Brisbane","Balance of Queensland","Queensland"),"jurisdiction"] <- "Queensland"
motwp[motwp$region %in% c("Hobart","Balance of Tasmania","Tasmania"),"jurisdiction"] <- "Tasmania"
motwp[motwp$region %in% c("Adelaide","Balance of SA","SA"),"jurisdiction"] <- "SA"
motwp[motwp$region %in% c("Perth","Balance of WA","WA"),"jurisdiction"] <- "WA"
motwp[motwp$region %in% c("Canberra","Balance of ACT","ACT"),"jurisdiction"] <- "ACT"
motwp[motwp$region %in% c("Darwin","Balance of NT","NT"),"jurisdiction"] <- "NT"

census.dates <- data.frame(census=c(1976,1981,1986,1991,1996,2001,2006,2011),census.date=c("1976-06-30", "1981-06-30", "1986-06-30", "1991-08-06", "1996-08-06", "2001-08-07", "2006-08-08", "2011-08-09"))

motwp$census <- factor(motwp$census,levels=c(1976,1981,1986,1991,1996,2001,2006,2011),ordered=T,labels=c("76","81","86","91","96","01","06","11"))

states.territories <- c("NSW","Victoria","Queensland","Tasmania","SA","WA","NT") # ACT deliberately missing
state.regions <- c("Sydney","Rest of NSW","Melbourne","Rest of Vic.","Brisbane","Rest of Qld","Hobart","Rest of Tas.","Adelaide","Rest of SA","Perth","Rest of WA")
state.capitals <- c("Sydney","Melbourne","Brisbane","Hobart","Adelaide","Perth")
state.rest <- c("Rest of NSW","Rest of Vic.","Rest of Qld","Rest of Tas.","Rest of SA","Rest of WA")
regions.list <- list(capitals=state.capitals,rest=state.rest)

selected.capitals <- c("Sydney","Melbourne","Brisbane","Hobart","Adelaide","Perth","Darwin","Canberra")
selected.balance <- c("Balance of NSW","Balance of Victoria","Balance of Queensland","Balance of Tasmania","Balance of SA","Balance of WA","Balance of NT")

plotit <- function(regions,region.type="capital") {
  b <- motwp
  b$region <- as.character(b$region)
  # Remove data for trains, ferries and trams in places that don't have them or use then (< 0.1% of mode share)
  b <- b[!(b$region %in% c("Hobart","Canberra","ACT","Darwin","NT") & b$travel.mode == "train"),] 
  b <- b[!(b$region %in% c("Perth","Canberra","ACT","Darwin","NT") & b$travel.mode == "ferry/tram"),] 
  # Show a different set of travel modes for capitals vs rest of jurisdiction to avoid displaying useless data.
  if (region.type == "capital") {
    b <- b[b$region %in% regions & b$travel.mode %in% c("bicycle","walked","bus","train","ferry/tram","motorcycle","car/taxi/truck"),]
    b$travel.mode <- factor(b$travel.mode,levels=c("bicycle","walked","bus","train","ferry/tram","motorcycle","car/taxi/truck"),labels=c("BIKE","WALK","BUS","TRAIN","FER/TRAM","M-BIKE","CAR"), ordered=T) 
    b$region <- factor(b$region,levels=regions,ordered=T)
  } else if (region.type == "balance") {
    b <- b[b$region %in% regions & b$travel.mode %in% c("bicycle","walked","bus","motorcycle","car/taxi/truck"),]
    b$travel.mode <- factor(b$travel.mode,levels=c("bicycle","walked","bus","motorcycle","car/taxi/truck"),labels=c("BIKE","WALK","BUS","M-BIKE","CAR"), ordered=T)
    b$region <- factor(b$region,levels=regions,ordered=T)
  } else {
    b <- b[b$region %in% regions & b$travel.mode %in% c("bicycle","walked","bus","motorcycle","car/taxi/truck"),]
    b$travel.mode <- factor(b$travel.mode,levels=c("bicycle","walked","bus","motorcycle","car/taxi/truck"),labels=c("BIKE","WALK","BUS","M-BIKE","CAR"), ordered=T)
    b$region <- factor(b$region,levels=regions,ordered=T)
  }
  p <- ggplot(data=b, aes(x=census, y=prop, group=travel.mode, colour=travel.mode)) 
  p <- p + geom_point() + geom_line() + facet_grid(travel.mode ~ region, scales="free_y", drop=F) 
  p <- p + labs(x=NULL, y=NULL) + theme(legend.position="none") + theme(axis.text.y = element_text(size = rel(0.8)))
  return(p)
}

plotit.modeshare <- function(region) {
  b <- motwp[motwp$region == region & motwp$census == "11" & motwp$travel.mode %in% c("bicycle","walked","bus","train","ferry/tram","motorcycle","car/taxi/truck"),]
  b$travel.mode <- factor(b$travel.mode,levels=rev(c("bicycle","walked","bus","train","ferry/tram","motorcycle","car/taxi/truck")),labels=rev(c("bicycle","walk","bus","train","ferry,tram","motorbike","car,truck")), ordered=T)
  b$region <- as.character(b$region)
  b$facet.heading <- "2011 mode share"
  p <- ggplot(data=b, aes(x=travel.mode, y=prop, fill=travel.mode)) + geom_bar( stat="identity") + coord_flip() 
  p <- p + theme(legend.position="none") + labs(x=NULL, y=NULL) + facet_grid(. ~ facet.heading, scales="free_y", drop=F)
  p <- p + scale_y_continuous(limits=c(0, 100)) 
  p <- p + geom_text(aes(x=travel.mode, y=prop+3, ymax=prop, label=paste(formatC(prop,digits=2,width=1,format="fg"),"%",sep=""),size=6,vjust=0.5,hjust=0))
  return(p)
}

plot.list.capitals <- lapply(selected.capitals, plotit,region.type="capital")
plot.list.balance <- lapply(selected.balance, plotit,region.type="balance")
plot.list.jurisdictions <- lapply(states.territories, plotit,region.type="states/territories")
plot.list.modeshare <- list(plotit.modeshare("NSW")) # Should be for all Oz!
args.list <- c(plot.list.capitals,plot.list.balance,plot.list.modeshare,plot.list.jurisdictions, list(nrow=3,ncol=(length(plot.list.capitals)+length(plot.list.balance)+length(plot.list.modeshare)+length(plot.list.jurisdictions)+2)/3,heights = unit(c(0.44,0.28,0.28),"null"), left="Percentage of commuters",sub="Census 1976 to 2011",main="Method of Travel to Work - Australia 1976 to 2011"))
do.call(grid.arrange,args.list)

plot.list.jurisdictions <- lapply(states.territories, plotit,capital=T)
args.list <- c(plot.list.jurisdictions, list(nrow=2,ncol=length(plot.list.jurisdictions)/2,heights = unit(c(0.6,0.4),"null"), left="Percentage of commuters",sub="Census 1976 to 2011",main="Method of Travel to Work - Australia 1976 to 2011"))
do.call(grid.arrange,args.list)

weather <- read.csv("Census_weather.csv",header=T)
weather$Date <- as.Date(weather$Date,format="%d/%m/%Y")
# ggplot(data=weather, aes(x=Date, y=rainfall)) + geom_point() + facet_grid(. ~ City, scales="free_y",space="free")


  