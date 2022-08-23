# Group project
# Due Aug,7th

# Require packages -------------
require(ggplot2) # visualization package
require(tidyr) # Clean data
require(dplyr) # data transformation  
require(readr) # use to eliminate '$' for price
require(formattable) # print nice looking table

# for Vis and maps -----
library("sf") # map
library("rnaturalearth") # map
library("rnaturalearthdata") # map
library("tools")
library('maps')
library('ggmap') 
library('plotly') # interactive plot

# use the classic dark-on-light theme for ggplot2 (theme_bw), which is more appropriate for maps
theme_set(theme_bw())


# Import data first -------------
denver_reviews <- read.csv("C:/Users/asus/Desktop/CU boulder/Course/Summer_22/MSBC5410_R/Group project - Airbnb/denver_reviews.csv")
denver_listings <- read.csv("C:/Users/asus/Desktop/CU boulder/Course/Summer_22/MSBC5410_R/Group project - Airbnb/denver_listings.csv", comment.char="#")
denver_calendar <- read.csv("C:/Users/asus/Desktop/CU boulder/Course/Summer_22/MSBC5410_R/Group project - Airbnb/denver_calendar.csv")

head(denver_reviews)
head(denver_calendar)
head(denver_listings)

# Q1) Where are the cheapest and most expensive neighborhoods to rent an Airbnb in Denver? -------------
# will only use Denver_listings.csv to analyze this problem now.
# Use the cost of the listing divided by the # of the accommodations to evaluate it's cheap or expensive

# Date cleaning ---------
listings <- read.csv("C:/Users/asus/Desktop/CU boulder/Course/Summer_22/MSBC5410_R/Group project - Airbnb/Denver_listing_GP_V02.csv")

# remove na (which means nothing provided in the cell)
listings <- na.omit(listings)

# Blank into N/A (replace blank string into 'N/A')
listings$neighborhood_overview <-replace(listings$neighborhood_overview, listings$neighborhood_overview=="","N/A")
listings$host_location <-replace(listings$host_location, listings$host_location=="","N/A")
listings$host_neighbourhood <-replace(listings$host_neighbourhood, listings$host_neighbourhood=="","N/A")
listings$neighbourhood <-replace(listings$neighbourhood, listings$neighbourhood=="","N/A")
listings$bathrooms_text <-replace(listings$bathrooms_text, listings$bathrooms_text=="","N/A")
listings$description <-replace(listings$description, listings$description=="","N/A")
listings$first_review <-replace(listings$first_review, listings$first_review=="","N/A")
listings$last_review <-replace(listings$last_review, listings$last_review=="","N/A")

# output
write.csv(listings,"C:/Users/asus/Desktop/CU boulder/Course/Summer_22/MSBC5410_R/Group project - Airbnb/Denver_listing_GP_V03.csv", row.names = FALSE)

# Group project data set denver_listing_GP_V03.csv
listing <- read.csv("C:/Users/asus/Desktop/CU boulder/Course/Summer_22/MSBC5410_R/Group project - Airbnb/Denver_listing_GP_V03.csv")

## Clean the price data - eliminate the '$' ----------
# and transfer they into integer/numeric values
# use readr packages; parse_number() to removes any non-numeric characters.
listing$price_num <- parse_number(listing$price)

## Price divided by accommodation, method 2 to think about Q1 ----------
class(listing$accommodates) # it's now integer and ready to do some calculation
listing$price_per_person <- round(listing$price_num/listing$accommodates,2)

## Count neighbor and create a table for ave price for each neighbor ----------
PricePerNeighbor <- aggregate(price_per_person ~ neighbourhood_cleansed, FUN = mean, data = listing)
neighbor_count <- count(listing,neighbourhood_cleansed,sort=TRUE)
nrow(listing[listing$neighbourhood_cleansed == ('Five Points'),])
priceneighbor <- aggregate(price_num ~ neighbourhood_cleansed, FUN = mean, data = listing)

## Merge neighbor count and price data  ----------
neighborPVT <- merge(x = neighbor_count, y = PricePerNeighbor, by = 'neighbourhood_cleansed')
neighborPVT <- merge (x = neighborPVT, y = priceneighbor, by = 'neighbourhood_cleansed' )

# latitude / longitude
latitude <- aggregate(latitude ~ neighbourhood_cleansed, FUN = mean, data = listing) 
longitude <-aggregate(longitude ~ neighbourhood_cleansed, FUN = mean, data = listing) 

## Merge latitude and longitude into the PV table as well ----------
neighborPVT <- merge (x = neighborPVT, y = latitude, by = 'neighbourhood_cleansed' )
neighborPVT <- merge (x = neighborPVT, y = longitude, by = 'neighbourhood_cleansed' )

# These can use to generate visualization

# rename variable
names(neighborPVT)[2] <- 'Count'

# Print out top and last expensive neighborhoods, price
neighborPVT <- neighborPVT[order(neighborPVT$Count,decreasing=TRUE),]
neighborPVT_price <- neighborPVT[order(neighborPVT$price_num,decreasing=TRUE),]
row.names(neighborPVT_price) <- NULL
formattable(head(neighborPVT_price,5)[,c(1,2,4)])
formattable(tail(neighborPVT_price,5)[,c(1,2,4)])

# Print out top and last expensive neighborhoods, price_per_person
neighborPVT_price_pp <- neighborPVT[order(neighborPVT$price_per_person,decreasing=FALSE),]
row.names(neighborPVT_price_pp) <- NULL
formattable(head(neighborPVT_price_pp,5)[,c(1,2,3)])
formattable(tail(neighborPVT_price_pp,5)[,c(1,2,3)])

## Visualization with map, use size to represent count and color to show the price ----------

# Histogram for price distribution  -----
ggplot(neighborPVT) + 
  geom_histogram(aes(price_num),binwidth = 10,fill = 'white',color = 'black')+
  labs(x = "Price",y= "Count",
              title = "Histograms for price per person",
              subtitle = "Airbnb listing in Denver,  average: $43")+
  geom_vline(aes(xintercept = mean(price_num)),color = 'red',size = 1)


## Visualization with Google maps -----  
# The package rnaturalearth provides a map of countries of the entire world. Use ne_countries to 
# pull country data and choose the scale (rnaturalearthhires is necessary for scale = "large"). 
# The function can return sp classes (default) or directly sf classes, as defined in the argument 
# returnclass
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# Mark the point on the map, while I haven't add any detail to the map yet. 
# Use latitude and longitude to find Denver
ggplot(data = world) +
  geom_sf() +
  geom_point(data = neighborPVT, aes(x = longitude, y = latitude,size = Count), 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-105.2, -104.7), ylim = c(39.55, 39.85), expand = FALSE)


# Add counties into the vis
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("colorado", counties$ID))
counties$area <- as.numeric(st_area(counties))
head(counties)

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = counties, fill = NA, color = gray(.5))+
  geom_point(data = neighborPVT, aes(x = longitude, y = latitude,size = Count), 
             shape = 21, fill = "darkred") +
  coord_sf(xlim = c(-105.15, -104.7), ylim = c(39.6, 39.85), expand = FALSE)

# Overlay with google maps
# Need to go to google maps API to create an account and get the key
# Register
register_google(key = "AIzaSyC1ZtSLw44LG02uajrWUC2E9jrntO7Srto") 

# Get map with longitutde and latitude, choose scale with 'zoom' argument
al1 = get_map(location = c(lon = -104.95, lat = 39.72),
              zoom =11,
              maptype = 'roadmap')
al1MAP = ggmap(al1) # make it into a ggplot2 object
al1MAP

# Overlap with point and adjust the style of the points
al1MAP+
  geom_point(data = neighborPVT, aes(x = longitude, 
                                     y = latitude,
                                     size = Count,
                                     fill = price_per_person), shape = 21)+
  scale_fill_continuous(low = '#fff5f0', high = '#ef3b2c')+scale_size(range=c(0,12))


# apply highlight label
HL_lot_num <- c('Cherry Creek','Civic Center','Cory - Merrill','Overland','Country Club')

Highlight <- neighborPVT %>% 
  filter(neighbourhood_cleansed %in% HL_lot_num)

HL_lot_num1 <- c('Montbello','Southmoor Park','Valverde','Marston','College View - South Platte')

Highlight1 <- neighborPVT %>% 
  filter(neighbourhood_cleansed %in% HL_lot_num1)

al1MAP+
  geom_point(data = neighborPVT, aes(x = longitude, 
                                     y = latitude,
                                     size = Count,
                                     fill = price_num), shape = 21)+
  geom_label(data = Highlight,  
             aes(x = longitude,y = latitude,label = neighbourhood_cleansed),
             fontface = "bold",
             vjust = 1.4,
             fill = '#33a02c',
             color = 'white',alpha=.7)+ 
  geom_label(data = Highlight1,
             aes(x = longitude,y = latitude,label = neighbourhood_cleansed),
             fontface = "bold",
             vjust = 1.4,fill = '#1f78b4',color = 'white',alpha=.7)+
  scale_fill_continuous(low = '#fff5f0', high = '#ef3b2c')+scale_size(range=c(0,12))

# apply highlight label - price/acc ------
HL_lot_num <- c('Indian Creek','East Colfax','Overland','Country Club','Cherry Creek')

Highlight <- neighborPVT %>% 
  filter(neighbourhood_cleansed %in% HL_lot_num)

HL_lot_num1 <- c('Harvey Park South','Mar Lee','Barnum','Valverde','Marston')

Highlight1 <- neighborPVT %>% 
  filter(neighbourhood_cleansed %in% HL_lot_num1)

al1MAP+
  geom_point(data = neighborPVT, aes(x = longitude, 
                                     y = latitude,
                                     size = Count,
                                     fill = price_per_person), shape = 21)+
  geom_label(data = Highlight,  
             aes(x = longitude,y = latitude,label = neighbourhood_cleansed),
             fontface = "bold",
             vjust = 1.4,
             fill = '#33a02c',
             color = 'white',alpha=.7)+ 
  geom_label(data = Highlight1,
             aes(x = longitude,y = latitude,label = neighbourhood_cleansed),
             fontface = "bold",
             vjust = 1.4,fill = '#1f78b4',color = 'white',alpha=.7)+
  scale_fill_continuous(low = '#fff5f0', high = '#ef3b2c')+scale_size(range=c(0,12))


# The color variation in downtown denver is not clear ---------
# The reason is that there is a point (Indian Creek) with ave price ~ $120 which is way higher than others. And makes the color scale starts from $25 to $120.
# While the price is exponential disttribution and average price is only ~ $43, means most of the neighbor price range are under $50 or $60
# Therefore, there is only a bit difference of color between points in downtown.
# I think we can see a more obvious result once we filter the outlier (ex: Indian Creek) or only do the vis for downtown denver  

# Visualization for those located in downtown Denver ---------
# subset 
neighbor_dt <- neighborPVT[neighborPVT$longitude < -104.9,]
neighbor_dt <- neighbor_dt[neighbor_dt$latitude > 39.7000,]
nrow(neighbor_dt)

# adjust zooming 
al2 = get_map(location = c(lon = -104.975, lat = 39.75),
              zoom =12,
              maptype = 'roadmap')

al2MAP = ggmap(al2)

# Price per person, in downtown denver
al2MAP+
  geom_point(data = neighbor_dt, aes(x = longitude, 
                                     y = latitude,
                                     size = Count,
                                     fill = price_per_person), shape = 21,alpha = 0.7)+
  scale_fill_continuous(low = '#fff5f0', high = '#ef3b2c')+scale_size(range=c(0,18))


# Price, Downtown Denver  
al2MAP+
  geom_point(data = neighbor_dt, aes(x = longitude, 
                                     y = latitude,
                                     size = Count,
                                     fill = price_num), shape = 21,alpha = 0.7)+
  scale_fill_continuous(low = '#fff5f0', high = '#ef3b2c')+scale_size(range=c(0,18))

# Highlight
HL_lot_num <- c('Cherry Creek','Civic Center','Country Club','Hilltop','Belcaro')

Highlight <- neighbor_dt %>% 
  filter(neighbourhood_cleansed %in% HL_lot_num)

HL_lot_num1 <- c('Valverde','Barnum West','Globeville','Clayton','Hale')

Highlight1 <- neighbor_dt %>% 
  filter(neighbourhood_cleansed %in% HL_lot_num1)

# Highlight expensive and cheap 
# point
al2MAP+
  geom_point(data = neighbor_dt, aes(x = longitude, 
                                     y = latitude,
                                     size = Count,
                                     fill = price_num), shape = 21,alpha = 0.7)+
  geom_point(data = Highlight,aes(x = longitude,y = latitude),size = 4,color = 'black',shape = 4,stroke = 1)+ 
  scale_fill_continuous(low = '#fff5f0', high = '#ef3b2c')+scale_size(range=c(0,18))
# label
al2MAP+
  geom_point(data = neighbor_dt, aes(x = longitude, 
                                     y = latitude,
                                     size = Count,
                                     fill = price_num), shape = 21,alpha = 0.7)+
  geom_label(data = Highlight,
             aes(x = longitude,y = latitude,label = neighbourhood_cleansed),
             size = 3,fontface = "bold",
             vjust = 1.4,fill = '#33a02c',color = 'white')+ 
  geom_label(data = Highlight1,
             aes(x = longitude,y = latitude,label = neighbourhood_cleansed),
             size = 3,fontface = "bold",
             vjust = 1.4,fill = '#1f78b4',color = 'white')+
  scale_fill_continuous(low = '#fff5f0', high = '#ef3b2c')+scale_size(range=c(0,18))

# plotly - interactive plot
# ggplotly(interactive_plot)

  
## Supplement coding -----------

# # use for loop to create a top 10 list of listting #
# top10 <- listing[listing$neighbourhood_cleansed == ('Highland'),]
# for (i in 3:11){
#   top10 <- rbind(top10,listing[listing$neighbourhood_cleansed == neighbor_count$neighbourhood_cleansed[i],])
# }
# num <- 0
# 
# # Check #
# # for (i in 2:11){
# #   num <- num + neighbor_count$n[i]
# # }
# # num
# 
# head(neighbor_count,11)
# 
# # top10 <- listing[listing$neighbourhood_cleansed == ('Highland'),]
# # top10 <- rbind(top10,listing[listing$neighbourhood_cleansed == ('Five Points'),])
# # top10 <- neighbor_count[2:11,'neighbourhood_cleansed']
# AvepriceTop10 <- aggregate(price_per_person ~ neighbourhood_cleansed, FUN = mean, data = top10)
# sort(AvepriceTop10)
# # PricePerNeighbor[,]
# 
# # top10
# # 
# # 
# # mean(listing$price_per_person)
# ggplot(top10,aes(neighbourhood_cleansed,price_per_person))+
#   geom_boxplot()+
#   coord_cartesian(ylim=c(0,150))+theme(axis.text.x = element_text(angle = 45, hjust = 1) ) 
# 
# ggplot(top10,aes(price_per_person,fill=neighbourhood_cleansed))+
#   geom_histogram(position = 'identity',alpha = 0.7)+ coord_cartesian(xlim=c(0,150))
# 
# ggplot(top10,aes(price_per_person))+
#   geom_histogram(position = 'identity',alpha = 0.7,fill = 'white',color = 'Black')+ facet_wrap(vars(neighbourhood_cleansed))+ coord_cartesian(xlim=c(0,150))
# 
# # Descriptive statistics
# mean(top10$price_per_person)
# sd(top10$price_per_person)
# 
# # order listing by neighbor count
# newListing <- listing[order(neighbor_count$neighbourhood_cleansed),]

# Extra code to check the information contain in the data ----------------------
# 
# # Check how many listing are included in the dataset  ----------------------
# nrow(denver_listings); # has 4602 listing in total 
# n_distinct(denver_listings$id) # count unique number of listing, result = 4507
# length(unique(denver_listings$id)) # Could also use length + unique to count distinct elements
# # means there are some listings repeated.
# 
# # Replace the neighbourhood with blank into 'unknown' ----------
# listing$neighbourhood_cleansed <-replace(listing$neighbourhood_cleansed, listing$neighbourhood_cleansed=="","unknown")

