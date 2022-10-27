# DA certification
# Kevin Chu

# The management would like you to answer the following:
  # ??? Are there products that do not sell as well in some locations?
  # ??? Are there any other patterns over time in each region that you can find in the data?

# Load library
library(tidyverse)
library(formattable)
library(GGally)
library(patchwork)
library(ggforce)
library(lubridate) # use for timee data set.
library(formattable)
library(gt)

# Import data
office <- read.csv("C:/Users/asus/Desktop/CU boulder/Course/Fall 22/Datacamp study guide/office_supplies.csv")
# Column name Details
# Order ID Character:   Unique identifier for the individual order.
# Order Date Character: Date of the order, in format YYYY-MM-DD.
# Ship Mode Character:  The method used to send out the order.
# Region Character:     The region the order was sent from.
# Product ID Character: Unique identifier of the product ordered.
# Category Character:   Category of the product, one of 'Office Supplies','Furniture', or 'Technology'.
# Sub-Category Char:    Subcategory of the product (e.g. Binders, Paper,etc.)
# Product Name Char:    The name of the product.
# Sales Numeric:        Total value of the products sold in the order.
# Quantity Numeric:     Quantity of the products in the order.
# Discount Numeric:     Discount of the order in decimal form. (e.g. 0.30indicates the order has a 30% discount, etc.)
# Profit Numeric:       Profit of the order.

# Check if there is any Na value in there
colSums(is.na(office))

# Dummy int flag for ship mode
office$shipmode_flag <- replace(office$Ship.Mode,office$Ship.Mode =='First Class',1)
office$shipmode_flag <- replace(office$shipmode_flag,office$shipmode_flag =='Second Class',2)
office$shipmode_flag <- replace(office$shipmode_flag,office$shipmode_flag =='Standard Class',3)
office$shipmode_flag <- replace(office$shipmode_flag,office$shipmode_flag =='Same Day',4)
office$shipmode_flag <- as.numeric(office$shipmode_flag)



## Note that profit has 1993 rows are Na

# ??? Are there products that do not sell as well in some locations?
# Check the sales vs region
Sales_region <- office %>% 
                  group_by(Region) %>% 
                    summarise(total_sales_region = sum(Sales)) %>%
                      arrange(desc(total_sales_region))
Sales_region %>% ggplot(aes(reorder(Region,total_sales_region),total_sales_region))+
  geom_col() + 
  xlab("Region")

# Check unique items for those 3 columns
unique(office$Sub.Category) %>% length()
unique(office$Product.ID) %>% length()
unique(office$Product.Name) %>% length()

# Descriptive summary
des <- office %>% group_by(Region, Category) %>% 
  summarise(Total_sold = sum(Quantity), Total_sales = sum(Sales)) %>%
  arrange(Category,desc(Total_sold))

des <- des %>% relocate(Region,.after = Category)

formattable(des)
des %>% gt(groupname_col = "Category") %>%
tab_header(
  title = md("Descriptive Summary for Sales and Quantity sold"),
  subtitle = md(" By region and Category")
)

# Use categories first
Sales_cate_region <- office %>% 
  group_by(Region,Category) %>% 
  summarise(Quantity = sum(Quantity)) %>% 
  arrange(Category,desc(Quantity))
View(Sales_cate_region)

Sales_cate_region %>% ggplot(aes(reorder(Region,Quantity),Quantity)) + 
  geom_col(position = 'dodge',fill = 'grey', color = 'black') +
  facet_wrap(vars(Category))+
  theme_bw()+
  xlab('Region')+
  ggtitle(label = 'Quantity for each category in different region')

# follow by sub.category 
Sales_subcate_region <- office %>% group_by(Region,Sub.Category) %>% summarise(Quantity = sum(Quantity),) %>% arrange(Sub.Category,desc(Quantity))
Sales_subcate_region %>% ggplot(aes(reorder(Region,Quantity),Quantity)) + 
  geom_col(position = 'dodge',fill = 'grey', color = 'black') +
  facet_wrap(vars(reorder(Sub.Category,desc(Quantity))))+
  theme_bw()+
  xlab('Region')+
  ggtitle(label = 'Quantity for each Sub-category in different region')

# Top 10 number sold product 
product <- office %>% group_by(Product.ID,Product.Name) %>% summarise(count = sum(Quantity)) %>% arrange(desc(count)) %>% head(12)
id <- product$Product.ID[1:12] 
product_top12 <- office %>% filter(Product.ID %in% id) %>% arrange(Product.ID,Region)
product_top12_summary <- product_top12 %>% group_by(Region,Category,Sub.Category,Product.ID,Product.Name) %>% summarise(sold_num = sum(Quantity)) %>% arrange(Product.ID,desc(sold_num))

product_top12_summary %>% ggplot(aes(reorder(Region,sold_num),sold_num)) + 
  geom_col(position = 'dodge',fill = 'grey', color = 'black') +
  facet_wrap(vars(Category,Product.ID))+
  theme_bw()+
  xlab('Region')+
  ggtitle(label = 'Quantity for top 12 quantity sold product in different region')


# ??? Are there any other patterns over time in each region that you can find in the data?
# Order date by sales
Date_Sale <- office %>% select(-c(Order.ID,Product.Name))
# Convert date time to "%Y-%m" and Quarter
Date_Sale <- Date_Sale %>% mutate(month = month(Order.Date),Date = as.Date(Order.Date,format = "%Y-%m"))
Date_Sale$Date <- format(as.Date(Date_Sale$Order.Date), format = "%Y-%m")
Date_Sale$Quarter <- paste0(year(Date_Sale$Order.Date),         
       "/0",
       quarter(Date_Sale$Order.Date))

# Create quarter sales trend line
Quarterly_sales_region <- Date_Sale %>% group_by(Quarter,Region) %>% summarise(Quarterly_sales = sum(Sales))
Quarterly_sales_total <- Date_Sale %>% group_by(Quarter,) %>% summarise(Quarterly_sales = sum(Sales))

ggplot() + 
      geom_line(data = Quarterly_sales_region,aes(Quarter,Quarterly_sales,color = Region,group = Region),size = 1) + 
      geom_line(data = Quarterly_sales_total,aes(Quarter,Quarterly_sales,group = 1),size = 1)+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      annotate('text',x=14,y=285000,label = 'Total quarter sales')+
      ggtitle(label = 'Quaterly Sales')


# Profit vs discount
coor <- Date_Sale %>% select(Sales,month,Profit,Discount,shipmode_flag,Ship.Mode,Region) %>% na.omit()

# profit and discount seems have relation : -0.228720384
coor %>% ggplot(aes(as.factor(month),Discount)) + geom_count()+scale_size(range=c(1,10))+
  xlab('Month')+
  theme_bw()+
  ggtitle(label = 'Discount vs Month')

# Discount and profit boxplot
coor$Discount <- as.factor(coor$Discount)
coor %>% ggplot(aes(Profit,Discount)) + 
  geom_boxplot(aes(group = Discount))+ 
  coord_cartesian(xlim = c(-500,500))+
  theme_bw()+
  ggtitle(label = 'Discount vs Profit')

# relation btw sales and profit
coor %>% ggplot(aes(Sales,Profit)) + geom_point() + 
  coord_cartesian(xlim = c(0,5000),ylim = c(-4000,4000))

# Region and profit
coor %>% ggplot(aes(Profit)) + 
  geom_histogram(binwidth = 25, color = 'black', fill = 'grey') + 
  coord_cartesian(xlim=c(-250,250)) +
  theme_bw()+
  facet_wrap(vars(Region))+
  ggtitle(label = 'Region vs Profit')

coor %>% ggplot(aes(Sales)) + 
  geom_histogram(binwidth = 25, color = 'black', fill = 'grey') + 
  coord_cartesian(xlim=c(0,500)) +
  theme_bw()+
  facet_wrap(vars(Region))+
  ggtitle(label = 'Region vs Sales')


# shipment
ship.labs <- c("First Class", "Second Class", "Standard Class",'Same Day')
names(ship.labs) <- c("1", "2", "3",'4')
coor %>% ggplot(aes(Profit)) + geom_histogram(binwidth = 10,color = 'black', fill = 'grey') + 
  facet_wrap(vars(shipmode_flag),labeller = labeller(shipmode_flag = ship.labs))+
  coord_cartesian(xlim = c(-100,100))+
  theme_bw()+
  ggtitle("Ship.Mode vs Profit")

# Recommendation:
# ??? Adjust product number according to the analysis, store more product on those with higher demands (Especially for West and East area)
# ??? Prepare more product in Q4 in case any of them sold out
# ??? Control the discount value for each product, limit them within 20% off
# ??? Consider to adjust shipment fee for different method. Also, can negotiate with shipment companies to reduce the cost of standard shipment.
# ??? Data completeness of profit values would increase the accuracy of the results
