# Preparation

## Load Libraries and set up work space

### We're loading library packages in R to access additional functions and capabilities.
```{r setup, include=FALSE}
library(tidyverse)  # For EDA
setwd("C:/Users/gabri/Downloads/Proyectos R/Car_price")



# Load Data

## Currently loading data alongside library packages in R for comprehensive analysis.

cars <- read.csv("car_prices.csv")

# Exploratory Data Analysis

## Explore the Data Structure


head(cars)  #### Displaying the first 6 observations

dim(cars) #### Quick view of data structure



str(cars) #### We're going to convert all columns in 'character' format to a more user-friendly format (factor) for data analysis.


# Transform all character columns to factor columns
cars <- cars %>%
  mutate(across(where(is.character), as.factor))

# Great, now we will remove the date column since we won't be using it in this analysis.
cars <- cars %>%
  select(-SaleDate)


# Let's see the summary of our data:
summary(cars)
Handle Missing Data
We observe several missing values within our columns. 

# Let's try to locate them more accurately.
na_counts <- colSums(is.na(cars))
NA_Count <- data.frame(NA_Count = na_counts)
print(NA_Count)


#Let's delve deeper into the missing values to try to understand them better.
cars %>%
  filter(is.na(condition)) %>% # quick view
  head()

cars <- cars %>%
  filter(!is.na(condition)) # cleaning

colSums(is.na(cars)) # result

cars %>%
  filter(is.na(odometer)) %>% # quick view of null values of odometer variable
  head()

cars %>%
  filter(is.na(mmr)) %>% # quick view of null values of mmr variable
  head()

cars %>%
  filter(is.na(sellingprice)) %>% # quick view of null values of sellingprice variable
  head()


# Neither do we care to know these thoroughly, so let's remove them:
cars <- cars %>%
  filter(!is.na(odometer) & !is.na(mmr) & !is.na(sellingprice))

colSums(is.na(cars))
Variable Exploration


# First, let's observe the unique values of our variables previously converted to factor format.
unique(cars$make)

# At first glance, we can already observe a comparison problem in the Make variable since, if we pay attention, almost all brands are repeated due to being written in uppercase and lowercase. Let's try to fix this.

cars$make <- tolower(cars$make)

unique(cars$make)

# Transformation of variable
cars$make <- gsub("mercedes-b", "mercedes-benz", cars$make)
cars$make <- gsub("mercedes", "mercedes-benz", cars$make)
cars$make <- gsub("mercedes-benz-benzenzenz", "mercedes-benz", cars$make)
cars$make <- gsub("mercedes-benz-benzenz-benzenz", "mercedes-benz", cars$make)
cars$make <- gsub("mercedes-benz-benzenz", "mercedes-benz", cars$make)
cars$make <- gsub("mercedes-benz-benz", "mercedes-benz", cars$make)
cars$make <- gsub("ford tk", "ford truck", cars$make)
cars$make <- gsub("vw", "volkswagen", cars$make)
cars <- cars %>%
  filter(make != "")

unique(cars$make)


#Now let's explore other variables:
cars %>%
  group_by(make, model) %>%
  count()
cars$model <- tolower(cars$model)
cars %>%
  group_by(make, model) %>%
  count()

head(sort(unique(cars$trim)))

cars %>%
  group_by(model, trim) %>%
  count()

unique(cars$body)

cars %>%
  group_by(body) %>%
  count()

cars$body <- tolower(cars$body)

cars %>%
  group_by(body) %>%
  count()

unique(cars$transmission)

unique(cars$state)

unique(cars$condition)

unique(cars$color)
cars$color <- gsub("—", "", cars$color) # replacing observation 
cars <- cars %>%
  filter(color != "")

unique(cars$interior)
cars$color <- gsub("—", "", cars$interior) # replacing observation

unique(cars$interior)

#Eliminate some null values
cars$seller <- gsub("1360250 alberta ltd.", "1360250 alberta ltd", cars$seller)
cars$seller <- gsub("a-l financial corp", "a-l financial corporation", cars$seller)
cars$seller <- gsub("ahfc/honda lease trust/hvt  inc.", "ahfc/honda lease trust/hvt inc.", cars$seller)

cars$seller <- gsub("auto integrity  llc", "auto integrity llc", cars$seller)

cars$seller <- gsub("nissan infiniti lt", "nissan-infiniti lt", cars$seller)
unique_val_cars_seller <- sort(unique(cars$seller))


cars %>%
  group_by(seller, color) %>% # Quick view of sellers and colors
  count() %>%
  head(10)


# Exploratory Data Analysis

## Variable Analysis


# Histogram of odometer distribution
ggplot(cars, aes(odometer))+
  geom_histogram(bins = 30, fill = "#00A6FB")+
  labs(title ="Odometer Distribution")


# Histogram of years distribution
ggplot(cars, aes(year))+
  xlim(1990, 2015)+
  geom_histogram(bins = 25, fill = "#00A6FB") +
  labs(title = "Years Distribution")


# Histogram of sales distribution
ggplot(cars, aes(sellingprice))+
  geom_histogram(bins = 80, fill = "#00A6FB")+
  xlim(0,100000)+
  labs(title = "Sales Distribution")


# Histogram of condition distribution
ggplot(cars, aes(condition))+
  geom_histogram(bins = 30, fill = "#00A6FB")+
  labs(title = "Condition Distribution")


# Fixing the problem in the condition variable
cars$condition <- gsub("^(\\d)(\\d)", "\\1.\\2", cars$condition)
cars$condition <- as.numeric(cars$condition)

# Scatter plot of selling price by condition
ggplot(cars, aes(condition, sellingprice))+
  geom_point(color = "#00A6FB")+
  labs(title = "Scatter Plot of Selling Price by Condition",
       x = "Condition",
       y = "Selling Price")


# Identifying the top-selling sellers
top_5_best_sellers <- cars %>%
  group_by(seller) %>%
  summarize(selling_price = round(sum(sellingprice)/1000000)) %>%
  arrange(desc(selling_price)) %>%
  head(5)

# Bar plot of top 5 best sellers
colors <- c("#00A6FB", "#0582CA", "#006494", "#003554", "#051923" )
ggplot(top_5_best_sellers, aes(reorder(seller, selling_price), selling_price, fill = seller))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = colors[order(top_5_best_sellers$seller)]) +
  geom_text(aes(label = selling_price), hjust = -0.15) +
  ylim(0, 550)+
  labs(title = "Top 5 Best Sellers", y = "Total Sales(in million)", x = "")


# Identifying the top-selling car brands
top_5_best_make <- cars %>%
  group_by(make) %>%
  summarize(selling_price = round(sum(sellingprice)/1000000)) %>%
  arrange(desc(selling_price)) %>%
  head(5)

# Bar plot of top 5 car brands by total sales revenue
ggplot(top_5_best_make, aes(reorder(make, selling_price), selling_price, fill = make))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = colors[order(top_5_best_make$make)]) +
  geom_text(aes(label = scales::dollar(selling_price)), hjust = -0.25) +
  ylim(0,1500) +
  labs(title = "Top 5 Car Brands by Total Sales Revenue", y = "Total Sales Revenue (in million)", x = "")


# Sales trends by brands between 2000 - 2015
sales_by_years_brand <- cars %>%
  filter(make %in% c("ford", "chevrolet", "nissan", "toyota", "bmw")) %>%
  group_by(year, make) %>%
  summarize(sales = sum(sellingprice)/1000000) 

ggplot(sales_by_years_brand, aes(year, sales, color = make))+
  geom_line(stat = "identity") +
  xlim(2000, 2015)+
  labs(title = "Sales by Brands Between 2000 - 2015", x = "", y = "Sales (in million)")


# Top 5 car brands by number of sales
top5_Sales <- cars %>%
  group_by(make) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(5)

ggplot(top5_Sales, aes(reorder(make, n), n, fill = make))+
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = colors[order(top5_Sales$make)]) +
  geom_text(aes(label = n), hjust = -0.25) +
  ylim(0,100000)+
  labs(title = "Top 5 Car Brands by Number of Sales", y = "Number of Sales", x = "")


# Top 5 most profitable models for the top 5 brands
Top5_sales_by_brand <- cars %>%
  filter(make %in% c("ford", "chevrolet", "nissan", "toyota", "bmw")) %>%
  group_by(make, model) %>%
  summarize(sales = round(sum(sellingprice)/1000000)) %>%
  arrange(make, desc(sales)) %>%
  group_by(make) %>%
  top_n(5) 

ggplot(Top5_sales_by_brand, aes(x = reorder(model, -sales), y = sales, fill = sales))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  facet_grid(~make, scales = "free")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top 5 Most Profitable Models for the Top 5 Brands", x = "", y = "Selling Models (in million)")


# Top 5 best selling models by top 5 brands
Top5_sales_by_make <- cars %>%
  filter(make %in% c("ford", "chevrolet", "nissan", "toyota", "bmw")) %>%
  group_by(make, model) %>%
  count() %>%
  arrange(make, desc(n)) %>%
  group_by(make) %>%
  top_n(5) 

ggplot(Top5_sales_by_make, aes(x = reorder(model, -n), y = n, fill = n))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  facet_grid(~make, scales = "free")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top 5 Best Selling Models by Top 5 Brands", x = "", y = "Selling Models")



# Average selling price of top-selling models in each of the top 5 brands
mean_sell_price <- cars %>%
  filter(model %in% c("3 series", "impala", "f-150", "altima", "camry")) %>%
  group_by(model) %>%
  summarize(mean_price = mean(sellingprice)) %>%
  arrange(desc(mean_price))

print(mean_sell_price)


# Top-selling models by trim for each brand within the top 5
top_models <- cars %>%
  filter(model %in% c("3 series", "impala", "f-150", "altima", "camry")) %>%
  group_by(model, trim) %>%
  summarize(sales = round(sum(sellingprice)/1000000, 3)) %>%
  top_n(3) %>%
  arrange(model, desc(sales))

top_models_impala <- top_models %>%
  filter(model == "impala")

top_models_f150 <- top_models %>%
  filter(model == "f-150")

top_models_altima <- top_models %>%
  filter(model == "altima")

top_models_camry <- top_models %>%
  filter(model == "camry")

top_models_3series <- top_models %>%
  filter(model == "3 series")


# Visualization of top-selling models by trim for each brand within the top 5
ggplot(top_models_f150, aes(reorder(trim, -sales), sales, fill = sales))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  geom_text(aes(label = scales::dollar(sales)), vjust = -1)+
  ylim(0,125)+
  labs(title = "Top Selling Car of Ford", x = "F - 150", y = "Sales (in million)")


# Visualization of top-selling models by trim for each brand within the top 5
ggplot(top_models_impala, aes(reorder(trim, -sales), sales, fill = sales))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  geom_text(aes(label = scales::dollar(sales)), vjust = -1)+
  ylim(0,20)+
  labs(title = "Top Selling Car of Chevrolet", x = "Impala", y = "Sales (in million)")


# Visualization of top-selling models by trim for each brand within the top 5
ggplot(top_models_altima, aes(reorder(trim, -sales), sales, fill = sales))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  geom_text(aes(label = scales::dollar(sales)), vjust = -1)+
  ylim(0,140)+
  labs(title = "Top Selling Car of Nissan", x = "Altima", y = "Sales (in million)")


# Visualization of top-selling models by trim for each brand within the top 5
ggplot(top_models_camry, aes(reorder(trim, -sales), sales, fill = sales))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  geom_text(aes(label = scales::dollar(sales)), vjust = -1)+
  ylim(0,70)+
  labs(title = "Top Selling Car of Toyota", x = "Camry", y = "Sales (in million)")


# Visualization of top-selling models by trim for each brand within the top 5
ggplot(top_models_3series, aes(reorder(trim, -sales), sales, fill = sales))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  geom_text(aes(label = scales::dollar(sales)), vjust = -1)+
  ylim(0,70)+
  labs(title = "Top Selling Car of BMW", x = "3-series", y = "Sales (in million)")


# Popularity of colors in vehicles
Top5_colors <- cars %>%
  filter(color %in% c("black", "gray", "beige", "tan", "brown")) %>%
  mutate(color = factor(color, levels = c("black", "gray", "beige", "tan", "brown"))) %>%
  group_by(color) %>%
  count()

# Manual color fill
coincidencia <- c("black"="black", "gray"="gray", "beige"="beige", "tan"="tan", "brown"="brown")# colors

# Bar plot of color popularity
ggplot(Top5_colors, aes(x = reorder(color, -n), y = n, fill = color)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = coincidencia) +
  geom_text(aes(label =n), vjust = -1)+
  ylim(0,250000)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Popularity of Colors in Vehicle", x = "", y = "Number of Solds") # See the final plot

# Price comparison between selling price and market value
Price_comparison <- cars %>%
  mutate(Price_Differential = case_when(sellingprice > mmr ~ "overvalued",
                                        sellingprice < mmr ~ " undervalued",
                                        TRUE ~ "Normal_price"), percent = abs(round(((sellingprice-mmr)/mmr)*100,2))) #Making variables
total_count <- nrow(Price_comparison)

Price_comparison <- Price_comparison %>%
  group_by(Price_Differential) %>%
  summarize(mean_diff_percent_price = round(mean(percent), 2),
            count = n()) %>%
  mutate(percent_of_total = count/total_count*100) %>%
  arrange(desc(mean_diff_percent_price))

Price_comparison # See Plot

# Visualization of ratio of overvalued and undervalued cars
ggplot(Price_comparison, aes(x = "", y = count, fill = Price_Differential))+
  geom_bar(stat = "identity",position = "fill")+
  labs(title = "Ratio of Overvalued and Undervalued Cars", x = "Ratio of cars", y = "Proportion %")


