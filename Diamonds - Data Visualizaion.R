#Clear Console and enviroment
rm(list=ls())
cat("\014")

#Scientific Notation
options("scipen"=99999, digits=3)

#Plot Margins
par(mar = c(1, 1, 1, 1))



library(tidyverse) #used for data wrangling and data visualization
library(vtable) # Used to plot table with summary statistics


#load diamonds dataset
data(diamonds)

Sumstats <- sumtable(diamonds)
st(statstable, digits = 3, fixed.digits = TRUE)
Sumstats

# Analyzing the relationship between Price and Cut
plot1 <- ggplot( diamonds , aes(x = cut, y = price, color = cut)) +
      geom_boxplot() + 
   labs(title = "Analyzing the spread of prices",
       subtitle = "Plot of Pirce by Cut",
       caption = "Data source: R Databases",
       x = "Type of Cut", y = "Price ($)",
       tag = "Fig.1") +
    theme(
    plot.title = element_text(color = "steelblue4", hjust = 0.5, size = 14, face = "bold"),    # Color,Center title position and size
    plot.subtitle = element_text(color = "black" , hjust = 0, face = "bold") ,          # Color and Center subtitle
    plot.caption = element_text(color = "thistle" , hjust = 1, face = "italic"),
    plot.tag = element_text(color = "turquoise4" , hjust = 1, face = "italic") 
  )
plot1



# Analyzing the relationship between Price and Clarity
plot2 <- ggplot( diamonds , aes(x = clarity, y = price, color = clarity)) +
  geom_boxplot() + 
  labs(title = "Analyzing the spread of prices",
       subtitle = "Plot of Pirce by Clarity",
       caption = "Data source: R Databases",
       x = "Type of Clarity", y = "Price ($)",
       tag = "Fig.2") +
  theme(
    plot.title = element_text(color = "steelblue4", hjust = 0.5, size = 14, face = "bold"),    # Color,Center title position and size
    plot.subtitle = element_text(color = "black" , hjust = 0, face = "bold") ,          # Color and Center subtitle
    plot.caption = element_text(color = "thistle" , hjust = 1, face = "italic"),
    plot.tag = element_text(color = "turquoise4" , hjust = 1, face = "italic") 
  )
plot2


# Plotting the distribution of Price
plot3 <- ggplot( diamonds,  aes(x=price)) +
  geom_histogram( binwidth=15, fill="cyan4", color="cyan4", alpha=0.9) +
  ggtitle("Bin size = 3") +
  theme(
    plot.title = element_text(size=15)
  )
plot3
