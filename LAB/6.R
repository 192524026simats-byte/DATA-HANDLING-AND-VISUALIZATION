library(ggplot2)
library(tidyr)
product_sales <- data.frame(
  ProductID = c(1, 2, 3),
  ProductName = c("Product A", "Product B", "Product C"),
  January = c(2000, 1500, 1200),
  February = c(2200, 1800, 1400),
  March = c(2400, 1600, 1100)
)
sales_long <- pivot_longer(product_sales, 
                           cols = c("January", "February", "March"), 
                           names_to = "Month", 
                           values_to = "Sales")
sales_long$Month <- factor(sales_long$Month, levels = c("January", "February", "March"))
sales_long$MonthNum <- as.numeric(sales_long$Month)
print("Monthly Sales Data for Each Product:")
print(product_sales)
grouped_bar <- ggplot(sales_long, aes(x = ProductName, y = Sales, fill = Month)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  theme_minimal() +
  labs(title = "First Quarter Sales by Product", 
       x = "Product Name", 
       y = "Sales ($)",
       fill = "Month")

print(grouped_bar)
stacked_area <- ggplot(sales_long, aes(x = MonthNum, y = Sales, fill = ProductName)) +
  geom_area(alpha = 0.8, color = "white") +
  scale_x_continuous(breaks = 1:3, labels = c("January", "February", "March")) +
  theme_minimal() +
  labs(title = "Overall Sales Trend over Q1", 
       x = "Month", 
       y = "Total Sales ($)",
       fill = "Product Name")

print(stacked_area)