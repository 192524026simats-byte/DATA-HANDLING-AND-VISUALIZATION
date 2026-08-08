library(ggplot2)
inventory_data <- data.frame(
  ProductID = c(1, 2, 3, 4, 5),
  ProductName = c("Product A", "Product B", "Product C", "Product D", "Product E"),
  QuantityAvailable = c(250, 175, 300, 200, 220),
  Category = c("Electronics", "Office", "Electronics", "Furniture", "Office"), # Mock data
  Price = c(150, 45, 800, 120, 60) # Mock data
)

print("Product Inventory Dataset:")
print(inventory_data)
task1_plot <- ggplot(inventory_data, aes(x = ProductName, y = QuantityAvailable, fill = ProductName)) +
  geom_bar(stat = "identity", color = "black") +
  theme_minimal() +
  labs(title = "Quantity Available per Product", 
       x = "Product Name", 
       y = "Quantity Available")

print(task1_plot)
task2_plot <- ggplot(inventory_data, aes(x = Category, y = QuantityAvailable, fill = ProductName)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  theme_minimal() +
  labs(title = "Product Quantities Distributed by Category", 
       x = "Product Category", 
       y = "Total Quantity Available",
       fill = "Product Name")

print(task2_plot)
task3_plot <- ggplot(inventory_data, aes(x = Price, y = QuantityAvailable)) +
  geom_point(color = "darkred", size = 4) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Product Price vs. Quantity Available", 
       x = "Price ($)", 
       y = "Quantity Available")

print(task3_plot)
print("Findings: Based on the scatter plot and mock data, there is a loose positive correlation, but in a real-world scenario, we might expect higher-priced items to have lower available quantities.")
