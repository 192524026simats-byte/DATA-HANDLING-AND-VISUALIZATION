# Load the ggplot2 library for visualizations
# Run install.packages("ggplot2") first if you do not have it installed
library(ggplot2)

# ---------------------------------------------------------
# DATASET CREATION
# ---------------------------------------------------------
# Dataset: Employee Performance
emp_data <- data.frame(
  EmployeeID = c(1, 2, 3, 4, 5),
  Department = c("Sales", "HR", "Marketing", "Sales", "HR"),
  YearsOfService = c(5, 3, 7, 4, 2),
  PerformanceScore = c(85, 92, 78, 90, 76)
)

print("Employee Performance Dataset:")
print(emp_data)

# ---------------------------------------------------------
# TASK 1: Line Chart
# Visualize the performance trend of employees over time.
# Note: We sort by Years of Service to show a logical "time" progression.
# ---------------------------------------------------------
# Sort the data by Years of Service
emp_data_sorted <- emp_data[order(emp_data$YearsOfService), ]

line_chart <- ggplot(emp_data_sorted, aes(x = YearsOfService, y = PerformanceScore)) +
  geom_line(color = "blue", size = 1) +
  geom_point(aes(color = Department), size = 3) +
  theme_minimal() +
  labs(title = "Performance Trend Over Years of Service",
       x = "Years of Service",
       y = "Performance Score") +
  theme(legend.position = "right")

print(line_chart)

# ---------------------------------------------------------
# TASK 2: Bar Chart
# Show the distribution of employees across different departments.
# ---------------------------------------------------------
bar_chart <- ggplot(emp_data, aes(x = Department, fill = Department)) +
  geom_bar(color = "black") +
  theme_minimal() +
  labs(title = "Employee Distribution by Department",
       x = "Department",
       y = "Number of Employees") +
  scale_y_continuous(breaks = c(0, 1, 2, 3)) # Ensuring whole numbers for count

print(bar_chart)

# ---------------------------------------------------------
# TASK 3: Scatter Plot
# Analyze the correlation between years of service and performance scores[cite: 1].
# ---------------------------------------------------------
scatter_plot <- ggplot(emp_data, aes(x = YearsOfService, y = PerformanceScore)) +
  geom_point(aes(color = Department), size = 4) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgray", linetype = "dashed") + # Adds a trendline
  theme_minimal() +
  labs(title = "Correlation: Years of Service vs. Performance Score",
       x = "Years of Service",
       y = "Performance Score")

print(scatter_plot)

