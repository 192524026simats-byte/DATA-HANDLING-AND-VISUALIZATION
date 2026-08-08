library(ggplot2)
customer_data <- data.frame(
  CustomerID = c(1, 2, 3, 4, 5),
  Age = c(25, 30, 35, 28, 40),
  SatisfactionScore = c(4, 5, 3, 4, 5)
)

print("Customer Feedback Analysis Dataset:")
print(customer_data)

hist_plot <- ggplot(customer_data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Customer Age", 
       x = "Age", 
       y = "Frequency")

print(hist_plot)

score_counts <- table(customer_data$SatisfactionScore)
score_labels <- paste0("Score ", names(score_counts), " (", 
                       round(100 * score_counts / sum(score_counts), 1), "%)")
pie(score_counts, 
    labels = score_labels, 
    main = "Overall Customer Satisfaction Scores", 
    col = c("lightcoral", "lightblue", "lightgreen"))


customer_data$AgeGroup <- cut(customer_data$Age, 
                              breaks = c(20, 30, 45), 
                              labels = c("21-30", "31-45"))

stacked_bar_plot <- ggplot(customer_data, aes(x = AgeGroup, fill = as.factor(SatisfactionScore))) +
  geom_bar(position = "stack", color = "black") +
  theme_minimal() +
  labs(title = "Satisfaction Scores by Age Group",
       x = "Age Group",
       y = "Number of Customers",
       fill = "Satisfaction Score")

print(stacked_bar_plot)

