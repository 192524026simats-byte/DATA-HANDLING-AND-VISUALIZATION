library(ggplot2)
website_traffic <- data.frame(
  Date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-05")),
  Page_Views = c(1500, 1600, 1400, 1650, 1800),
  CTR = c(2.3, 2.7, 2.0, 2.4, 2.6) 
)

print("Website Traffic Dataset:")
print(website_traffic)
user_interactions <- data.frame(
  Date = rep(as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-05")), 3),
  Interaction = rep(c("Likes", "Shares", "Comments"), each = 5),
  Count = c(
    150, 165, 140, 175, 190,  # Mock Likes
    50,  60,  45,  70,  85,   # Mock Shares
    20,  25,  15,  30,  40    # Mock Comments
  )
)

plot_task1 <- ggplot(website_traffic, aes(x = Date, y = Page_Views)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "black", size = 3) +
  theme_minimal() +
  labs(title = "Trend in Daily Page Views", 
       x = "Date", 
       y = "Page Views")

print(plot_task1)
top_n_days <- website_traffic[order(-website_traffic$CTR), ][1:3, ]

plot_task2 <- ggplot(top_n_days, aes(x = reorder(format(Date, "%b %d"), -CTR), y = CTR, fill = CTR)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(title = "Top 3 Days by Click-through Rate", 
       x = "Date", 
       y = "Click-through Rate (%)")

print(plot_task2)
plot_task3 <- ggplot(user_interactions, aes(x = Date, y = Count, fill = Interaction)) +
  geom_area(alpha = 0.8, color = "white", size = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(title = "Distribution of User Interactions on Website", 
       x = "Date", 
       y = "Total Interactions",
       fill = "Interaction Type")

print(plot_task3)
