bar plot----------------------------------
temperatures <- c(20, 22, 25, 29, 23, 27, 28)
result <- barplot(temperatures,
main = "Maximum Temperatures in a Week",
xlab = "Degree Celsius",
ylab = "Day",
names.arg = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
col = "blue",
)
print(result)
horizantal plot--------------------------
# Example data
categories <- c("A", "B", "C", "D")
values <- c(10, 20, 15, 30)
# Create a horizontal bar plot
barplot(values, names.arg = categories,
        main = "Horizontal Bar Plot Example",
        xlab = "Values",
        ylab = "Categories",
        horiz = TRUE,
        col = "blue")
