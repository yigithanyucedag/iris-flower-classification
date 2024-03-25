# Importing the Dataset into R
data <- read.csv("/Users/yigithanyucedag/School/r-course/midterm/dataset.csv")

# Data Understanding and Exploration
# Question 1: Describe the dataset briefly. What are the main features and their data types?
# Question 2: How many observations and variables are there in the dataset?
# By using str() function, we can see the structure of the data
str(data)
# By using dim() function, we can see the number of observations and variables
dim(data)
# Answer 1: This dataset includes three iris species with 50 samples each as well as some properties of the flowers.
# The variables are Id, SepalLengthCm, SepalWidthCm, PetalLengthCm, PetalWidthCm, and Species.
# Id is an integer, SepalLengthCm, SepalWidthCm, PetalLengthCm, and PetalWidthCm are numeric, and Species is a character variable.
#
# Answer 2: The dataset has 150 observations and 6 variables.



# Data Analysis and Visualization
# Get summary statistics for numerical features (SepalLengthCm, SepalWidthCm, PetalLengthCm, PetalWidthCm)
sum <- summary(data[, 2:5])
print(sum)
# Visualize the distribution of a numerical feature
hist(data$SepalLengthCm, main = "Distribution of Sepal Length", xlab = "Sepal Length (cm)", col = "lightblue")
# Explore the relationship between two numericalvariables using a scatter plot
plot(data$SepalLengthCm, data$SepalWidthCm, main = "Sepal Length vs. Sepal Width", xlab = "Sepal Length (cm)", ylab = "Sepal Width (cm)", col = "blue")



# Data Manipulation with dplyr
# Filter data to include only flowers of a specific species
library(dplyr)
setosa <- data %>% filter(Species == "Iris-setosa")
print(head(setosa))
# Group by flower species and calculate the mean Sepal Length for each species
species_mean <- data %>%
    group_by(Species) %>%
    summarise(mean(SepalLengthCm))
# Arrange data in descending order based on Petal Width
arranged_data <- data %>%
    arrange(desc(PetalWidthCm))
# Create a new feature that calculates the area of the petal (assuming Petal Length * Petal Width)
data <- data %>%
    mutate(PetalArea = PetalLengthCm * PetalWidthCm)



# Data Visualization with ggplot2
library(ggplot2)
# Create a histogram to visualize the distribution of Sepal Length
ggplot(data, aes(x = SepalLengthCm)) +
    geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
    labs(title = "Distribution of Sepal Length", x = "Sepal Length (cm)", y = "Frequency")
# Create a scatter plot to explore the relationship between Petal Length and Petal Width
ggplot(data, aes(x = PetalLengthCm, y = PetalWidthCm, color = Species)) +
    geom_point() +
    labs(title = "Petal Length vs. Petal Width", x = "Petal Length (cm)", y = "Petal Width (cm)")
# Create a bar plot to compare the number of flowers in each species category
# Note: This dataset has 50 samples for each species, so the counts will be equal
ggplot(data, aes(x = Species, fill = Species)) +
    geom_bar() +
    labs(title = "Number of Flowers in Each Species", x = "Species", y = "Count") +
    theme_minimal()



# Combining dplyr and ggplot2
# Preprocess data with dplyr before plotting with ggplot2 (filter by Sepal Length > 5)
large_sepal_data <- data %>% filter(SepalLengthCm > 5)
# Create a scatter plot of Petal Length vs. Petal Width for flowers with Sepal Length > 5
ggplot(large_sepal_data, aes(x = PetalLengthCm, y = PetalWidthCm, color = Species)) +
    geom_point() +
    labs(title = "Petal Length vs. Petal Width (Sepal Length > 5)", x = "Petal Length (cm)", y = "Petal Width (cm)")
# Create a grouped bar plot using ggplot2 to compare the mean values of a numerical variable across different groups defined by a categorical variable, with the data processed using dplyr.
ggplot(species_mean, aes(x = Species, y = `mean(SepalLengthCm)`, fill = Species)) +
    geom_bar(stat = "identity") +
    labs(title = "Mean Sepal Length by Species", x = "Species", y = "Mean Sepal Length (cm)") +
    theme_minimal()
# Use dplyr to calculate summary statistics (e.g., mean, median) for different groups in the dataset, and then visualize these statistics using ggplot2 (e.g., as points on a line plot or as bars in a bar plot).
summary_stats <- data %>%
    group_by(Species) %>%
    summarise(mean = mean(PetalArea), median = median(PetalArea))
ggplot(summary_stats, aes(x = Species, y = mean, fill = Species)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_point(aes(y = median), color = "black", size = 3) +
    labs(title = "Summary Statistics of Petal Area by Species", x = "Species", y = "Petal Area") +
    theme_minimal()
