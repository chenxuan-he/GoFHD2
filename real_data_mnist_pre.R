# preprocessing code
library(keras)

# Load the MNIST dataset
mnist <- dataset_mnist()

# Extract training and test data
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# Filter the data to keep only the digits "4" and "9"
train_filter <- y_train == 4 | y_train == 9
test_filter <- y_test == 4 | y_test == 9

x_train_filtered <- x_train[train_filter,,]
y_train_filtered <- y_train[train_filter]

data <- cbind(y=y_train_filtered, data.frame(x_train_filtered))

set.seed(0)
n <- dim(data)[1]
p <- dim(data)[2]
ind <- sample(1:n, 1000)
data <- data[ind, ]
data$y[data$y==4] <- 0
data$y[data$y==9] <- 1

save(data, file = "real_data_mnist.RData")
