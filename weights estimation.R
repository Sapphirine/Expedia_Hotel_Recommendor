library(readr)
train <- read_csv("/Users/zhanghaoyan/Downloads/train.csv")
train <- na.omit(train)
dim(train)
book <- train[train$is_booking==1, ]
browse <- train[train$is_booking!=1, ]

# one subset
library("randomForest")
set.seed(8312)
index <- sample(dim(book)[1], 5000)
b1 <- book[index, ]
b1 <- na.omit(b1)
stay_length <- b1$srch_co-b1$srch_ci

rf <- randomForest(as.factor(b1$hotel_cluster)~as.factor(b1$is_mobile)+as.factor(b1$is_package)+
                     b1$srch_adults_cnt+b1$srch_children_cnt+stay_length,  mtry=5, importance=T)
rf$importance[ ,101]

# estimate the weights
T = 10
out <- matrix(0, 5, T)
for (t in 1:T) {
  index <- sample(dim(book)[1], 5000)
  b1 <- book[index, ]
  b1 <- na.omit(b1)
  stay_length <- b1$srch_co-b1$srch_ci
  
  rf <- randomForest(as.factor(hotel_cluster)~srch_ci+stay_length+is_package+
                       hotel_country+srch_children_cnt
                     , data=b1, mtry=3, importance=T)
  out[ ,t] <- rf$importance[ ,101]
}
out

weights <- apply(out, MARGIN=1, mean)
weights <- weights / sum(weights)


# calculate the distances between first customer and other customers
index <- which((book$user_id==12))
c1 <- book[index, ]
c <- cbind(c1$srch_ci, (c1$srch_co-c1$srch_ci), 0, 0, 0, 0, c1$srch_adults_cnt, c1$srch_children_cnt)
x <- cbind(train$srch_ci, (train$srch_co-train$srch_ci), train$is_mobile==c1$is_mobile,
           train$is_package==c1$is_package, train$srch_destination_type_id==c1$srch_destination_type_id,
           train$hotel_country==c1$hotel_country, train$srch_adults_cnt, train$srch_children_cnt)
x <- scale(x, center=T, scale=T)
c <- x[index, ]
x <- x[-index, ]
L <- dim(x)[1]
weights <- cbind(weights[1], weights[2], 0.01, weights[3], 0.01, weights[4], 0.01, weights[5])
distance <- function(a,b) {
  out <- (a-b)^2 * weights
  out <- sqrt(sum(out))
  out
}

# calculate the distances
d <- vector(length=L)
for (i in 1:L) {
  d[i] <- distance(x[i,],c)
}

recommend <- cbind(d, as.factor(train$hotel_cluster[-index]))
aggregate(1/(recommend[,1]+0.1), by=list(Category=recommend[,2]), FUN=mean)
