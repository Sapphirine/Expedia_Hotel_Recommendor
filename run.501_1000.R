if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = "/Users/zhanghaoyan/spark-2.0.1-bin-hadoop2.7")
}

library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))
sparkR.session(master = "local[*]", sparkConfig = list(spark.driver.memory = "2g"))



weights <- c(0.04446338, 0.16288116, 0.16548473, 0.61601504, 0.01115568)
distance <- function(a,b) {
  out <- (a-b)^2 * weights
  out <- sqrt(sum(out))
  out
}

# function to list cluster rates for a single user
recommending <- function(s) {
  user <- unique(book$user_id)[s]
  index <- which((train$user_id==user))
  srch.hist <- train[index, ]
  
  # calculating rating Rijm
  out <- matrix(nrow=100, ncol=nrow(srch.hist))
  for (i in 1:nrow(srch.hist)) {
    c1 <- srch.hist[i, ]
    x <- cbind(book$srch_ci, (book$srch_co-book$srch_ci), 
               book$is_package==c1$is_package, book$hotel_country==c1$hotel_country, book$srch_children_cnt)
    c1 <- cbind(c1$srch_ci, (c1$srch_co-c1$srch_ci),
                1, 1, c1$srch_children_cnt)
    x <- rbind(c1, x)
    x <- scale(x, center=T, scale=T)
    c <- x[1, ]
    x <- x[-1, ]
    
    L <- dim(x)[1]
    d <- vector(length=L)
    for (l in 1:L) {
      d[l] <- distance(x[l, ],c)
    }
    
    # average over n, get Rijr
    recommend <- cbind(d, as.factor(book$hotel_cluster))
    out[ ,i] <- aggregate(1/(recommend[,1]+0.1), by=list(Category=recommend[,2]), FUN=mean)$x
  }
  
  # average over j, get Rir
  rate <- cbind(1:100, apply(out, 1, mean) )
  
  # print out the recommended clusters from high prob to low prob
  R = 100
  recommend <- vector(length=R)
  for (j in 1:R) {
    r <- sort(rate[,2])[101-j]
    ind <- which(rate[,2]==r)
    recommend[j] <- ind
  }
  
  return(recommend)
}



# create a function to run recommender from a certain point to a certain point
# Note: include start and include stop
run <- function(start, stop) {
  part.recommend <- matrix(nrow=(stop-start+1), ncol=100)
  for (s in start:stop) {
    r <- recommending(s)
    r <- as.numeric(r)
    part.recommend[s-start+1, ] <- r
  }
  return(part.recommend)
}



# recommend for users of the target range and save into csv file
d <- run(501,1000)
write.csv(d, file="data.501_1000")
