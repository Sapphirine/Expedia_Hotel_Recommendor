recom <- read.csv("C:/Users/Administrator/Desktop/data.501_1000")

S <- 500
user <- unique(book$user_id)[1:S]

A.in.B <- function(A, B) {
  l.a <- length(A); l.b <- length(B)
  out = 0
  for (i in 1:l.a) {
    check <- A[i]
    for (j in 1:l.b) {
      if (check == B[j]) out = 1
    }
  }
  return(out)
}

precision <- function(begin, end) {
  right <- 0
  for (i in 1:S) {
    u <- user[i]
    index <- (book$user_id == u)
    a <- book$hotel_cluster[index]
    b <- recom[i, begin:end]
    right <- right + A.in.B(a, b)
  }
  return(right)
}

# precision
precision(1,10)/S