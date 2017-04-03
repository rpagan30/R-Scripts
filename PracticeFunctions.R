add2 <- function(x,y) {
        x + y
}

above10 <- function(x) {
        use <- x > 10
        x[use]
}

above <- function(x,n= 10) {
        #Uses 10 as a default value
        use <- x > n 
        x[use]#Subset x for x>10
}

columnmean <- function(y, removeNA=TRUE) {
        nc < ncol(y) #figures out number of columns
        means <- numeric(nc) # stores the means
        for(i in 1:nc) {
          means[i] <- mean(y[,i],na.rm = removeNA)
        }
        means
}
