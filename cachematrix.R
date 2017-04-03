## Rafael F. Pagan
## R Programming
## by John Hopkins University
## Assignment for Week 3
## These functions will cache the inverse of a matrix.

## Takes in an invertible matrix X such that it can be stored in memory with > MakeCacheMatrix(X)
## and displayed by using the command >X$get().

## USAGE:
## > mat<-matrix(1:4,2,2)
## mat
##      [,1] [,2]                               ## invertible matrix
##[1,]    1    3
##[2,]    2    4
## > m1<-makeCacheMatrix(mat)                   ## matrix created

## **ALTERNATIVE METHOD**
## mat<-matrix(1:4,2,2)
## m1$set(mat)                                  ## store using the enclosed 'set' function

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL                              
        set <- function(y) {
                x <<- y
                I <<- NULL 
        }
        get <- function() x 
        setinverse <- function(solve) I <<- solve
        getinverse <- function() I
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Takes the output of makeCacheMatrix and stores its inverse into cache.
## USAGE:
## > cacheSolve(m1)
##[,1] [,2]                                     ## inverse of the matrix
##[1,]   -2  1.5
##[2,]    1 -0.5

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getinverse()
        if(!is.null(I)) {
                message("getting cached matrix data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)                  ## calculates inverse
        x$setinverse(I)
        I
}
