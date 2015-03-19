
## The << assignment operator is used to implement cached values of a matrix inversion. The function makeCacheMatrix
## creates a list of four functions - a get and set function for the incoming array and a get and set function for
## the cached inverted matrix. Note that for each matrix to be cached, a new invocation of makeCacheMatrix must be made
## setting new values for the matrix in the calling environment. Similarly, if the array has changed, 
## a new invocation of makeCacheMatrix must be made. Here is some sample output:
## 
## 
## > x <- stats::rnorm(16)
## > dim(x) <- c(4,4)
## > m <- makeCacheMatrix(x)
## > cacheSolve(m)
## [,1]       [,2]       [,3]       [,4]
## [1,] -0.3378125 -0.9445924 -0.7689950  0.7487606
## [2,] -3.3116676 -3.2248763 -1.0699306  0.8097309
## [3,] -0.2663809  0.2229054 -0.3784120 -0.1330681
## [4,] -1.1471525 -1.3451847 -0.3012276  0.9753661
## > cacheSolve(m)
## getting cached data
## [,1]       [,2]       [,3]       [,4]
## [1,] -0.3378125 -0.9445924 -0.7689950  0.7487606
## [2,] -3.3116676 -3.2248763 -1.0699306  0.8097309
## [3,] -0.2663809  0.2229054 -0.3784120 -0.1330681
## [4,] -1.1471525 -1.3451847 -0.3012276  0.9753661
##  


## This function returns the list of four functions with the "<<-" used to pick the matrix values out of the appropriate
## environment (calling environment) when invoked in the cacheSolve function for the two functions that set the values 
## of the array and it's inverse.

makeCacheMatrix <- function(x = matrix()) {m <- NULL
                                           
                                           set <- function(y) {
                                                   x <<- y
                                                   m <<- NULL
                                           }
                                           
                                           get <- function() x
                                           
                                           setinvert <- function(invert) m <<- invert
                                           
                                           getinvert <- function() m
                                           
                                           list(set = set, get = get,
                                                setinvert = setinvert,
                                                getinvert = getinvert)
                                           
}


## This function checks to see if the matrix had been inverted and cached. If yes, the cached value is returned. If
## no, the matrix is obtained using get, solved using R's solve function, and stored using setinvert. Note that this
## function takes the list of functions as input.

cacheSolve <- function(x,y, ...) {
        matrix <- x$get()
        matrix <- x$getinvert() 
        if(!is.null(matrix)) {
                message("getting cached data")
                return(matrix)
        }
        
        matrix <- x$get()
        invert <- solve(matrix, ...)
        x$setinvert(invert)
        invert
}

