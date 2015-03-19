
## The << assignment operator is used to implement cached values of a matrix inversion. This problem is not fully 
## specified as, for example, what happens when the function is called with a changed value. cacheSolve has two arguments. The
## first is a list of four functions - a get and set function for the incoming array and a get and set function for
## the cached inverted matrix. The second argument is the array to be inverted. The first test is whether the array to be inverted
## has been modified. If so, the cached values of the set for the incoming array and the set for the cached inverted array are
## modified to reflect the changed array. If the array was not modified, then a test is made to see if the cache value has to
## calculated. If no, return the cached value. If yes, calculate the inverted matrix and cache it. Here is some example runs
## 
## > x <- stats::rnorm(16)
## > dim(x) <- c(4,4)
## > m <- makeCacheMatrix(x)
## > cacheSolve(m,x)
## [,1]       [,2]       [,3]        [,4]
## [1,]  0.09007438 -0.4531021  0.6151197 -0.03915281
## [2,]  0.07924133  0.5887923  0.2814162 -0.46584067
## [3,] -0.19697683 -0.6426029  0.7271492 -0.47339112
## [4,]  0.38170659  0.2200105 -0.5181898 -0.13789317
## > y <- cacheSolve(m,x)
## getting cached data
## > cacheSolve(m,y)
## Cached Matrix was modified
## [,1]       [,2]       [,3]       [,4]
## [1,]  2.26613139  0.2092266 -0.8589063  1.5983889
## [2,] -0.02766335  1.0189113 -0.8177392 -0.6269837
## [3,]  1.34761267  0.6941942 -0.5737281 -0.7581881
## [4,]  1.16461128 -0.4038635 -1.5262638 -0.9785987
## > x
## [,1]       [,2]       [,3]       [,4]
## [1,]  2.26613139  0.2092266 -0.8589063  1.5983889
## [2,] -0.02766335  1.0189113 -0.8177392 -0.6269837
## [3,]  1.34761267  0.6941942 -0.5737281 -0.7581881
## [4,]  1.16461128 -0.4038635 -1.5262638 -0.9785987
##___________________________________________________
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


## This function checks two things. First it checks to see if the original matrix had been modifed. It does this by testing using
## the identical function. If yes, it replaces the old area by the new array, solves the new array, and puts the inverted array in the 
## cache. It next tests whether the inversion has been yet done - if so, it fetches the cached value. If no, the matrix is inverted
## and the value put into the cache. Messages are passed to the user if the array has been modifed, and for this assignment, 
## if cached data is being retrieved.

cacheSolve <- function(x,y, ...) {
        old_matrix <- x$get()
        
        if (!identical(old_matrix,y)) {
                x$set(y)
                invert <- solve(y, ...)
                x$setinvert(invert)
                message("Cached Matrix was modified")
                return(invert)             
        }
        
        invert <- x$getinvert() 
        if(!is.null(invert)) {
                message("getting cached data")
                return(invert)
        }
        
        matrix <- x$get()
        invert <- solve(matrix, ...)
        x$setinvert(invert)
        invert
}
