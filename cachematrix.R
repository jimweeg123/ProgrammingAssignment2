## These functions provide facilities to cache a matrix and calculate
## its inverse.  If there is a second attempt to calculate the inverse
## of the same matrix, the inverse is retrieved from the cache rather
## than repeating the calculation.
##
## Example:
## > source("cachematrix.R")
## > j <- matrix(c(4,2,7,6), nrow=2, ncol=2)
## > j
##      [,1] [,2]
## [1,]    4    7
## [2,]    2    6
## > w <- makeCacheMatrix(j)
## > w$get()
##      [,1] [,2]
## [1,]    4    7
## [2,]    2    6
## > v <- cacheSolve(w)
## > v
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## > v <- cacheSolve(w)
## getting cached data
## > v
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4

## This function exposes the cache operations regarding
## the input matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the input matrix
## or retrieves it from the cache if it is available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

