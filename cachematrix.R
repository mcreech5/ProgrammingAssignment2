## This code contains two functions for creating a special matrix with caching ability
## and calculating the inverse of an input matrix (assuming input is invertible).

## makeCacheMatrix creates a special matrix with the ability to cache a previously calculated
## inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function checks if the input contains an inverse. If the inverse exists, the function
## will not calculate the new inverse and instead return the cached value.
## If the inverse = NULL, the function will calculate and cache the inverse matrix.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x%setinv(i)
        i
}
