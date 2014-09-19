## These functions are used to cache the inverse of a matrix such that it need
## not be repeatedly calculated

## The function makeCacheMatrix makes a special "matrix" object that can cache the inverse
## This "matrix" is really a list of functions to: 1)set the value of the matrix,
##        2) get the value of the matrix, 3) set value of the inverse, 4) get value of inverse
  
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function returns the inverse of the special "matrix" created by makeCacheMatrix
## If already calculated it retreives from cache.  Otherwise it calculates the inverse

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
