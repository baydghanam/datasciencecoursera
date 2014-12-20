## Put comments here that give an overall description of what your
## functions do
## The idea is to feed a matrix to makeCacheMatrix() and use 
## cacheSolve() to check if it's value had perviously been cached
## if not, to calculate it. The result is displayed sating if
## the value was retrieved from cache.

## Comment describing makeCacheMatrix:
## The following function takes a matrix as its primary argument.
## It defines "inv" to which the matrix inverse is meant to be 
## assigned
## It defines a function "set" that will be called to calculate 
## and retrieve the value of "inv"
## It defines "setinv" that calculates the matrix inverse and 
## superassigns it to "inv"
## It defines methods to retrieve the value of inv to be used by
## other functions

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Comment describing cacheSolve:
## This function takes as argument the value returned by
## makeCacheMatrix 
## It calls on the methods defined in makeCacheMatrix to verify if
## the inverse exists in the cache, if it's there it displays a 
## statement to that effect and displays it.
## Otherwise it calls on a makeCaheMatrix defined method calculates
## it and displays it.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}