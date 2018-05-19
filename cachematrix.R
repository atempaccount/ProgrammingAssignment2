## The code below allows one to cache the computation of matrix inverses. For
## example:
## m <- matrix(1:4, ncol=2)
## wrap <- makeCacheMatrix(m)
## inverse <- cacheSolve(wrap)
## [some more code, inverse gets changed]
## inverse <- cacheSolve(wrap)
## [cache was used]

## This function creates a wrapper object for a matrix to use for cached inverse
## computation.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function takes an object constructed by `makeCacheMatrix' and returns
## the (possibly cached) inverse of the matrix. The extra arguments get passed
## to the `solve' function together with the matrix, if cached result is not
## available.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    m <- x$get()
    inverse <- solve(m, ...)
    x$setinverse(inverse)
    inverse
}
