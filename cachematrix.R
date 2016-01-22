## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This creates a special matrix object that caches its own inverse ("inv")
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL             ## Initially the inverse is not set
    set <- function(mat) {
        x <<- mat
        inv <<- NULL        ## Any old inverse is now obsolete, so clear it
        
    }
    get <- function() x
    setinverse <- function(inv) inv <<- inv
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## Write a short comment describing this function
## Computes the inverse of the matrix or grabs the cached inverse if it already exists.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        ## Found an existing inverse cached
        message("Using cached inverse")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)    ## Compute the inverse
    x$setinverse(inv)    ## Cache it for the next time
    inv
}
