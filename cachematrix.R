## The functions makeCacheMatrix and cacheSolve are used in combination
## in order to initialize/store a matrix in a matrix-type object and
## return the inverse of the stored matrix respectively.

## Initiates a "CacheMatrix" object with setter and
## getter methods
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
        
    get <- function() x
    
    setinv <- function(inv) i <<- inv
    
    getinv <- function() i
    
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)

}


## Returns the inverse of a matrix which is originally kept in a
## "CacheMatrix" object. Checks if the object has a pre-calculated
## (non-empty) "inverse" attribute and returns it. If not, computes
## the inverse of the matrix and "sends" this inverse value to the
## using the setinv method of the "CacheMatrix" object
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
            message("getting cached inverse")
            return(i)
            }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setinv(i)
        message("no cached inverse, computing it")
        i
}
