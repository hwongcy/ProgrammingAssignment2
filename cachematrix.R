## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # get the value of the matrix
    get <- function() x
    
    # set the value of the inverse
    setinv <- function(inv) m <<- inv
    
    # get the value of the inverse
    getinv <- function() m
    
    list(
        set = set,
        get = get,
        setinv = setinv,
        getinv = getinv
    )
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    
    m <- x$getinv()
    
    # check whether the inverse is cached.
    if(!is.null(m)) {
        
        # inverse is cached, print the message and return the inverse.
        message("inverse matrix cached")
        return(m)
    }
    
    # inverse is not cached.
    z <- x$get()
    m <- solve(z, ...) # find the inverse.
    x$setinv(m) # cache the inverse.
    
    m # return the inverse.
    
}
