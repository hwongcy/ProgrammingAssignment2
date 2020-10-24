## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function used to accept a matrix as an input and 
## cache the inverse of the matrix. 


makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # get the matrix
    get <- function() {
        x
    }
    
    # cache the inverse
    setinverse <- function(inverse) {
        i <<- inverse
    }
    
    # get the inverse from cache
    getinverse <- function() {
        i
    }
    
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
    
}


## Write a short comment describing this function

## This function is used to 
## 1. check whether the inverse is cached. If it is not cached,
##    it will compute the inverse of the matrix and
##    cache the inverse.
## 2. if the inverse is cached, it will return the cached inverse.

cacheSolve <- function(x, ...) {
    
    inverseMatrix <- x$getinverse()
    
    # check whether the inverse is cached.
    if(!is.null(inverseMatrix)) {
        
        # inverse is cached, print the message and return the inverse.
        message("inverse matrix cached")
        return(inverseMatrix)
    }
    
    # inverse is not cached.
    z <- x$get()
    inverseMatrix <- solve(z, ...) # find the inverse.
    x$setinverse(inverseMatrix) # cache the inverse.
    
    inverseMatrix # return the inverse.
    
}
