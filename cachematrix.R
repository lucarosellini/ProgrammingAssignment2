## As in the provided example, makeCacheMatrix wraps the
## provided matrix 'x' using a special list that exposes
## four methods:
## 1. set: sets the values of the matrix
## 2. get: returns the value of the matrix
## 3. setinverse: sets the inverse of the matrix
## 4. getinverse: returns the inverse of the matrix (using the cached value, if exists)
##
makeCacheMatrix <- function(x = matrix()) {
    # the inverse value, initialized with NULL
    inv <- NULL
    
    # sets the value of the matrix, called 'm'
    set <- function(m){
        x <<- m
        
        # if a new matrix is provided, the cached value for the inverse must be invalidated
        inv <<- NULL
    }
    
    # returns the matrix
    get <- function(){
        x
    }
    
    # getting the cached value for the inverse of the matrix 
    getinverse <- function(){
        inv
    }
    
    setinverse <- function(inverse){
        inv <<- inverse
    }
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## the following function contains the logic needed to calculate and to cache
## the inverse of the matrix wrapped using the function above.
##
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    
    ## get the cached inverse
    inv <- x$getinverse()
    
    ## checks if the inverse has already been calculated. 
    ## If yes, return the cached inverse. 
    if (!is.null(inv)){
        message("Returning the cached inverse matrix")
        return(inv)
    }
    
    ## No cached inverse has been found, let's calculate the inverse
    
    # getting the matrix for which we need to calculate the inverse
    m <- x$get()
    
    # calc the inverse propagating any additional parameter the user may have provided
    inv <- solve(m, ...)
    
    # cache the computed inverse
    x$setinverse(inv)
    
    # return the computed inverse
    inv
}
