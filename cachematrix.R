## Put comments here that give an overall description of what your
## functions do

## The function takes a square invertible matrix and has getters and setters for that matrix and inverse if any.
## The function returns a list object which gives access to all the sub-funtions defined within this.

makeCacheMatrix <- function(x = matrix()) {
        y <- NULL
        
        ## sets the value of x as matrix passed as a parameter
        setMatrix <- function(y) x<<-y
        ## returns the value of the x
        getmatrix <- function() x
 
        ## sets the inverse value of a matrix
        setInverse <- function(inverse) y <<- inverse
        ## returns the inverse value of matrix if already stored else returns null.
        getInverse <- function() y
 
        list(setMatrix = setMatrix, getmatrix = getmatrix,
        setInverse = setInverse,
        getInverse = getInverse)
}


## This function is responsible for returning the inverse of a matrix. It calculates the inverse of a matrix the first
## time and thereafter on any request for the inverse it returns the cached copy instead of computing it again.

cacheSolve <- function(x, ...) {
        ## Calls the getInverse() from above function on a list object returned by the later. 
        inverse <- x$getInverse()
        
        ## if the value for inverse is no null i.e. the inverse has already been cached from the earlier request
        ## then it simply returns the cached copy. 
        if(!is.null(inverse)){
        print("Inverse returned from cache")
        return(inverse)
        }
        ## If this is the first request for the inverse of a particular matrix then it computes the inverse of that
        ## matrix and first caches the inverse and then returns the value.
        data <- x$getmatrix()
        inverse <- solve(data)
        x$setInverse(inverse)
        
        inverse
}
