## These functions, makeCacheMatrix() and cacheSolve() are used in conjunction 
## to create a matrix object that can calculate and cache its own inverse result.

## Usage:
## a <- matrix(1:4,2,2)
## mat <- makeCacheMatrix(a)
## cacheSolve(mat)

## Assumptions:
## Matrix is always invertable

## makeCacheMatrix() creates a 'special' matrix object that is able to cache its inverse result.

makeCacheMatrix <- function(x = matrix()) {
    ## x is a square matrix that is invertable
    ## Returns a list containing the following four functions:
    ## get - gets the matrix 
    ## set - set the matrix
    ## getInverse - gets the matrix inverse
    ## setInverse - sets the matrix inverse
    
    inv <- NULL
    set <- function(y){
        # <<- assigns a value to an object from environment different to current 
        x <<- y
        # Inverse placeholder
        inv <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve() calculates the inverse of the matrix object returned by makeCacheMatrix().
## If the inverse has been calculated already and matrix is unchnaged it will retrieve result from cache;
## if not, the inverse will be calculated using the solve function.

cacheSolve <- function(x, ...) {
    ## x is the output of makeCacheMatrix()
    ## Returns the inverse of matrix defined in original makeCacheMatrix call.
    inv <- x$getInverse()
    
    # Get cached inverse if inv not null i.e. inverse calculated already
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    # As not in cache need to calculate matrix inverse
    data <- x$get()
    
    inv <- solve(data,...)
    
    #Store calculated inverse into cache
    x$setInverse(inv)
    return(inv)
}