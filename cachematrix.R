## PROGRAMMING ASSIGNMENT 2 
## Guillermo Monge - April, 2014
##
## This script contains two functions that enable the user
## to create matrices and calculate and cache their inverse.


## makeCacheMatrix function:
##    Returns a list containing functions to set, get a matrix
##      as well as set and get its inverse matrix.

makeCacheMatrix <- function(x = matrix(c(1,0,0,1),nrow=2,ncol=2)) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve function:
##    Returns the inverse of the matrix, and stores it. Should the inverse
##      already been calculated, gets the cached data.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()  ## assigns cached inverse
    if(!is.null(inv)) {  ## if cached data is not empty, print cached data
        message("getting cached data")
        return(inv)
    }
    data <- x$get()  ## otherwise, get matrix
    inv <- solve(data, ...)  ## calculate inverse
    x$setinverse(inv)  ## cache inverse matrix
    inv  ## return inverse of the matrix 'x'
}



## Example of use
# m <- makeCacheMatrix(x= matrix(c(1,23,15,0),nrow=2) )  ## creates matrix m
# cacheSolve(x)  ## computes mean
# cacheSolve(x)  ## gets computed mean from cache

