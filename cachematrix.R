## Put comments here that give an overall description of what your
## functions do
## The functions written in partial fulfillment of R Programming
## Week 3 Assignment on January 19, 2020 by Yujiedoris


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## It gives a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) { ## create the argument
    inverse_ <- NULL            ## inverse_ as NULL
    set <- function(y) {        ## create a function
        x <<- y                 ## give a value of matrix in parent environment
        inverse_ <<- NULL       ## if there is a new matrix, it will reset inv as NULL
     }
     get <- function() x        ## create a fucntion
                
     setinverse <- function(inverse) inverse_ <<- inverse  ## set a value of inv in parent environment
     getinverse <- function() inverse_                     ## get the value of inv 
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## refer to the functions with operator
                                                                                   
}


## Write a short comment describing this function
## This function gives the inverse of the matrix.
## If the inverse is calculated, cacheSolve will retrieve the inverse.

cacheSolve <- function(x, ...) {
        ## Return the inverse of 'x'
        inverse_ <- x$getinverse()
        if(!is.null(inverse_)) {
                message("getting cached data")
                return(inverse_)
        }
        data <- x$get()
        inverse_ <- solve(data, ...)
        x$setinverse(inverse_)
        inverse_
}

