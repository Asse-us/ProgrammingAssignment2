## Title: Caching the Inverse of a Matrix
## This assignment is all about caching the inverse of a matrix. 
## The goal is to write a pair of functions that can compute and cache the inverse of a matrix. 
## These functions are makeCacheMatrix() and cacheSolve(). 
## The first one creates a special “matrix” object that can cache its inverse. 
## And the second one computes the inverse of the special “matrix” returned by makeCacheMatrix(). 
## In this assignment, it is assumed that the matrix supplied is invertible. 


## The makeCacheMatrix() creates a special “matrix” object that can cache its inverse. 
## In this function, x is an assumed invertible matrix which returns 
## a list containing functions to set the matrix, get the matrix, set the inverse, and get the inverse. 
## This list is used as an input to catcheSolve() function. 
## `<<-` is used to assign a value to an object in an environment different from the current environment.


makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set<- function(y){
                x<<- y
                inv<<- NULL
        }
        get<- function()x
        setinv<- function(inverse) inv<<- inverse
        getinv<- function()inv
        list(set = set, get = get,
             setinv = setinv, getinv = getinv)
}


## The cacheSolve() computes the inverse of the special “matrix” returned by makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix has not changed, 
## then cacheSolve will retrieve the inverse from the cache directly.

cacheSolve <- function(x, ...) {
        inv<- x$getinv()
        if (!is.null(inv)){
                message("Getting cached data")
                return(inv)
        }
        m.data<- x$get()
        inv<- solve(m.data, ...)
        x$setinv(inv)
        return(inv)
}
