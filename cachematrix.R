## Put comments here that give an overall description of what your
## functions do
### A function that is able to cache potentially time-consuming computations.
### This functions does a Matrix inversion is usually a costly computation.
### using some benefit to caching the inverse of a matrix rather than computing 
### it repeatedly.

## This function creates a special "matrix" object that can cache its 
## inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("cached inverse matrix found, getting the matrix...")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

# Testing
# special_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
# cacheSolve(special_matrix)

# Functions used in this assignment
# solve: Computing the inverse of a square matrix.
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.
