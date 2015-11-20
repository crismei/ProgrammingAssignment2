#    Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly.
# Using the below functions, you can obtain a invert matrix retrieved without any 
# computation if this has already been calculated before.


#   Function MakeCacheMatrix
# This function creates a special "matrix", which is really a list containing a 
# function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix (setinverse())
# 4. get the value of inverse of the matrix (getinverse())

makeCacheMatrix <- function(x = matrix()) {
        matrixInv <- NULL
        set <- function(y) {
                x <<- y
                matrixInv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) matrixInv <<- inverse
        getinverse <- function() matrixInv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#   Function cacheSolve
# This function returns the inverse of the special "matrix".If the inverse has 
# already been calculated, the inverse is returned by retrieving the inverse from
# the cache.


cacheSolve <- function(x, ...) {
        matrixInv <- x$getinverse()
        if(!is.null(matrixInv)) {
                message("Getting cached data,retrieve the inverse from the cache")
                return(matrixInv)
        }
        data <- x$get()
        message("No cached data, invert the matrix through function solve()")
        matrixInv <- solve(data)
        message("Storage the result in cache through setinverse() ")
        x$setinverse(matrixInv)
        return(matrixInv)
}
