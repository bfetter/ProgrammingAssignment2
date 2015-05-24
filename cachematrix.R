## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix function inputs an invertible matrix, and outputs an object which is a list
## with 4 elements: set, setInverse, get, and getInverse 
## These elements serve as the input instructions for the cacheSolve function
## By default, the inverse 
# Each item in the list is a set of instructions needed for the cacheSolve function 


# The makeCacheMatrix function creates a list of 4 items
# set: a call to write the original matrix to cache
# get: a call to retrive the original matrix
# setInverse: a call to write a new value for the cached matrix inverse
# getInverse: a call to retrieve cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {  
cachedInverse <- NULL      
set <- function(y) {       
x <<- y                    
cachedInverse <<- NULL
}
get <- function() x
setInverse <- function(inverse) cachedInverse <<- inverse
getInverse <- function() cachedInverse
list(set = set, get = get,
setInverse = setInverse,
getInverse = getInverse)
}


# Using the list created in makeCacheMatrix, the function reads in the cached inverse that corresponds
# with the matrix 'x' identified in makeCacheMatrix 
# If the cached inverse for the matrix is not NULL, then the functions returns the cached value
# with a message to the user
# Otherwise, the solve function will be called to determine the matrix inverse, which will 
# then overwrite the cached inverse

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
invFunc <- x$getInverse()    # Read in the inverse matrix stored in the cache
if(!is.null(invFunc)) {      # Check to see if inverse matrix is non-blank/NULL
message("retrieving cached data")
return(invFunc)              # return the cached matrix inverse without invoking solve function
}
data <- x$get()              # Read in the input matrix
invFunc <- solve(data, ...)  # Find the inverse using the solve function
x$setInverse(invFunc)        # Write the inverse to the cache
invFunc                      # print the inverse function
}
