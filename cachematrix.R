# This file contains two functions for calculating and caching
# the inverse of a matrix.
# The first function creates a list of useful functions,
# the second one calculates the inverse or gets the result
# from cache.

# The first funcion creates a list containing the value of
# the matrix, and four additional functions:
# - two for setting and getting the value of the matrix
# - two for setting and getting the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    # When the function is called for a new matrix,
    # create a container for the inverse matrix
    inverse <- NULL
    
    # When a new value for the matrix is set, erase
    # the inverse
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # Simply get the value of the matrix
    get <- function() x
    
    # Set and get the value of the inverse matrix
    setinverse <- function(z) inverse <<- z
    getinverse <- function() inverse
    
    # Return the list of the functions
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


# The second function calculates the inverse of the matrix
# or gets the result from cache

cacheSolve <- function(x, ...) {
    
    # Check if the inverse has already been calculated:
    # read the value from the list,
    inverse <- x$getinverse()
    # return the value of the inverse if it is cached.
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # If the inverse hasn't been calculated yet,
    # get the value of the matrix,
    data <- x$get()
    # calculate the inverse for this matrix,
    inverse <- solve(data, ...)
    # cache this value into the list for future use.
    x$setinverse(inverse)
    
    # Return the value of the inverse matrix.
    inverse
}

# Steps to calculate the inverse of a matrix (e.g. your_matrix)
# using these two functions.
# 1. Use makeCacheMatrix on your_matrix, assign to a variable:
# > x <- makeCacheMatrix(your_matrix)
# You will get an R object that contains a list of available
# functions.
# 2. After this, whenever you need the inverse of your_matrix,
# use cacheSolve on the new variable (x):
# > cacheSolve(x)
# You will get the inverse of your_matrix, either
# calculated anew (first call) or loaded from cache.
