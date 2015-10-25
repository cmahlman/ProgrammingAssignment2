#######################################################################
# These two functions: makeCacheMatrix() and cacheSolve() work 
# together to create a special object that stores a matrix
# and caches its inverse. 
#
# Example usage:
# 1. source("cachematrix.R")
# 2. mx <- makeCacheMatrix(matrix(c(100,150,200,250), nrow=2, ncol=2))
# 3. cacheSolve(mx)
# 4. mx$getInverse()
# 5. cacheSolve(mx)
#######################################################################


#######################################################################
# This function, makeCacheMatrix(), creates a speical matrix which 
# is really a list containing a function to:
#     1. set the value of the matrix
#     2. get the value of the matrix
#     3. set the value of the inverse of the matrix
#     4. get the value of the inverse of the matrix
#######################################################################
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    setInverse <- function(solve) i <<- solve
    getInverse <- function() i
    
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)

}


#######################################################################
# The following function calculates the inverse of the special
# matrix created from the function makeCacheMatrix(). If the inverse
# has already been calculated, pull from the cache in order to reduce
# costly computation. 
#######################################################################
cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    
    i <- x$getInverse()
    
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()       # get the matrix set by makeCacheMatrix
    i <- solve(data, ...) # compute the inverse of the matrix 
    x$setInverse(i)       # store the inverse
    
    i # return the inverse
}
