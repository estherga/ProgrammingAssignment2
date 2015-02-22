# Functions MakeCacheMatrix and CacheSolve are used to compute
# the inverse of a matrix. MakeCacheMatrix returns
# a special matrix object able to cache its inverse. 
# cacheSolve returns the inverse of a matrix created by 
# the function makeCacheMatrix.If the inverse has not already  
# been calculated, it is computed with the solve function
# and set in the cache. If it has already been computed, 
# then cacheSolve retrieves the inverse from the cache.

# The function MakeCacheMatrix returns a special matrix
# able to cache its inverse.
#
# Args: 
#   x: Invertible matrix
#
# Returns: 
#   A special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    # Initialization of the inverse
    i <- NULL
    
    # Function to set the value of the matrix x. 
    # It resets the value of the inverse.
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # Function to get the value of the matrix
    get <- function() x
    
    # Function to set the inverse of the matrix
    setinverse <- function(inverse) i <<- inverse
    
    # Function to get the inverse of the matrix
    getinverse <- function() i
    
    # A list containing functions set, get, setinverse 
    # and getinverse is returned
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# The function cacheSolve returns the inverse of a matrix 
# created by the function makeCacheMatrix.
#
# Args: 
#   x: matrix created using the makeCacheMatrix function
#   ...: additional arguments that will be passed to the 
#        solve function 
#
# Returns: 
#   The inverse of a matrix created by the function makeCacheMatrix. 
#   If the inverse has already been calculated (and the matrix 
#   has not changed), then cacheSolve retrieves the inverse from 
#   the cache. If not, the inverse is computed with the solve function.

cacheSolve <- function(x, ...) {
    # We get the inverse of the matrix from the cache
    i <- x$getinverse()
    
    # We check if the inverse has already been calculated
    if(!is.null(i)) {
        message("getting cached data")
        
        # The cached value is returned
        return(i)
    }
    
    # We get the value of the matrix
    data <- x$get()
    
    # The inverse of the matrix is computed with the solve function
    i <- solve(a = data, b = diag(nrow(data)), ...)
    
    # The value of the inverse is set in the cache with the setinverse function 
    x$setinverse(i)
    
    i
}
