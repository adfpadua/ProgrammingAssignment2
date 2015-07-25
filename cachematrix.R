## Put comments here that give an overall description of what your
## functions do

# This general function caches the value of the inverted matrix so that 
# when we need it again, it can be looked up in the cache 
# rather than recomputed.

## Write a short comment describing this function
# The first function, makeCacheMatrix creates a special "matrix", which is 
# really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the solved matrix
# get the value of the solved matrix

makeCacheMatrix <- function(x = matrix()) {

        cache_m <- NULL
        set <- function(y) {
                x <<- y
                cache_m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) cache_m <<- solve
        getsolve <- function() cache_m
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
        
}


## Write a short comment describing this function
# The following function cacheSolve calculates the inverse of the 
# special "matrix" created with the above function. However, it first checks 
# to see if the matrix has already been solved. If so, it gets the inverted 
# matrix from the cache and skips the computation. Otherwise, it calculates the 
# invertion of the data and sets the value of the inverted matrix in the cache 
# via the setsolve function.
 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        cache_m <- x$getsolve()
        if(!is.null(cache_m)) {
                message("Attention! getting cached data")
                return(cache_m)
        }
        data <- x$get()
        cache_m <- solve(data, ...)
        x$setsolve(cache_m)
        cache_m
        
        }
