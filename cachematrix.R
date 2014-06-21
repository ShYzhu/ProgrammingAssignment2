## This R file calculats the inverse of an input
## matrix (We assume the input is invertible). 

## When the matrix is new, its inverse is calculated;
## the inverse and the matrix are also stored 'globally'
## such that the inverse can be looked up in the cache.
## When the data has been calculated, the calculation
## process is skiped and its inverse is obtained from 
## the cache.


## The first function, makeCacheMatrix creates a special 
## "vector", which is really a list containing a function to
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse
##  4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## The following function calculates the inverse of the 
## special "vector" created with the above function. 
## However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the setmean function.

cacheSolve <- function(x, ...) {        
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
