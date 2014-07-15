## This R script contains functions able to calculate matrix inverse
## in a lazily populated manner, so that the result is calculated
## only once and served from cache on subsequent requests


## Creates a matrix wrapper object, which (in addition to the raw matrix)
##  stores matrix inverse (upon first calculation) in internal cache
## :param x: A matrix to inverse (assumed to be inversible!)
## :return: Matrix wrapping object
makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL

    set <- function(m) {
        x <<- m
        cachedInverse <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setInverse <- function(inverse) {
        cachedInverse <<- inverse
    }
    
    getInverse <- function() {
        cachedInverse
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse=getInverse)
}


## Computes and returns inverse of a 'special' cache-aware, invertible matrix x, 
##  by using solve()
## Caches the results in the matrix object, so that subsequent invocations  on 
##  the same object require no additional computations and are served from cache
## :param x: Invertible 'special' matrix, as obtained by 'makeCacheMatrix' call
## :param ...: Additional params to pass as-is to solve() function on first
##             computataion
## :return: Matrix inverse
cacheSolve <- function(x, ...) {
    cachedInverse <- x$getInverse()
    if(!is.null(cachedInverse)) {
        message("Returning cached matrix inverse")
        return(cachedInverse)
    }
    m <- x$get()
    inverse <- solve(a = m, ...)
    x$setInverse(inverse)
    ## Return a matrix that is the inverse of 'x'
    inverse        
}

