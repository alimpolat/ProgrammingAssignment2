## Caching the Inverse of a Matrix:
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## set the value of the matrix
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## get the value of the matrix
        get <- function() x
        
        ## set the inverse of the matrix
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## get the inverse of the matrix 
        inv <- x$getInverse()
        
        ## check if there is the matrix
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## if not: get the inverse of the matrix
        mat <- x$get()
        inv <- solve(mat, ...)
        ## set the inverse of the matrix
        x$setInverse(inv)
        inv
}
