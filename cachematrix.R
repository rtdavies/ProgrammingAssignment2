## A utility for repeatedly accessing inverted matrices.
## Computing a matrix's inverse is potentially expensive, so
## this utility is optimized to avoid repeating the
## computation by caching the results.

## Example:
## > cm <- makeCacheMatrix(m) # construct an invertable matrix
## > cacheSolve(cm)           # computes and returns the inverse of m
## > cacheSolve(cm)           # returns the inverse of m, avoiding recomputation
## > cm$set(m2)               # changes the matrix, and clears the cache
## > cacheSolve(cm)           # computes and returns the inverse of m2

## Construct a matrix that can be inversed multiple times. 
makeCacheMatrix <- function(original = matrix()) {
    cached_inverse <- NULL
    
    # overrides the original matrix and clears the cached inverse
    set <- function(replacement) {
        original <<- replacement
        cached_inverse <<- NULL
    }
    
    get <- function() original
    setInverse <- function(inverse) cached_inverse <<- inverse
    getInverse <- function() cached_inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Efficiently access the inverse of a matrix constructed with makeCacheMatrix
cacheSolve <- function(x, ...) {
    cached <- x$getInverse()
    
    if(!is.null(cached)) {
        message("cached:")
        return(cached)
    }
    
    # else calculate, set and return inverse
    i <- solve(x$get(), ...)
    x$setInverse(i)
    i
}
