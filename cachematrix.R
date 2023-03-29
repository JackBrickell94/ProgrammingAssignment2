## The following functions are designed to cache the inverse of a Matrix:
## Matrix inversion is a costly computation and there is benefit to caching the inverse of a matrix
## The functions below create a special object that storesd a matrix and caches its inverse.

## This function creates a special "matrix" object:

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" generated above by 
## makeCacheMatrix above. Providing the inverse has been calculated (and the matrix has not changed,
## it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
