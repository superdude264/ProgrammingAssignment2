## Functions that allow caching the inverse of a matrix for later use.

## Create a structure to hold a matrix & it's (uncalculated) inverse.  The inverse may be calculated with 'cacheSolve()'
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   
   set <- function(input) {
      x <<- input
      inv <<- NULL
   }
   
   get <- function() {
      x
   }
   
   setinv <- function(inv) {
      inv <<- inv
   }
   
   getinv <- function() {
      inv
   }
   
   list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Will cache the inverse of matricies created with 'makeCacheMatrix()' and utilize cached entries of the inverse is asked for again.
cacheSolve <- function(x, ...) {
   inv <- x$getinv()
   if(!is.null(inv)) {
      message("Retrieving inverse from cache.")
      return(inv)
   }
   
   matrix <- x$get()
   inv <- solve(matrix)
   x$setinv(inv)
   inv
}
