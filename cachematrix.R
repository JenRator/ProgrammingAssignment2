## The objective of the two functions is to cache the inverse of a matrix 
## by taking advantage of R's lexical scoping rules.

## This first function creates a special "matrix" that can cache its inverse.
## The 'get' functions carry values for the matrix and its inverse. 
##
## By calling each element of the resultant list the respective function 
## is called. Though list elements can be called directly, the 'setinv' 
## should not be set directly.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinv <- function(solve) i <<- solve
      getinv <- function() i
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This second function computes the inverse of the special "matrix" returned
## by makeCacheMatrix. If the inverse has already been calculated (& the inverse
## is the same) then the inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      i <- x$getinv()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)      
      }
      data <- x$get()
      i <- solve(data,...)
      x$setinv(i)
      i
}