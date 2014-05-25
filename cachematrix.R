## Programming Assignment 2 - R Programming
## Scott McClure (scottmcclure68)
## This program consists of two functions, makeCacheMatrix and cacheSolve, which cache the inverse
## a given matrix, thus improving performance when large matrices are inverted.  The purpose of the assignment
## is to provide an example of two functions which use lexical scoping to pass object values between them.

## makeCacheMatrix - This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      # if the function is called but no references are made to any subfunctions, set m to NULL
      m <- NULL
      set <- function(y) {
            # super assignment operator (<<-) allows x and m variables to be altered in parent environment
            x <<- y     
            m <<- NULL
      }
      get <- function() x   # grab the matrix stored in x and return it
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve - This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
##              If the inverse has already been calculated (and the matrix has not changed), then
##              cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {  # check to see if the returned cache has anything in it - if so, get the data
            message("Getting cached data...")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)  # return the inverse of the matrix
      x$setinverse(m)
      m
}
