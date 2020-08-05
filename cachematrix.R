## These functions take a 2x2 matrix, calculate its inverse and cache it 
## for retrieval.

## This function creates a list of commands for the second function to use upon 
## solving for or retrieving the inverse of the original matrix.

makeCacheMatrix <- function(x = matrix()) {
      invers <- NULL
      set <- function(y) {
            x <<- y
            invers <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) invers <<- inverse
      getinverse <- function() invers
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function either retrieves the inverse of the matrix or calculates it and 
## then prints it.

cacheSolve <- function(x, ...) {
      invers <- x$getinverse()
      if(!is.null(invers)) {
            message("getting cached data")
            return(invers)
      }
      data <- x$get()
      invers <- solve(data, ...)
      x$setinverse(invers)
      invers
}