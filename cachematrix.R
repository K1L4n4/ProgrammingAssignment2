## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	my_inverse <- NULL
	  set <- function(y) {
	    x <<- y
	    my_inverse <<- NULL
}
	get <- function() x
	  setinverse <- function(inverse) my_inverse <<- inverse
	  getinverse <- function() my_inverse
	  list(set = set, get = get,
	       setinverse = setinverse,
	       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not ## changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	my_inverse <- x$getinverse()
	  if(!is.null(my_inverse)) {
	    message("getting cached data")
	    return(my_inverse)
	  }
	  data <- x$get()
	  my_inverse <- solve(data, ...)
	  x$setinverse(my_inverse)
	  my_inverse
}
