## Below is a pair of functions which enable the caching of the inverse of a matrix.

## makeCacheMatrix takes in a matrix and assigns it to the parent environment so that it call be
## retrived by cacheSolve. It also returns a list of getters and setters functions
## including getters and setters for the inverse of the matrix.
## the inverse of the matrix is also assigned to the parent environement to be made avaiable
## to be retrieved in cacheSolve without requiring it to be computed in cacheSolve.



makeCacheMatrix <- function(x = matrix()){
    inverse <- NULL #set the inverse of the matrix to NULL : no inverse will be cached for a new matrix.
    set <- function(y = matrix(as.numeric(y), nrow, ncol, byrow = FALSE)) { #require a matrix
    x <<- y #assign to parent environment
    inverse <<- NULL #reset the inverse to NULL : no inverse will be  in the cache for a new matrix.
    
  }
  
    get <- function() x
    setInverse <- function(solve) inverse <<- solve #assign inversed matrix to parent environment
    getInverse <- function() inverse
    list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##cacheSolve returns the inverse of the matrix passed. It attempts to first retrieve it from the
##cache and return it without computing it. If no inverse in the cache is available, it attempts
## to compute it and return it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    print("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}

