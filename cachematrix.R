## The following functions, makeCacheMatrix and cacheSolve allow one
## to leverage the power of scoping to 'cache' the expensive matrix
## operation of calculating the inverse of a matrix. 

## The makeCacheMatrix function creates a special matrix object and 
## caches the inverse of the matrix.  This special matrix object is 
## actually a list of containing functions to set & get the matrix 
## as well as to set & get the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                 ## set the internal matrix object to NULL
  set <- function(y) {                      ## Declare the set function for the matrix object
      x <<- y                               ## Makes a copy of the given y matrix into the x variable scope
      m <<- NULL                            ## clears the internal matrix object which is outside the scope of this function 
  }
  get <- function() x                       ## Declare the get function which returns the x matrix
  setinverse <- function(solve) m <<- solve  ## Declare the setinverse function which solves the matrix and sets the internal matrix object
  getinverse <- function() m                ## Declare the getinverse function which returns the internal matrix object
  list(set = set, get = get,                ## Creates the list object of fuctions to set & get the matrix
       setinverse = setinverse,             ## as well as the setinverse function
       getinverse = getinverse)             ## and the getinverse function
}


## The cacheSolve function checks the cache to determine if a calculated matrix
## inverse is already availalbe.  If so, it returns the cached inverse.  If not, 
## then the matrix inverse is calculated using the 'solve' function and then caches
## the inverted matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()                       ## Obtains, if possible, the inverse of the matrix X and sets the internal matrix object
  if(!is.null(m)) {                         ## Check to see if the cached object indeed exists
    message("getting cached data")          ## If we did, then let the user know that a cached object was available
    return(m)                               ## and return the cached inverse matrix object
  }
  data <- x$get()                           ## If we get here, then the cached object was not found, and we set an internal matrix to solve
  m <- solve(data, ...)                     ## now we solve for the inverse and save it to our internal matrix object
  x$setinverse(m)                           ## Lastly, the solved/inverted matrix is cached 
}
