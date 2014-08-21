## These functions create a cahe of a matrix inverse. 
##They assume the input matrix x is invertible.

## This first function makes the vector that will set and cache the matrix.
##The input argument is x, and invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
  ## Set local variables
  iofx <- matrix(NA)
  
  ##Define set function
  set <- function(y) {
    x <<- y
    iofx <<- matrix(NA)
  }
  ##Get function to get the matrix x
  get <- function() x
  ##Define setinverse function to create inverse of x
  setinverse <- function(iofx) i <<- iofx
  ## Define getinverse function, which simply returns inverse of x is it exists
  getinverse <- function() i
  ##combine functions into list and return them
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


##This function returns the inverse of the matrix.
##The input argument is the list created by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
  browser()
## Return a matrix that is the inverse of 'x'
  iofx <- x$getinverse()
  if(!is.na(iofx[1,1])) {
    message("getting cached data")
    return(iofx)
  }
  data <- x$get()
  iofx <- solve(data)
  x$setinverse(iofx)
  iofx
}
