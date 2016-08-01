## Two functions are defined below. The first makeCacheMatrix, creates a 
## special 'matrix' object which is actually al ist containing functions
## get, set, setinverse and get inverse, which repectively retrieve the matrix
## values, set the matrix values, set the inverse of the matrix, and retrieve
## the inverse of the matrix.
## The second function cacheSolve(), takes this special 'matrix' object as 
## input, and returns it's inverse. If the inverse already exists in memory
## it will return the cached value, otherwise the inverse is computed using
## solve().

## the makeCacheMatrix() function creates a special 'matrix' object which
## is a list of functions to get the matrix values, set the matrix values, 
## set the inverse of the matrix, and return the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  mat.inverse <- NULL
  set <- function(y) {
    x <<- y
    mat.inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) mat.inverse <<- inv
  getinverse <- function() mat.inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function takes the special 'matrix' object created above
## and returns the inverse. If the inverse already exists it will return 
## the cached value from memory, otherwise it is computed using solve()

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
      
  mat.inverse <- x$getinverse()
  # Check if inverse exists already, and if so, return from memory.
  if(!is.null(mat.inverse)) {
    message("getting cached data")
    return(mat.inverse)
  }
  # If inverse does not exist, compute using solve() and cache using 
  # setinverse() function
  data <- x$get()
  mat.inverse <- solve(data, ...)
  x$setinverse(mat.inverse)
  mat.inverse
}
