##  makeCacheMatrix creates a special "matrix" object that can cache its inverse.
##  cacheSolve`computes the inverse of the special "matrix" returned by

##  makeCacheMatrix above. If the inverse has already been calculated 
##  (and the matrix has not changed), then `cacheSolve` should retrieve the 
##  inverse from the cache.


## Function to create a special matrix
makeCacheMatrix <- function(x = matrix()) {
  ## create empty matrix
  m <- NULL
  
  set <- function(y) {
    ## make a copy of the input matrix in parent environment
    x <<- y
    ## create empty matrix 'm' in the parent enviroment
    m <<- NULL 
  }
  
  ## Function that returns matrix x
  get <- function() x 
  
  ## set matrix 'm' as the inverse of matrix 'x'
  setinverse <- function(inverse) m <<- inverse 
  
  ## return matrix m
  getinverse <- function() m 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  }

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  ## get matrix m if inverted x already calculate, if not then calculate it.
  m <- x$getinverse()
  if(!is.null(m)) {
    message("inverting matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


