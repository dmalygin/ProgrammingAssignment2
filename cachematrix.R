## The function is aimed to inverse a matrix and place the result of the calculation in the cache. 
## If the cache already has the result of inversing, the function just returns the result without calculation.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  #set the value of the input matrix
  set <- function(y) { 
    x <<- y
    m <<- NULL
  }
  
  #get the value of the matrix
  get <- function() {
    x 
  }
  
  #set the value of the matrix's inverse
  setInverse <- function(inverse) {
    m <<- inverse 
  }
  
  #get the value of the matrix's inverse
  getInverse <- function() {
    m 
  }
  
  #save the inverted matrix
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## inverse returned from cache

cacheSolve <- function(x, ...) {
  
  #return inversed version of the matrix
  inversed <- x$getInverse()
  
  if(!is.null(inversed)) {
    message("getting cached data")
    return(inversed)
  }
  
  #get the inverse from the cache
  data <- x$get()
  
  #set the inversed matrix
  inversed <- solve(data, ...)
  
  #set the inversed matrix from the cache
  x$setInverse(inversed)
  
  #return inversed matrix
  inversed
}

