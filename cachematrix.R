## Put comments here that give an overall description of what your
## functions do

#This function creates the cacheable matrix to be solved by "cacheSolve" in the other file
makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
  ##displays list upon reaching end of creation
}

## This function computes the inverse of the matrix returned by "makeCacheMatrix" above
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse() 
  if(!is.null(j)){ ##check if inverse already exists , if so returns it from cache
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...) ##calls R's native solve function
  x$setInverse(j)
  j
}
