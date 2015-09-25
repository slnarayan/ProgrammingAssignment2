## makeCacheMatrix & cacheSolve are pair of functions that cache the inverse of a matrix.
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
## Assuming input is always a square matrix
## isChange attribute will be set to TRUE whenever square matrix is changed
   
  minverse <- NULL
  set <- function(y){
		x <<- y
		isChange <<- TRUE
		minverse <<- NULL
  }
  get <- function() x
  
  ## isChange is initially set to FALSE
  isChange <<- FALSE
  
  getChange <- function() isChange
  
  setinverse <- function( m ) minverse <<- m
  
  getinverse <- function() minverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse,
       getChange = getChange )
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  m <- x$getinverse()
  if(!is.null(m) & !x$getChange()){
		message("getting cached data")
		return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
