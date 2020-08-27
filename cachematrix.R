## The following code finds inverse of a matrix and stores it in cache. 
## If the inverse is already calculated for a specific matrix, it will 
## be read from the cached data directly and not be recalculated.

## This function is responsible for creating a cache for matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function(){
    x
  } 
  
  setinv <- function(i){
    inv <<- i
  }
  
  getinv <- function(){
    inv
  }  
  
  list(set = set, get = get, getinv = getinv, setinv = setinv)
}



## This function will return the inverse of the matrix by either reading from
## cached data or calculating. 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(is.null(inv))
  {
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    return(inv)
  }
  
  message("Cached inverse")
  inv
}
