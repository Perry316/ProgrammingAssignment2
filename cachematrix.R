## Put comments here that give an overall description of what your
## functions do
## These functions caculate and cache the inverse of a matrix.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## SO first, the function initialize two objects, namely x and inv.Then the code provides 
## four basic behaviors called "getters and setters".Finally, it creates a new object by returning a list.
## The super-assignment operator <<- aasigns the value on the right side of the operator to an object in the parent environment named by
## the object on the left side of the operator.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.This function starts with a single argument x and an ellipis allowing the caller to pass additional arguments into 
## the function.Next, it attemps to retrieve the inverse from the object passed in as the argument. If there is already a result, 
## it will return this value, or it will get the matrix from the input object and caculate its inverse.
## These functions address the importance of lexical scoping, which allows subsequent code can access the values of objects through the
## getters and setters.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting the cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
