makeCacheMatrix <- function(x = matrix()) {
  #setting inv for use in the function below, it's empty now
  inv <- NULL
  #creating the first function in the list to be, this'll take the matrix in
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # second function
  get <- function() x
  # third function which will inverse the matrix created above
  setsolve <- function(inverse) inv <<- inverse
  #and the last bit, to get the inverse out again
  getsolve <- function() inv
  #listing it
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
  #checking if inv is already filled 
  inv <- x$getsolve()
  #if it is return what is stored in 'inv'
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #if not fill it
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
}

