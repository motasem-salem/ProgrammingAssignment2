
# This function creates a special object type that holds a matrix amnd its inverse and provides getters and setters for these values

makeCacheMatrix <- function(x = matrix()) {
  
  # the inverse, initially NULL
  inv <- NULL
  
  # the matrix setter function, sets the matrix and sets NULL as its inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # the matrix getter function
  get <- function() x
  
  # the inverse setter function. Given an value, it assigns it to the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # the inverse getter function
  getinverse <- function() inv
  
  # return a list of the four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function takes a special matrix object (as defined above) as input and returns its inverse

cacheSolve <- function(x, ...) {
        
  # get the inverse of the input special matrix. Possibly NULL if it has not been computed and cached yet.
  inv <- x$getinverse()
  
  # if we did not get NULL back, that means the inverse was computed earlier, so we just return it.
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  # otherwise, let's get the matrix, compute its inverse, cache the inverse value and then return it
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinverse(inv)
  inv
}
