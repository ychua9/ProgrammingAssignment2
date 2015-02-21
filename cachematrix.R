## The first function, makeCacheMatrix creates a special "vector", 
## which is a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverted matrix
## 4. get the value of the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function, cacheSolve calculates the inversion of the 
## special "vector" created with the above function. However, it first 
## checks to see if the inversion has already been calculated.
## If so, it gets the inverted matrix from the cache and skips the computation.
## Otherwise, it calculates the inverted matrix and sets the inverted matrix 
## in the cache via the setinverse function.
cacheSolve <- function(x, new_mat = NULL, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## check if a new matrix have been defined
  if(!is.null(new_mat)) {
    x$set(new_mat)
  }
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

