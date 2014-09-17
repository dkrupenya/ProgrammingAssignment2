## These functions perform inversion of a matrix 
## and store the result of the operation in closure.
## If the matrix is alredy in closure cache, chached result
## will be return instead of computations repetition.

## usage: cachedMatrix <- makeCacheMatrix(someMatrix)
##        inverseMatrinx <- cacheSolve(cachedMatrix)


## Organize Closure to store cache and return list of functions to operate with it
makeCacheMatrix <- function(x = matrix()) {
  inverese <- NULL
  set <- function(y) {
    x <<- y
    inverese <<- NULL
  }
  get <- function() x
  setInverese <- function(inv) inverese <<- inv
  getInverese <- function() inverese
  list(set = set, get = get,
       setInverese = setInverese,
       getInverese = getInverese) 
  
  
}

## If x doesn't contain inverse matrix - calculate and returen inverse matrix
## else return inverse matrix from cache

cacheSolve <- function(x) {
  
  ## check cache
  inv <- x$getInverese()
  if(!is.null(inv)) {
    message("getting inverted matrix from cache")
    return(inv)
  }
  ## store inverse matrix in X
  y <- x$get()
  inv <- solve(y)
  x$setInverese(inv)

  ## Return a matrix that is the inverse of 'x'
  inv  

}

cachedInverse <- function(x = matrix()) {
  cacheX <- makeCacheMatrix(x)
  cacheSolve(cacheX)
}


## This function will test functionality/ Run it from command line

testCacheSolve <- function() {
  
  # generate random matrix
  m <- matrix(1:4, nrow = 2, ncol = 2)
  
  cm <- makeCacheMatrix(m)
  
  im <- cacheSolve(cm)
  
  message("matrix: ")
  print(m)
  
  message("inverted matrrix: ")
  print(im)
  
  message("check m %*% im = I: ")
  print(m %*% im)
  
  message("Now lets invert the same matrix again.")
  message("You shoild see a message about using cache:")
  
  invAgain <- cacheSolve(cm)
  
}