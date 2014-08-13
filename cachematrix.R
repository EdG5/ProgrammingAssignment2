## The following functions take a matrix, invert it and return the inverted matrix
## If the matrix has already been inverted, the functions return a stored copy...
## of the inverted matrix, saving time.

## makeCacheMatrix is a 'container' for storing a matrix ...
## and the inverted form of that matrix.

## It contains functions for setting and returning:

## 1. the matrix passed in
## 2. a copy of the matrix used when inverting.
## 3. the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  
  cache.matrix <- matrix()
  inverted.matrix <- NULL
  
  get.mat <- function() x
  set.mat <- function(mat.in) x <<- mat.in
  
  get.c.mat <- function() cache.matrix
  set.c.mat <- function(mat.in) cache.matrix <<- mat.in
  
  get.i.mat <- function() inverted.matrix
  set.i.mat <- function(mat.in) inverted.matrix <<- mat.in
  
  list(
    get.mat = get.mat,
    set.mat = set.mat,
    
    get.c.mat = get.c.mat,
    set.c.mat = set.c.mat,
    
    get.i.mat = get.i.mat,
    set.i.mat = set.i.mat
  )
}


## cacheSolve acts on a 'makeCacheMatrix' object.
## If not previously done, cacheSolve inverts the matrix it receives,
## and stores it in the makeCacheMatrix object it is working on.
## If cacheSolve finds an inverted matrix and the matrix passed to it ...
## is identical to the one cached at the time of inversion ...
## it returns the cached, inverted matrix saving computation time.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv.mat <- x$get.i.mat()
  
  if(!is.null(inv.mat) && identical(x$get.mat(), x$get.c.mat())){
    print("Getting cached matrix")
    inv.mat    
  }
  
  data <- x$get.mat()
  x$set.c.mat(data)
  inv.mat <- solve(data)
  x$set.i.mat(inv.mat)
  inv.mat
}

# Testing -----------------------------------------------------------------

# # The following code can be un-commented and run for testing / review
# 
# # Test matrices
# test.mat1 <- matrix(
#   data = c(1, 5, 6, 2, 4, 3, 5, 6, 7),
#   nrow = 3,
#   ncol = 3
# )
# 
# test.mat2 <- matrix(
#   data = c(2, 7, 3, 4, 8, 1, 3, 5, 9),
#   nrow = 3,
#   ncol = 3
# )
# 
# # Create test matrix object
# test1 <- makeCacheMatrix(test.mat1)
# # Invert the matrix
# cacheSolve(test1)
# # Return the cached inverted matrix
# cacheSolve(test1)
# # Update the 'x' matrix in the object
# test1$set.mat(test.mat2)
# # Cached matrix no longer identical to 'x' matrix
# # New matrix is inverted
# cacheSolve(test1)
# # Return the cached inverted matrix
# cacheSolve(test1)
