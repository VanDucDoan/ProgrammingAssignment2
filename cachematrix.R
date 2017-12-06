## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  set <- function(y){
    x <<- y
    invMat <<- NULL
  }
  get <- function() x
  setInvMat <- function(invMatInp) invMat <<- invMatInp
  getInvMat <- function() invMat
  list(set = set, get = get,
       setInvMat = setInvMat,
       getInvMat = getInvMat)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMat <- x$getInvMat()
  if (!is.null(invMat)){
    message("getting cached inverse matrix")
    return(invMat)
  }
  Mat <- x$get()
  invMat <- solve(Mat)
  x$setInvMat(invMat)
  return(invMat)
}

# Example
# Make a special matrix
speMat <- makeCacheMatrix()
Mat <- matrix(data = rnorm(10000), nrow = 100, ncol = 100, byrow = TRUE)
speMat$set(Mat)

# Calculate the inverse matrix
invMat <- cacheSolve(speMat)

# Check the accuracy
print(norm(diag(100) - Mat %*% speMat$getInvMat(), "F"))

# In consolse, call source("cachematrix.R")