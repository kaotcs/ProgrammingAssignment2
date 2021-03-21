## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve #set the solve function
  getsolve <- function() m #save the inverse, after calculated
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)#list where every attributes are saved as a list
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve() #verify if this inverse has been calculated
  if(!is.null(m)) { #if it is not null, show "getting cached data" with the inverse
    message("getting cached data")
    return(m) #print the inverse matrix in cache
  }
  data <- x$get() #get the matrix and store into data
  m <- solve(data, ...) #m gets the inverse matrix
  x$setsolve(m) #x receives the inverse of matrix, if the process need to be showed again
  m #print the inverse matrix
  }
