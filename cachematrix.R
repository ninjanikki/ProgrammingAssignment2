## This function creates a special "matrix" object that can cache its inverse.
## In this method, matrix is saved as x and its inverse as m in an environment 
## that is different from current envirionment
makeCacheMatrix <- function(x = matrix()) {
  #set variable m as NULL so when searching for m, we get the m value that is
  #saved in the environment that is not the current environment
  m <- NULL
  #set sets the matrix to be the matrix that is passed in to the function
  #when x is reset, we need to reset the inverse value m to NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  #get retrieves the matrix
  get <- function() {
    x
  }
  #setsolve sets the inverse of the matrix as m
  setsolve <- function(solve)
  {
    m <<- solve
  }
  #getsolve retrieves the inverse of the matrix m
  getsolve <- function() 
  {
    m 
  }
  #create a list that holds the four operations
  list(set = set, 
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function returns a matrix that is the inverse of 'x'
## It first checks to see if an inverse of 'x' has already been 
## computed and cached, if not, it computes the inverse of 'x' then
## save it in the cache for later use
cacheSolve <- function(x, ...) {
  #first try to get the cached inverse of matrix
  m <- x$getsolve()
  #if the value is not null, the is a cached matrix. Print a message and return the value
  if(!is.null(m)){
    message("getting cached data")
    return (m)
  }
  #if the value is null, get the matrix 'x'and call solve on the matrix then save
  #the computed value as m, which is cached and can be retrieved later on
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}
