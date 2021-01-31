## These functions are meant to store the already calculated data
## instead of throwing it in the garbage after a function has run
## to use the result if it is required later on.

## The first functions generates a function which returns a list with the 
## elements used (set, get, setsolve, setsolve), this is meant to allow as to  
## use these elements, and the code of the function in another function to be 
## able to retrieve the results with the "get" functions and mutate the result
## with the "set" function. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## The following function is meant to be used with an object that was generated
## using the previous function, to be able to retrieve the code from the 
## previous function to use cache information (results that are stored to avoid 
## doing recalculations), retrieving and mutating them in the response if it had
## been already calculated. In the it had not been calculated, then it would 
## just run the calculation. 

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
  }      
  ## Return a matrix that is the inverse of 'x'
