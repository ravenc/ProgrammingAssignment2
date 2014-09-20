## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      
      ## Sets variable i ("inverse") to NULL
      i <- NULL
     
      ## Sets x to the argument y and sets i to NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      ## Returns the value of x, the argument of makeCacheMatrix
      get <- function() x
     
      ## Sets i in makeCacheMatrix to inverse matrix
      setinverse <- function(inverse) i <<- inverse
     
      ## Returns the value of i from makeCacheMatrix
      getinverse <- function() i
     
      ## Makes a list of functions set, get, setinverse, getinverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
      
      ## Attempts to get the inverse matrix from x (if it has already been calculated  
      ## & cached) & assign it to "i".
      i <- x$getinverse()
      
      ## If i is not NULL (that is, the inverse matrix has already been 
      ## calculated and cached), the given message is displayed and the inverse matrix
      ## is taken from the cache & returned.
      if(!is.null(i)) {      
            message("getting cached data") 
            return(i) 
      }
      
      ## (If i is NULL) Sets data to x, the matrix from MakeCacheMatrix.
      data <- x$get()   
      
      ## Calculates the inverse of the matrix and assigns it to i.
      i <- solve(data, ...)
      
      ## Sets the inverse of the matrix.
      x$setinverse(i)
      
      ## Returns the inverted matrix.
      i
}