## Together, 'makeCacheMatrix' and 'cacheSolve' create a matrix and return
## the inverse of the matrix

## 'makeCacheMatrix' builds a set of functions which are returned in a list 
makeCacheMatrix <- function(inmatx = matrix()) {
      inv <- NULL   ## Initialize inverse matrix
      
      ## Define the 'set' function
      set <- function(inmaty) {
            inmatx <<- inmaty
            inv <<- NULL
      }
      
      ## Define the 'get' function
      get <- function() inmatx
      
      ## Define the 'setinvrs' function
      setinvrs <- function(solve) inv <<- solve
      
      ## Define the 'getinvrs' function
      getinvrs <- function() inv
      
      ## Assign each function as an element in a list
      list(set = set, get = get,
           setinvrs = setinvrs,
           getinvrs = getinvrs)
}

## 'cacheSolve' takes the list created by 'makeCacheMatrix' and either
## calculates the inverse of the matrix or, if the inverse has alrady been
## calculated, simply returns the inverse
cacheSolve <- function(inmatx, ...) {
      
      ## Retrieve inverse of matrix (if it exists)
      inv <- inmatx$getinvrs()
      
      ## Check to see if inverse of matrix exists, and return it if it does
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      ## If inverse doesn't exist, calculate it
      data <- inmatx$get()
      inv <- solve(data, ...)
      inmatx$setinvrs(inv)
      inv
}
