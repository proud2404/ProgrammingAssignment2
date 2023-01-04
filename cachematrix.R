## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
 #This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    answer <- NULL
    set <- function(y) {
      x <<- y
      answer <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) answer <<- inverse
    getinverse <- function() answer
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), 
      #then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  answer <- x$getinverse()
  if(!is.null(answer)){  #check is was calculated or not?
    message("getting cached data")
    return(answer)
  }
  data <- x$get()
  inverse <- slove(data)
    ## Return a matrix that is the inverse of 'x'
    x$setinverse(inverse)  # set value inverse matrix
    answer  #return inverse matrix

  
}
