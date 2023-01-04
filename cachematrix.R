## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

cacheSolve <- function(x, ...) {
  answer <- x$getinverse()
  if(!is.null(answer)){  #check is was calculated or not?
    message("getting cached data")
    return(answer)
  }
  data <- x$get()
  minor <- function(m = data,i = numeric(),j = numeric()){
    min <- m[-i,-j]  #cut row "i" and colum "j"
    min  #find Minor matrix //matrix
  }
  deter <- function(d = matrix()){
    if(!is.matrix(d)){
      det <- d
      det # return that value  //numeric
    }else{
      rowst <- 1:col(d) #first row of data
      det <- sum(sapply(1:ncol(d),function(j) deter(minor(d,1,j))*d[1,j]*((-1)^(1+j))))
      # sum of 1st row in matrix { value(i,j)*((-1)^(i+j))*det(minor(i,j))}
      det  #find determinant of matrix //numeric
    }
    
  }
  cofactor <- function(c = numeric(),dt = data){  
    i <- which(data==c)%%nrow(dt)  # position of value in matrix (row)
    if(i==0){ 
      i <- nrow(dt)  # if it at last row in matrix
    }
    j <- ceiling(which(dt==c)/nrow(dt))  # position of value in matrix (column)
    cof <- ((-1)^(i+j))*deter(minor(dt,i,j))
    
    cof  #find cofactor in each position  //numeric
  }
  adjoint <- function(adj=data){
    adj <- matrix(sapply(data, cofactor),nrow = nrow(data),byrow = TRUE)
      # list to matrix by row -(transpose cofactor matrix by find cofactor at each position then make them to be list)
      # adjoint of matrix  //matrix
    adj
  }
  if(nrow(data)==ncol(data)){ #check it is square matrix or not?
    
    inverse <-(1/deter(data))*adjoint(data)
      # inverse = adjoint matrix / determinant of matrix
      # inverse matrix  //matrix
    ## Return a matrix that is the inverse of 'x'
    x$setinverse(inverse)  # set value inverse matrix
    x$getinverse()  #return inverse matrix
    
  }else{
    print("Cannot find it's inverse matrix")
  }
  
}
