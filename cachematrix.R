
library (MASS) # used to calculate inverse for squared and non-squared matrieces
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   #inititilizing inveerse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x # function to get matrix x 
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() {
    
    inver<- ginv(x)
    inver%*%x # function to get inverse of the matrix
  }
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}





# to get cache data
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) { # to check is invers is NULL
    message("getting cached data.")
    return(inv) # to return inverse value
  }
  data <- x$get()
  inv <- solve(data) # calculates inverse value
  x$setinverse(inv)
  inv # returns a matrix that is the inverse of x
}

p <- makeCacheMatrix(matrix(1:8, 2, 4))
p$get()

p$getinverse()
cacheSolve(p)
