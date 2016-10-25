## Put comments here that give an overall description of what your
## functions do

#The function makeCacheMatrix will set a value to a matrix, will get the value of a matrix,
#will set the inverse of a matrix and return the inverse of a matrix

makeCacheMatrix <- function(m = matrix()) {
  x <- NULL
  set <- function(y) {
    m <<- y
    x <<- NULL
  }
  get <- function() m
  setinverse <- function(solve) x <<- inverse
  getinverse <- function() x
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


#The function cacheSolve initially checks if an inverse matrix, i, has already been created. 
#If i exists it will return the message "getting cached data" and the cached matrix. 
#If not, it will inverse the matrix and will set the inverse in the cache

cacheSolve <- function(m, ...) {
  i <- m$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- m$get()
  i <- solve(data)
  m$setinverse(i)
  i
}
