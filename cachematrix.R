
## it is almost same as the example given in the class
## note I have changed the variable m to im and also
##changed the get/set function names for getting inverse

#The function makeCacheMatrix can be used to save a matrix
#and its inverse first time and return it if needed later

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinv <- function(inv) im <<- inv
  getinv <- function() im
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function tries to get the inverse of the given matrix 
## if it exists in the cache uses it or else creates it
## and returns that

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getinv()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data)
  x$setinv(im)
  im
}


##tested witht the following matrix and steps


#some sample matrix that are invertible

#mx <- matrix(c(3,-5,-2,3),2,2,byrow=T)
#mx
#mx1 <- matrix(c(2,7,1,4),2,2,byrow=T)
#mx1
#mx2 <- matrix(c(-1,-3,1,3,6,0,1,0,1),3,3,byrow=T)
#mx2

##cache and check

#y <- makeCacheMatrix(mx)
#y
#attributes(y)
#n <- y$set(mx)
#print(y$get())
#r <- y$setinv(solve(mx))
#print(y$getinv())

#cy <- cacheSolve(y)
#cy
#print(cy)

##non cached case

#y <- makeCacheMatrix(mx1)
#y
#attributes(y)
#n <- y$set(mx1)
#print(y$get())

#cy <- cacheSolve(y)
#cy
#print(cy)



