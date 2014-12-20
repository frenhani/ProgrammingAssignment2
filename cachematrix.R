## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix() defines a special element with 4 functions as described:
## set(): stores the source matrix that will be inverted
## get(): returns the source matrix
## setinverse(): stores the reversed matrix of source matrix
## getinverse(): returns the reversed matrix

## These functions are called by cacheSolve() to determine whether
## cached data must be returned or matrix inversion must be computed.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function (inverse) inv <<- inverse
	getinverse <- function() inv

	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve() returns the inverse of special matrix given by parameter 'x'.
## First checks for cached data in case the inverse has been previously calculated.
## If not, computes the matrix inverse and stores through setinverse()

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
  
    if (!is.null(inv))
    {
      ## original matrix hasn't changed and there's cached data
      message("getting cached data")
    }
    else {
      ## no cached data. Computes inversion
      message("computing matrix inversion")
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)      
    }

    inv
}
