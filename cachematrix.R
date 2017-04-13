## Put comments here that give an overall description of what your
## functions do

## The two functions below cache the reverse of a matrix
## Matrix inversion is usually a costly computation and there
## may be benefit to caching the inverse of a matrix rather than
## compute it repeatedly

## Write a short comment describing this function
## The makeCacheMatrix function contains a list of 4 functions to
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the inverse value of a matrix
## 4. get the inverse value of a matrix
## 
## Usage: 
## x <- matrix(1:4, nrow=2, ncol=2)
## mcm < makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL 
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inversematrix) inv <<- inversematrix
	getinv <- function() inv
	list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function
## The cacheSolve function computes the inverse of the special matrix 
## returned by makeCacheMatrix. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache
##
## Usage: 
## x <- matrix(1:4, nrow=2, ncol=2)
## mcm < makeCacheMatrix(x)
## cs <- cacheSolve(mcm)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invm <- x$getinv()
	if(!is.null(invm)) {
		message("Getting cached data!")
		return(invm)
	}
	data <- x$get()
	invm <- solve(data)
	x$setinv(invm)
	invm
}
