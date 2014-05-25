## This function takes as an input a matrix and converts this matrix
## into an object that contains the input matrix and a list of functions
## that set/get the values of the matrix (with functions "set()" and "get()") 
## and set/get the inverse of the matrix (with functions "setinverse()" and
## "getinverse()").
## 
## Setting the inverse of the input matrix inside this object would cache
## inverse so that whenever the inverse is needed, it does not need to be
## re-computer. Instead, the stored or the cached inverse data can be 
## retrieved with the getinverse() function defined in the object.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) inv <<- solve
	getinverse <- function() inv
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## This function takes in an object created by the function "makeCacheMatrix()"
## and computes the inverse of the matrix contained in this object.
##
## Before the function  does the inverse computation, it checks to see whether
## the inverse of the matrix has been set and cached within the object.
## If the matrix inverse has been set, the function will return these cached
## data instead.
##
## However, if the inverse of the matrix has not been computed, and hence, cached
## within the object, the function will compute the inverse of the matrix,
## set the inverse of the matrix within the object, and return the inverse
## of the matrix.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
