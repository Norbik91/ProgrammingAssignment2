## Put comments here that give an overall description of what your
## functions do

# Write a short comment describing this function
# makeCacheMatrix creates a special "matrix" object, which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	invm <- matrix()
	set <- function (y) {
		x <<- y
		invm <<- matrix()
	}
	get <- function() x
	set.invm <- function(inverse) invm <<- inverse
	get.invm <- function() invm
	list(set = set, get = get, 
	     set.invm = set.invm,
	     get.invm = get.invm)
}


## Write a short comment describing this function

## Input: object from makeCacheMatrix() function
## Check if the inverted matrix already exists in cache. 
## If so, get this matrix from cache with appropriate message.
## Otherwise, compute inverse with solve() function. 
## Output: inversion of the initial matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invm <- x$get.invm()
	if(!is.na(invm[1, 1])) {
		message("getting cached data")
		return(invm)
	}
	data <- x$get()
	invm <- solve(data, ...)
	x$set.invm(invm)
	invm
}


