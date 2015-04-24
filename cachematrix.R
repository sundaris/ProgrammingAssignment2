##Cache Inverse Matrix

## makeCacheMatrix takes the input as matrix and returns the list of function such as set, get and set inverse and get inverse matrices
makeCacheMatrix <- function(x = matrix()) 
{
	inversematrix <- NULL
	## This function will set the matrix
	set <- function(y) 
	{
		x <<- y
		inversematrix <<- NULL
	}
	## This function return the matrix
	get <- function() x
	## This function set the inverse of a matrix
	setinverse <- function(invmatrix) inversematrix <<- invmatrix
	
	## This function returns the inverse of a matrix
    getinverse <- function() inversematrix
	## Return the list which contains the functions
	list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
 }

 ## This function returns the inverse of a square matrix. It will first search in the cache and if not found then it will compute the inverse
 cacheSolve <- function(x,...) 
 {
	 inverm <- x$getinverse()
	 if(!is.null(inverm)) 
	 {
		 message("getting inverse cached data")
		 return(inverm)
	 }
	datam <- x$get()
	invx <- solve(datam)
	x$setinverse(invx)
	invx
 }
