## The assignment is to write a pair of functions that cache the inverse of a matrix

## The function "makeCacheMatrix" create a list of functions that can cache the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
	set <- function(y) 
	{
	        x <<- y
	        m <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) m <<-inverse
	getInverse <- function() m
  	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}

## The function "cacheSolve" computes the inverse of a matrix returned by the previous function "makeCacheMatrix",
## unless the inverse has already been calculated, in which case it retrieves it from the cache      

cacheSolve <- function(x, ...) 
{
        m <- x$getInverse()
    	if ( ! is.null(m)) 
	{
	        print("getting cached data")
		return(m)
	}
	m <- solve(x$get())
	x$setInverse(m)
	m
}
