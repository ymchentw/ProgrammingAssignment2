## Put comments here that give an overall description of what your
## functions do

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	    im <- NULL
	    set <- function(y) {
	        x <<- y
	        im <<- NULL
	    }
	    get <- function() x
	    setim <- function(inverseM) im <<- inverseM
				        
	    getim <- function() im
					        
	    list(set = set, get = get,
        	 setim = setim,
	         getim = getim)
} 

## computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {

	im <- x$getim()
	# try to  retrieve the inverse from the cache
	    
	if(!is.null(im)) {
	# If the inverse has already been calculated 
		      
       		message("getting cached data")
		  return(im)
	}
					      
	data <- x$get()
									
	im <- solve(data)
	# calculate the inverse
							          
	x$setim(im)

	im
        ## Return a matrix that is the inverse of 'x'
}

