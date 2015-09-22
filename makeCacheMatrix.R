makeCacheMatrix <- function(x) {
    # Verifies if the argument x is a matrix            
		if(is.matrix(x)==F) {
			stop("Argument must be a matrix!") }
  
    # Verifies if the matrix is invertible
		if(det(x)==0) {
	   		stop("Non-invertible matrix!") }
	   	
		else {
        		inv = NULL
        		set = function(y) {
                	x <<- y
                	inv <<- NULL }

        	get = function() x
        	setinv = function(inverse) inv <<- inverse 
        	getinv = function() inv
        	list(set=set, get=get, setinv=setinv, getinv=getinv) }
}
