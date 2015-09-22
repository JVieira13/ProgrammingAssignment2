makeCacheMatrix <- function(x) {
    # Verifies if the argument x is a matrix            
		if(is.matrix(x)==F) {
			stop("Argument must be a matrix!") }
  
    # Verifies if the matrix is invertible
		if(det(x)==0) {
	   		stop("Non-invertible matrix!") }
	   	
    #   set the matrix
         		else {
        		inv = NULL
        		set = function(y) {
                	x <<- y
                	inv <<- NULL }
    #   get the matrix
        	get = function() x
    #   set the inverse
        	setinv = function(inverse) inv <<- inverse
    #   get the inverse
        	getinv = function() inv
        	list(set=set, get=get, setinv=setinv, getinv=getinv) }
}




cacheSolve <- function(x) {
     # The argument must be the output of makeCacheMatrix
     		inv = x$getinv()
     		
     # Checks if the inverse matrix has already been calculated
		if (!is.null(inv)){
      			message("getting cached inverse matrix")
            	return(inv) }
            	
     # If not, calculates the inverse of the new matrix and return it   
		new.inv = x$get()
      		inv = solve(new.inv)
      		x$setinv(inv)
      		return(inv)
}
