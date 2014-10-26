## cachematrix.R has two main functions, makeCacheMatrix() and cacheSolve() 
## makeCacheMatrix() takes a matrix argument but organizes other functions so
## users can also set()matrix input or their inverses in the function environment,
## and super-assign them to parent cache; cacheSolve() then retrieves an existing 
## inverse outside the function environment or if not there calculates it anew

makeCacheMatrix <- function(x = matrix()) {  ## allows incoming matrix as its argument, x

	I <- NULL				## Initialize variable 'I' making space for inverse of argument x

	set <- function(y) {		## Defines set() function taking a matrix argument y  
						## Set() is callable as part of makeCachematrix environment
						
		x <<- y			## Super-assigns the y-argument to a matrix x outside the set-function environment, 
						## and into the makeCachMatrix environment. This x is not the original argument,
						## but a free variable that once super-assigned replaces the original argument x
						## Allows the user to change input matrix without re-invoking makeCacheMatrix
																		
		I <<- NULL			## Initialize 'I' within set(), because if setting x this way, skips previous NULL 
		}
	
	get <- function() { x }		## Here x is either super-assigned by set(y), or passes as makeCacheMatrix(x) argument;
						## Either way callable by get() defined in the makeCacheMatrix environment, displays x
	
	setinverse <- function(inverse) { I <<- inverse }
	
						## Defines setinverse() function, to set inverse matrix with argument 'inverse';
						## Called by user or cacheSolve() below. Super-assigns argument to 'I' outside 
						## makeCacheMatrix environment to parent cache for retrieval
 
	getinverse <- function() { I }
						## Defines getinverse function to display 'I' super-assigned via setinverse,
						## whether invoked from user or computed from cacheSolve() below

	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
						## allows calling these functions individually as part of makeCacheMatrix environment
}

## If inverse of x is already calculated and matrix is not changed, cacheSolve() retrieves 'I' from cache
## Otherwise uses the R function solve()to calculate the inverse matrix

cacheSolve <- function(x, ...)  {	

	I <- x$getinverse()		## Checks if 'I' already set by setinverse() above into parent environment,
						## by calling the getinverse function defined above
					
		if(!is.null(I)) {		## If 'I' is not null, it will return 'I' and done
			
			message("getting cached data")
			return(I)
		}				# if 'I' is null it skips message and return, and continues to computation

	data <- x$get()			# Using the get function defined above, takes x matrix passed as cacheSolve argument
	I <- solve(data, ...)		# Uses the R function solve()to calculate inverse and assign to I in the function environment
	x$setinverse(I)			# Uses setinverse() function defined above to super-assign I to parent cache
	I					# Output the calculated inverse matrix 'I'
}