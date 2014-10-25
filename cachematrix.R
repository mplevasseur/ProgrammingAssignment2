## cachematrix.R has two functions, 1-makeCacheMatrix() and 2-cacheSolve() 
## makeCacheMatrix() takes a matrix argument but organizes other functions so
## users can also set()--or reset--matrix input, or their inverses, in the function environment,
## and super-assign them to parent cache; cacheSolve() then retrieves an existing 
## inverse outside the function environment or calculates it new

makeCacheMatrix <- function(x = matrix()) {  ## makeCacheMatrix allows incoming matrix as its argument, x

	I <- NULL				## Initialize variable 'I' to make space for inverse of argument x

	set <- function(y) {		## Defines set() function taking a matrix argument y  
						## Set() is callable as part of makeCachematrix environment
						
		x <<- y			## Super-assigns the y-argument to a matrix x outside the set-function environment, 
						## and into the makeCachMatrix environment. x here is not the original argument,
						## but a free variable that once super-assigned replaces the original argument x
						## Allows the user to change input matrix without re-invoking makeCacheMatrix
																		
		I <<- NULL			## Must initialize 'I' within set(), because if called to set x, skips previous NULL 
		}
	
	get <- function() { x }		## Here x is either super-assigned by set(y), or passes as makeCacheMatrix(x) argument;
						## Either way callable by get() defined in the makeCacheMatrix environment, to display x
	
	setinverse <- function(inverse) { I <<- inverse }
	
						## Defines setinverse() function, to set inverse matrix with argument 'inverse';
						## Called by user or cacheSolve() below. Super-assigns argument to 'I' outside 
						## makeCacheMatrix environment to parent cache
 
	getinverse <- function() { I }
						## Defines getinverse function to display 'I' super-assigned via setinverse,
						## whether invoked from user or computed from cacheSolve() below

	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
						## allows calling these functions individually as part of makeCacheMatrix environment
}

## If inverse of x is already calculated and matrix is not changed, cacheSolve() retrieves 'I' from cache
## Otherwise uses the R function solve()to calculate the inverse matrix

cacheSolve <- function(x, ...)  {	

	I <- x$getinverse()		## Checks if 'I' already set by setinverse() above into parent environment, by
						## calling the getinverse function defined above
					
		if(!is.null(I)) {		## If 'I' is not null, it will return 'I' and done
			
			message("getting cached data")
			return(I)
		}				# if 'I' is null it skips message and return, and continues

	data <- x$get()			# Using the get function defined above,to takes x matrix passed as cacheSolve argument
	I <- solve(data, ...)		# Uses the R function solve()to calculate inverse and assign to I in the function environment
	x$setinverse(I)			# Uses setinverse() function defined above to super-assign I to parent cache
	I					# Output the calculated inverse matrix 'I'
}