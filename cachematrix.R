## Very similar to the exfample functions of the mean,
## these two functions store (Cache) data it is passed
## while it solves (inverts) pass matrix's. 
##
## This Allows us to keep data for future use so it doesn't
## always have to recalcualte when the calculations have
# already been done. 


## makeCacheMatrix will store all matrix's that are passed to it for future use
## creating all needed shell functions to cache the data it gets
makeCacheMatrix <- function(x = matrix()) {
	
	#first create an empty variable, we will store our data here 
	m <- NULL
	
	##create a shell function, set, that will give us var X 
	## which can exists outside its scope
	## and will clear m in case it has been modified
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	
	## create the shell function to get the matrix 
	get <- function() x
	
	## create the shell function to store the inverse
	## and make again use the <<- to allow cross-env access
	setinverse <-function(solve) m <<- solve
	
	## create the shell functionto get the inverse 
	getinverse <- function() m
	
	## store this all in a list
	list(set = set, get = get,
		  setinverse = setinverse,
		  getinverse = getinverse
	)	
}


## CacheSolve does the actual work of inversion
## IF the inversion has not already been solved. 
## If it has, then it simply returns the stroed data
## from the previous calculation

cacheSolve <- function(x, ...) {
	
	#set m to equal the get inverse function (with x passed to it)
	m <- x$getinverse()
	
	##If there is currently data that matches, it will show a message
	## telling you that it is simply receiving that data
	## and then will display that
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	
	##otherwise, creates a variable 'data' that runs the get function
	#with the data it was passed 
	data <- x$get()
	
	##The meat of this whole thing. Inverse the matrix! 
	m <- solve(data)
	
	##send this to our set inverse function to store it for future use
	x$setinverse(m)
	
    ## Return a matrix that is the inverse of 'x'
    return(m)
}
