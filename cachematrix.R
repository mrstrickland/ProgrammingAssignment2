## The makeCacheMatrix function creates a list of 4 functions set, get, setinv and getinv.
## set - sets the value of the matrix, get - gets the value of the matrix,
## setinv - sets the value of the inverse matrix and getinv gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y){
	x<<-y
	inv<-NULL
	}
	get <- function() x       
			setinv <- function(solve) inv <<- solve
            getinv <- function() inv
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
    }

## Once running a particular matrix through the makeCacheMatrix function, the function cacheSolve can then be applied to the resultant list of functions.
## The function first checks whether the inverse has already been calculated and if this is the case, uses the getinv function to retrieve the inverse matrix from the cache.
## If this is not the case, then the matrix x is retrieved using the get function and then the solve function is applied and the result printed. 

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
    if(!is.null(inv)) {
			message("retrieving cache data")
            return(inv)
            }
    data <- x$get() 
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
    }
   

