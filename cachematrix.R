## The makeCacheMatrix function creates a list of 4 functions set, get, setinv and getinv.
## set - sets the value of the matrix throughout the makeCacheMatrix function, get - gets the value of the matrix,
## setinv - sets the value of the inverse matrix throughout the makeCacheMatrix function and getinv gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL	##Creates the inv value and sets it to NULL
	set<-function(y){ ##Creates a function that sets the value of x outside of the set function environment and assigns inv as NULL again. 
					  ##Will be used to reset values when x is changed after the inverse of the previous x has been calculated.
	x<<-y
	inv<<-NULL
	}
	get <- function() x ##Creates an empty function that reurns the value of x.     
            setinv <- function(a) inv <<- a ##Creates a function that sets inv to be a value outside of the setinv function environment.
			getinv <- function() inv ##Creates and empty function that returns the value of inv.
            list(set = set, get = get, ##Outputs a list of the four functions created in makeCacheMatrix
                 setinv = setinv,
                 getinv = getinv)
    }

## Once running a particular matrix through the makeCacheMatrix function, the function cacheSolve can then be applied to the resultant list of functions.
## The function first checks whether the inverse has already been calculated and if this is the case, uses the getinv function to retrieve the inverse matrix from the cache.
## If this is not the case, then the matrix x is retrieved using the get function and then the solve function is applied and the result printed. 

cacheSolve <- function(x, ...) {
	inv <- x$getinv() ##Sets inv as the value from the getinv function of the input
    if(!is.null(inv)) { ##Tests whether inv is not the value NULL - if this is the case if retrieves the value from 
	                    ##the cache as it has already been calculated.
            message("retrieving cache data")
            return(inv) ##Returns the value of the inverse.
            }
    data <- x$get() ##If the inverse is not already in the cache, use the x$get() function to get the value of the matrix x, 
	                ##originally entered into makeCacheMatrix, assigning it to variable data.
    inv <- solve(data, ...) ##inv is assigned as the inverse of the the original matrix (now stored in variable data).
    x$setinv(inv) ## The inverse value is stored in the cache, using the setinv function defined previously.
    inv ##inv is returned
    }
   

