
  ## This function creates a special "matrix" object that can cache its inverse.
  
  makeCacheMatrix<- function( x = numeric()) {
    ## Clear it out by setting local value to empty, 
    m <- NULL
    
    ## set the values in parents enviorment , which is m at the line above
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    ## Define get funciton
    get <- function() x
    
    ## Define set funciton 
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    
    ## Return a list which include all sub funcitons defined in the funciton
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }
  
  
  
  ## This is a funciton which used cached value for calculatio
  cacheSolve <- function(x, ...) {
    ## check cached value first
    m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    ## have to calcuate since it is not in the cache
    data <- x$get()  ## get the dataq
    ## calculate it
    m <- solve(data, ...)
    ## stick back to cache
    x$setInverse(m)
    m
  }
  
  
  ## added a comment line to test second summit