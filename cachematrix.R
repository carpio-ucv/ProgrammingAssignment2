## GENERAL DESCRIPTION
## The functions allows to avoid time-consuming computations by saving in a different environment the inverse of a matrix
## being calculated. Then it can be recalled from that environment (instead of calculate it againa) when it is again needed.

makeCacheMatrix <- function(x = matrix()) {
 mi <- NULL                             
 
  set <- function(y) {                  
                                        
    x <<- y                             
    mi <<- NULL                         
                                        
  }
  get <- function() x                   
                                        
  setinverse <- function(solve) mi <<- solve  
                                        
  getinverse <- function() mi              
  
  list(set = set, get = get,            
       setinverse = setinverse,              
       getinverse = getinverse)
}
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse 
## has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
          mi <- x$getinverse()                     
                                        
  if(!is.null(mi)) {                     
                                        
    message("getting cached data")
    return(mi)
  }
  data <- x$get()                       
                                        
  inverse <- solve(data, ...)   
                                
  x$setinverse(mi)                  
                                
  mi                             
}
}
