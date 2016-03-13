## Git commands
# cd C:\Users\K56CA\Documents\GitHub\ProgrammingAssignment2
# git add . 
# git commit -m "[first commit]"
# git push

#####################
## makeCacheMatrix:##
#####################

# This function takes a square matrix as argument (inversable)
# and cache its inverse. Speciflicaly it does the following: 

#set the value of the matrix
#get the value of the matrix
#set the value of the matrix inverse
#get the value of the matrix inverse

makeCacheMatrix <- function(x) {
        mat<-NULL

# Set funtion to store inverse matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }        
# Get matrix introduced in the function as argument
        get <- function() x
# Set inverse of the matrix 
        setinv <- function(solve) mat <<- solve
# Get inverse of the matrix 
        getinv <- function() mat        
# Create a list copntaining outcomes from the 4 previous subfunctions
        list(set = set, get = get, setinv = setinv,
             getinv = getinv)
}

##############
# cacheSolve:# 
##############
# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already 
# been calculated (and the matrix has not changed), then the 
# cachesolve should retrieve the inverse from the cach       

cacheSolve <- function(x, ...) {
# Get inverse of the matrix introduced as parameter in makeCacheMatrix()
        inv <- x$getinv()
# Return a messae when inverse hasn't been calculated before, otherwise 
# it use the cache from makeCacheMatrix()        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
# Store original matrix in "data"
        data <- x$get()
# Estimate the inverse of the matrix (when there it has not been cached)
        inv <- solve(data)
        x$setinv(inv)
# Return inverse matrix
        inv
}

##########
# EXAMPLE#
##########

# Define and inversable square matrix
x<- matrix(c(4,1,3,1),2,2)

# Runiing first function to cache inverse matrix
cachedmatrix<-makeCacheMatrix(x)

#       [,1] [,2]
# [1,]    4    3
# [2,]    1    1

# Running function for the fist time, where the inverse is calculated 
cacheSolve(cachedmatrix)
#       [,1] [,2]
# [1,]    1   -3
# [2,]   -1    4

# Running function for the second time, where the inverse is cached
cacheSolve(cachedmatrix)
# getting cached data
#      [,1] [,2]
# [1,]    1   -3
# [2,]   -1    4
