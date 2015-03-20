
## These two functions allow a matrix x to be stored in the parent environment 
 ## and its inverse, m, to also be stored in the parent environment. 
 ## Then these stored matrices can be recalled rather than recalculated, hence saving processing time. 
 
 
 ## makeCacheMatrix starts with a matrix x, stores it is the parent enviroment,  
 ## calculates its inverse and stores that in the parent environment. 
 
 
 makeCacheMatrix <- function(x = matrix()) { 
 m <- NULL 
         set <- function(y) { 
                 x <<- y 
                 m <<- NULL 
         } 
         get <- function() x 
         setsolve <- function(solve) m <<- solve 
         getsolve <- function() m 
         list(set = set, get = get, 
              setsolve = setsolve, 
              getsolve = getsolve) 
 } 
 
 
 
 ## cacheSolve checks whether the inverse of x exists and if so retrieves the cached data 
 ## Else it calculates the inverse. 
 
 
cacheSolve <- function(x, ...) { 
        ## Return a matrix that is the inverse of 'x' 
 m <- x$getsolve() 
        if(!is.null(m)) { 
                message("getting cached data") 
                 return(m) 
                 } 
         data <- x$get() 
         m <- solve(data, ...) 
         x$setsolve(m) 
         m 
 } 

 


 
