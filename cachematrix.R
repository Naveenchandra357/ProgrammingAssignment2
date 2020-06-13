##My functon here first cache the data into a matrix
##then it returns the value of its inverse



makeCacheMatrix <- function(x = matrix()) {
      inv<- NULL
      set<- function(y){
        x<<- y
        inv<<- NULL
      }
      get<- function() x
      setinverse<- function(inverse) inv<<- inverse
      getinverse<- function() inv
      
      list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse
             )
}


##this function computes the inverse of the matrix and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv<- x$getinverse()
     if(!is.null(inv)){
       message("getting cached data")
       return(inv)
     }
     mat<- x$get()
     inv<- solve(mat, ...)
     x$setinverse()
     inv
     
}
