## makeCacheMatrix(x) and cacheSolve(x,...) are created for the purpose to find 
## an inverse of an invertable matrix and cache it, so the next time you will 
## need to find the inverse of the same matrix you can retrieve it from a cache 
## and save some time.

## makeCacheMatrix(x) creates an environment, where both original matrix 
## and inverted matrix are stored as well as a list of functions, which allow 
## to read and write them.

makeCacheMatrix <- function(x = matrix()) {
      ## We create an empty variable, called InvMatrix, where our inverted 
      ## matrix will be later stored.
      InvMatrix <- NULL
      ## set(y) function allows us to change the input matrix externally using
      ## name$set(y) function.While resetting the matrix, our previously stored 
      ## InvMatrix will be replaced with NULL value.
      set <- function(y) {
            x <<- y
            InvMatrix <<- NULL
      }
      ## name$get() function allows us to retrieve input matrix x from memory.
      get <- function() x
      ## name$setInvMatrix assignes a value to InvMatrix.
      setInvMatrix <- function(solved) InvMatrix <<- solved
      ## name$getInvMatrix retrives a value of InvMatrix.
      getInvMatrix <- function() InvMatrix
      ## list saves all our functions as a list 'name' 
      ## (name <- makeCacheMatrix(x))
      list(set = set, get = get,
           setInvMatrix = setInvMatrix,
           getInvMatrix = getInvMatrix)
}


## cacheSolve function uses the output of makeCacheMatrix as an argument. 
## If the value of InvMatrix is already stored in cache it prints: 
## "getting cached data" and returns cached inverted matrix. 
## Otherwise it finds the inverse of the matrix using solve() function 
## and writes it to the cache of setInvMatrix.

cacheSolve <- function(name, ...) {
      ## First we look for the inverted matrix in "cache", previously saved
      ## in the environment created by makeCacheMatrix function (aka "cache"). 
      InvMatrix <- name$getInvMatrix()
      ## If inverted matrix is found we print "getting cached data"
      ## and return the value of inverted matrix.
      if(!is.null(InvMatrix)) {
            message("getting cached data")
            return(InvMatrix)
      }
      ## In case inverted matrix is not found in "cache", we retrieve the input 
      ## matrix using name$get() and find an inverse of this matrix using 
      ## solve() function.
      data <- name$get()
      InvMatrix <- solve(data, ...)
      ## We write the obtained inverse of a matrix back using 
      ## name$setInvMatrix(InvMatrix).
      name$setInvMatrix(InvMatrix)
      ## We return the value of inverted matrix.
      InvMatrix
}

## End of the programm.