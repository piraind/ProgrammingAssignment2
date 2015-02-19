## R Programming Assignment 2
## Understanding environments
## makeCacheMatrix function that creates list for caching
## cacheSolve calculates matrix inversion and caches the solution

## makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set<-function(y){
            x<<-y
            m<<-NULL
            
      }
      get<-function() x
      setmatrix<-function(solve) m<<- solve
      getmatrix<-function() m
      list(set=set, get=get,
           setmatrix=setmatrix,
           getmatrix=getmatrix)
}

## cacheSolve
cacheSolve <- function(x=matrix(), ...) {
      m<-x$getmatrix()
      if(!is.null(m)){
            message("cached")
            return(m)
      }
      matrix<-x$get ()
      m<-solve(matrix, ...)
      x$setmatrix(m)
      message("inverted matrix no cache")
      m
}

# test commands to see how functions work
# Copy commands to R console
      # create test list matrix
      tmatrix <- makeCacheMatrix()

      ## set matrix in test list matrix
      ## tmatrix$set(matrix(1:4,2,2))
      ## make test matrix
      set.seed(1)
      test <- matrix(rnorm(5*5), 5, 5)
      ## set test matrix in list matrix
      tmatrix$set(test)
      tmatrix$get () # orginal matrix before first call
      tmatrix$getmatrix () # cache before first call

      # invert matirx first time
      tmatrix$getmatrix () # cache before first call
      cacheSolve(tmatrix)
      tmatrix$getmatrix () # cache after first call

      #invert matrix second time with cache
      cacheSolve(tmatrix) 