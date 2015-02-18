## Put comments here that give an overall description of what your
## functions do
## github test

## Write a short comment describing this function

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

cacheSolve <- function(x=matrix(), ...) {
      m<-x$getmatrix()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      matrix<-x$get ()
      m<-solve(matrix, ...)
      x$setmatrix(m)
      m
}

# test commands to see how functions work
# Copy commands to R console
      # create test list matrix
      tmatrix <- makeCacheMatrix()

      # set matrix in test list matrix
      tmatrix$set(matrix(1:4,2,2))
      str(tmatrix$get ()) # orginal matrix before first call
      str(tmatrix$getmatrix ()) # cache before first call

      # invert matirx first time
      str(tmatrix$getmatrix ()) # cache before first call
      cacheSolve(tmatrix)
      str(tmatrix$getmatrix ()) # cache after first call

      #invert matrix second time with cache
      cacheSolve(tmatrix) 