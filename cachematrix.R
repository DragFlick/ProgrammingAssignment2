## ASSIGNMENT 2
## AS SUGGESTED THE FIRST FUNCTION makeCacheMatrix() CREATES THE SOURCE MATRIX. THE FUNCTION CONSISTS OF VARIOUS CONSTRUCTOR 
## FUNCTIONS WHICH CREATES AND THE SOURCE MATRIX (get_soource_matrix()) , SET THE VALUE OF THE SOURCE MATRIX (set_source_matrix())
## , FETCH THE INVERSE MATRIX FROM THE CACHE (get_inverse_matrix()) AND SET THE VALUE OF THE INVERSE MATRIX IN CACHE(set_inverse_matrix())

## cacheSolve() FUNCTION EVALUATE THE INVERSE OF THE MATRIX IN CASE THE INVERSE IS NOT PRESENT IN CACHE AND POPULATES THE CACHE.
## IN CACHE , THERE ARE SUBSEQUENT FUNCTION CALLS WITH SAME INPUT , THE FUNCTION ACCESSES THE CACHE TO EXTRACT THE VALUE OF THE 
## INVERSE





## THE FUNCTION makeCacheMatrix() TAKES A NUMERIC VECTOR AS INPUT . IT VALIDATES IF THE VECTOR IS OF CORRECT LENGTH
## AND THEREAFTER CONVERTS THE VECTOR INTO A SQUARE MATRIX. IT ALSO EVALUATES CONSTRUCTOR FUNCTION TO EXTRACT OR SET 
## DATA VALUES WITHIN THE MATRIX OBEJCT

makeCacheMatrix <- function(SOURCE_VECTOR )
{
        SOURCE_VECTOR <- as.numeric(SOURCE_VECTOR)      # CONVERTING TO NUMERIC VECTOR
        SOURCE_MATRIX <- matrix()      
        CACHE_MATRIX_INVERSE <- matrix()
        
        # cHECKING IF THE INPUT VECTOR IS A PERFECT SQUARE
        
        if((sqrt(length(SOURCE_VECTOR))-trunc(sqrt(length(SOURCE_VECTOR))))!=0)
                stop("Please enter a vector with a perfect square length")
        
        
        
        # CREATING THE SOURCE MATRIX
        
        set_source_matrix <- function()
        {  
                v_nrow <- as.integer(sqrt(length(SOURCE_VECTOR)))
                v_ncol <- as.integer(sqrt(length(SOURCE_VECTOR)))
                
                SOURCE_MATRIX <<- matrix(SOURCE_VECTOR, nrow = v_nrow ,ncol = v_ncol)
                
        }
        
        # TO FETCH THE SOURCE MATRIX FROM SOURCE VARIABLE
        
        get_source_matrix <- function()
                
        {
                return(SOURCE_MATRIX)
        }
        
        # TO SET VALUE OF INVERSE MATRIX IN CACHE
        
        set_inverse_matrix <- function(INVERSE_MATRIX)
        {
                CACHE_MATRIX_INVERSE <<- INVERSE_MATRIX
        }
        
        # TO FETCH THE VALUE OF THE INVERSE MATRIX FROM CACHE
        
        get_inverse_matrix <- function()
        {
                return(CACHE_MATRIX_INVERSE)
        }
        
        list(get_source_matrix = get_source_matrix,set_source_matrix = set_source_matrix,set_inverse_matrix = set_inverse_matrix,get_inverse_matrix = get_inverse_matrix)
}



## cacheSolve() FUNCTION TAKES INPUT ANY makeCacheMatrix() OBJECT AND EVALUATES THE INVERSE OF THE MATRIX. IN CASE 
## THE INVERSE HAS ALREADY BEEN EVALUATED EARLIER , THE FUNCTION FETCHES THE INVERSE FROM THE CACHE.


cacheSolve <- function(INPUT_CACHE_MATRIX)
        
{
        INPUT_CACHE_MATRIX$set_source_matrix()
        s_matrix <- INPUT_CACHE_MATRIX$get_source_matrix()
        cache_matrix_inverse <- INPUT_CACHE_MATRIX$get_inverse_matrix()
        
        
        if(!(is.na(cache_matrix_inverse[1][1])))
        {
                
                print("Returning Cached Value")
                return(cache_matrix_inverse)
        }
        
        else
        {
                v_invert_matrix <- solve(s_matrix)
                INPUT_CACHE_MATRIX$set_inverse_matrix(v_invert_matrix)
                v_invert_matrix
        }
        
}


