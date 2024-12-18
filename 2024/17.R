
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")
i <- 1

# Problem -----------------------------------------------------------------


inp <- get_input("2024/17", parse = F, user = "bjebert", cache = F)
inp


# inp <- strsplit("Register A: 2024
# Register B: 0
# Register C: 0
# 
# Program: 0,3,5,4,3,0", "\n")[[1]]  # Example

a <- nums(inp)[[1]]
b <- nums(inp)[[2]]
c <- nums(inp)[[3]]
program <- nums(inp)[[5]]
reg <- c('a' = a, 'b' = b, 'c' = c)

run(208)
run <- function(a) {
    ptr <- 0
    out <- c()
    
    reg[['a']] <<- a
    
    print(reg)
    
    while(ptr < length(program)) {
        opcode <- program[ptr + 1]
        operand <- program[ptr + 2]    
        
        lval <- copy(operand)
        if(operand %in% 0:3) {
            cval <- operand    
        } else if(operand == 4) {
            cval <- reg[['a']]
        } else if(operand == 5) {
            cval <- reg[['b']]
        } else if(operand == 6) {
            cval <- reg[['c']]
        }
        
        if(opcode == 0) {
            numer <- reg[['a']]
            denom <- 2^cval
            
            reg[['a']] <- floor(numer / denom)
            ptr <- ptr + 2
        } else if(opcode == 1) {
            reg[['b']] <- bitwXor(reg[['b']], lval)
            ptr <- ptr + 2
        } else if(opcode == 2) {
            reg[['b']] <- cval %% 8
            ptr <- ptr + 2
        } else if(opcode == 3) {
            if(reg[['a']] != 0) {
                ptr <- lval
            } else {
                ptr <- ptr + 2
            }
        } else if(opcode == 4) {
            reg[['b']] <- bitwXor(reg[['b']], reg[['c']])
            ptr <- ptr + 2
        } else if(opcode == 5) {
            out <- c(out, cval %% 8)
            
            # if(!identical(out, program[1:length(out)])) {
            #     next
            # }
            ptr <- ptr + 2
        } else if(opcode == 6) {
            numer <- reg[['a']]
            denom <- 2^cval
            
            reg[['b']] <- floor(numer / denom)
            ptr <- ptr + 2
        } else if(opcode == 7) {
            numer <- reg[['a']]
            denom <- 2^cval
            
            reg[['c']] <- floor(numer / denom)
            ptr <- ptr + 2
        }
        
        print(sprintf("opcode = %s | literal = %s | combo = %s", opcode, lval, cval))
        print(reg)
    }
    
    paste(out, collapse = ",")
}

# analysis ----------------------------------------------------------------

xor <- function(n, x) {
    if(x == 0) return(n)
    if(x == 1) if(n %% 2 == 0) return(n + 1) else return(n - 1)
    if(x == 2) return(n - c(-2L, 2L, 2L, -2L)[(n - 1) %% 4 + 1])
    if(x == 3) return(n - c(-1, 1, 3, -3)[(n - 1) %% 4 + 1])
    if(x == 4) return(n - c(-4L, -4L, -4L, 4L, 4L, 4L, 4L, -4L)[(n - 1) %% 8 + 1])
    if(x == 5) return(n - c(-3L, -5L, -3L, 3L, 5L, 3L, 5L, -5L)[(n - 1) %% 8 + 1])
    if(x == 6) return(n - c(-6L, -2L, -2L, 2L, 2L, 6L, 6L, -6L)[(n - 1) %% 8 + 1])
    if(x == 7) return(n - c(-5L, -3L, -1L, 1L, 3L, 5L, 7L, -7L)[(n - 1) %% 8 + 1])
    return(NA)
}



# cheese method (used for original solution) ------------------------------
# 'guess' and check

s <- seq(236555995274500, 236555995276500, 1)
mat <- lapply(s, solve)
mat <- mat[sapply(mat, function(x) length(x) == 16)]
mat <- t(sapply(mat, function(x) x))
z <- apply(mat != matrix(rep(program, nrow(mat)), ncol = ncol(mat), byrow = T), 1, function(x) {
    w <- which(x)
    if(length(w) == 0) {
        return(0)
    }
    
    return(max(which(x)))
})
    
s[which(z == min(z))]
print(min(z))

solve(236555995274943)
program

solve <- function(a) {
    out <- c()
    while(a > 0) {
        b <- xor(a %% 8, 3)
        b <- xor(xor(floor(a / 2^b), b), 5)
        out <- c(out, b %% 8)
        a <- floor(a / 8)
    }
    out
}


# analysis ----------------------------------------------------------------

a <- 236555995274943

b <- xor(xor(floor(a / 2^xor(a %% 8, 3)), xor(a %% 8, 3)), 5)
out <- c(out, b %% 8)
a <- floor(a / 8)


while(a > 0) {
    b <- xor(a %% 8, 3)
    b <- xor(floor(a / 2^b), b)
    b <- xor(b, 5)
    
    b <- xor5(bitwXor(floor(a / 2^b), b))
    print(b %% 8)
    a <- floor(a / 8)
}



