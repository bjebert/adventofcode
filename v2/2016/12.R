
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

# inp <- strsplit("cpy 41 a
# inc a
# inc a
# dec a
# jnz a 2
# dec a", "\n")[[1]]  # Example

inp <- get_input("2016/12", parse = F, user = "blakeebets", cache = T)

reg <- setNames(rep(0, 4), letters[1:4])
reg[["c"]] <- 1

i <- 1
last_d <- reg[["d"]]
while(i <= length(inp)) {
    ls <- strsplit(inp[i], " ")[[1]]
    # print(ls)
    
    if(ls[1] == "cpy") {
        source <- ls[2]
        x <- if(source %in% names(reg)) reg[[source]] else as.numeric(source)
        
        reg[[ls[3]]] <- x
        i <- i + 1
    } else if(ls[1] == "inc") {
        reg[[ls[2]]] <- reg[[ls[2]]] + 1
        i <- i + 1
    } else if(ls[1] == "dec") {
        reg[[ls[2]]] <- reg[[ls[2]]] - 1
        i <- i + 1
    } else if(ls[1] == "jnz") {
        source <- ls[2]
        x <- if(source %in% names(reg)) reg[[source]] else as.numeric(source)
        
        if(x != 0) {
            source <- ls[3]
            y <- if(source %in% names(reg)) reg[[source]] else as.numeric(source)
            i <- i + y
        } else {
            i <- i + 1
        }
    }
    
    if(reg[["d"]] != last_d) {
        print(reg)
        last_d <- reg[["d"]]
    }
}

# 6:51 (5th)

# 11:44 (40th)

