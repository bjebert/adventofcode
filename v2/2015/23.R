
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

# inp <- strsplit("inc a
# jio a, +2
# tpl a
# inc a", "\n")[[1]]  # Example

inp <- get_input("2015/23", parse = F, user = "blakeebets", cache = T)

reg <- c('a' = 1, 'b' = 0)
i <- 1

while(TRUE) {
    if(i < 1 || i > length(inp)) {
        break
    }
    
    line <- inp[i]
    ss <- strsplit(line, " ")[[1]]
    instr <- ss[1]
    
    if(instr == "hlf") {
        reg[[ss[2]]] <- reg[[ss[2]]] / 2
        i <- i + 1
    } else if(instr == "tpl") {
        reg[[ss[2]]] <- reg[[ss[2]]] * 3
        i <- i + 1
    } else if(instr == "inc") {
        reg[[ss[2]]] <- reg[[ss[2]]] + 1
        i <- i + 1
    } else if(instr == "jmp") {
        amt <- as.numeric(ss[2])
        i <- i + amt
    } else if(instr == "jie") {
        target <- gsub(",", "", ss[2])
        
        if(reg[[target]] %% 2 == 0) {
            amt <- as.numeric(ss[3])
            i <- i + amt
        } else {
            i <- i + 1
        }
    } else if(instr == "jio") {
        target <- gsub(",", "", ss[2])
        
        if(reg[[target]] == 1) {
            amt <- as.numeric(ss[3])
            i <- i + amt
        } else {
            i <- i + 1
        }
    }
}

reg

# 5:58 (2nd)

# 6:08 (1st)



