
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")
inp1 <- get_input("2015/07", parse = F, user = "blakeebets", cache = T)

# Problem -----------------------------------------------------------------

wires <- c()
inp <- copy(inp1)

inp[inp == "14146 -> b"] <- "956 -> b"  # Part 2

while(length(inp) > 0) {
    to_remove <- c()
    
    for(line in inp) {
        instr <- strsplit(line, " -> ")[[1]]
        target <- instr[length(instr)]
        
        if(grepl("AND", instr[1])) {
            w <- strsplit(instr[1], " AND ")[[1]]
            val1 <- ifelse(w[1] %in% names(wires), wires[[w[1]]], as.numeric(w[1]))
            val2 <- ifelse(w[2] %in% names(wires), wires[[w[2]]], as.numeric(w[2]))
            
            if(is.na(val1) || is.na(val2)) {
                next
            }
            
            wires[[target]] <- bitwAnd(val1, val2)
            
        } else if(grepl("NOT", instr[1])) {
            w <- gsub("NOT ", "", instr[1])
            val <- ifelse(w %in% names(wires), wires[[w]], as.numeric(w))
            
            if(is.na(val)) {
                next
            }
            
            x <- bitwNot(val)
            if(x < 0) {
                x <- 65536 +x
            }
            
            wires[[target]] <- x  
            
        } else if(grepl("OR", instr[1])) {
            w <- strsplit(instr[1], " OR ")[[1]]
            val1 <- ifelse(w[1] %in% names(wires), wires[[w[1]]], as.numeric(w[1]))
            val2 <- ifelse(w[2] %in% names(wires), wires[[w[2]]], as.numeric(w[2]))
            
            if(is.na(val1) || is.na(val2)) {
                next
            }
            
            wires[[target]] <- bitwOr(val1, val2)
            
        } else if(grepl("LSHIFT", instr[1])) {
            w <- strsplit(instr[1], " LSHIFT ")[[1]]
            val1 <- ifelse(w[1] %in% names(wires), wires[[w[1]]], as.numeric(w[1]))
            val2 <- ifelse(w[2] %in% names(wires), wires[[w[2]]], as.numeric(w[2]))
            
            if(is.na(val1) || is.na(val2)) {
                next
            }
            
            wires[[target]] <- bitwShiftL(val1, val2)
            
        } else if(grepl("RSHIFT", instr[1])) {
            w <- strsplit(instr[1], " RSHIFT ")[[1]]
            val1 <- ifelse(w[1] %in% names(wires), wires[[w[1]]], as.numeric(w[1]))
            val2 <- ifelse(w[2] %in% names(wires), wires[[w[2]]], as.numeric(w[2]))
            
            if(is.na(val1) || is.na(val2)) {
                next
            }
            
            wires[[target]] <- bitwShiftR(val1, val2)
        } else {
            w <- instr[1]
            val <- ifelse(w %in% names(wires), wires[[w]], as.numeric(w))
            
            if(is.na(val)) {
                next
            }
            
            wires[[target]] <- val
        }
        
        # remove line from inp
        w <- which(line == inp)
        print(line)
        to_remove <- c(to_remove, w)
    }
    
    inp <- inp[-to_remove]
    print(length(inp))
}

# 18:22 (4th)

wires[["a"]]

# 20:06 (4th)
