
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("", "\n")[[1]]  # Example
inp <- get_input("2015/24", parse = F, user = "blakeebets", cache = T)

inp <- as.numeric(inp)

inp

# weighs same, first group: few as possible, then smallest product
# need to use every package

group_weight <- sum(inp) / 3
# min group size is 6, max group size is 16
# find smallest group size of 6 that sums up, i reckon?

combos6 <- t(combn(inp, 6))
combos6 <- combos6[rowSums(combos6) == group_weight, ]

min_q <- Inf

for(i in 1:nrow(combos6)) {
    inp2 <- setdiff(inp, combos6[i,])
    
    for(g in seq(6, 10, 2)) {
        combos_g2 <- t(combn(inp2, g))
        combos_g2 <- combos_g2[rowSums(combos_g2) == group_weight, ]
        
        if(length(combos_g2) == 0) {
            next
        }
        
        if(!is.matrix(combos_g2)) {
            combos_g2 <- t(as.matrix(combos_g2))
        }
        
        for(j in 1:nrow(combos_g2)) {
            combos_g3 <- setdiff(inp2, combos_g2[j, ])
            
            if(sum(combos_g3) == group_weight) {
                q <- prod(combos6[i,])
                
                if(q < min_q) {
                    min_q <- q
                    print(q)
                }
            }
        }
    }
}

# 17:14 (27th)


# Part 2 ------------------------------------------------------------------


group_weight <- sum(inp) / 4

# groups must now be odd

combos5 <- t(combn(inp, 5))
combos5 <- combos5[rowSums(combos5) == group_weight, ]

min_q <- Inf

for(i in 1:nrow(combos5)) {
    inp2 <- setdiff(inp, combos5[i,])
    
    for(g in seq(5, 9, 2)) {
        combos_g2 <- t(combn(inp2, g))
        combos_g2 <- combos_g2[rowSums(combos_g2) == group_weight, ]
        
        if(length(combos_g2) == 0) {
            next
        }
        
        if(!is.matrix(combos_g2)) {
            combos_g2 <- t(as.matrix(combos_g2))
        }
        
        for(j in 1:nrow(combos_g2)) {
            inp3 <- setdiff(inp2, combos_g2[j,])
            
            for(g2 in seq(5, 9, 2)) {
                combos_g3 <- t(combn(inp3, g2))
                combos_g3 <- combos_g3[rowSums(combos_g3) == group_weight, ]
                
                if(length(combos_g3) == 0) {
                    next
                }
                
                if(!is.matrix(combos_g3)) {
                    combos_g3 <- t(as.matrix(combos_g3))
                }
                
                for(k in 1:nrow(combos_g3)) {
                    combos_g4 <- setdiff(inp3, combos_g3[k, ])
                    
                    if(sum(combos_g4) == group_weight) {
                        q <- prod(combos5[i,])
                        
                        if(q < min_q) {
                            min_q <- q
                            print(q)
                        }
                    }
                }
                
            }
        }
    }
}

# 21:02 (29th)
