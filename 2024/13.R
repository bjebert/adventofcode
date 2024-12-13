# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- get_input("2024/13", parse = F, user = "bjebert", cache = F)
inp

# sum(sapply(1:length(ax), function(i) {
#     x_a <- ax[i]
#     y_a <- ay[i]
# 
#     x_b <- bx[i]
#     y_b <- by[i]
# 
#     goal_x <- prizex[i]
#     goal_y <- prizey[i]
# 
# 
#     wx <- which(press[,1] * x_a + press[,2] * x_b == goal_x)
#     wy <- which(press[,1] * y_a + press[,2] * y_b == goal_y)
# 
#     w <- intersect(wx, wy)
# 
#     if(length(w) > 0) {
#         print(i)
#         print(press[w,])
#         return(min(rowSums(press[w,] * matrix(rep(c(3, 1), length(w)), ncol = 2, byrow = T))))
#     } else {
#         return(0)
#     }
# }))


# 
# inp <- strsplit("Button A: X+94, Y+34
# Button B: X+22, Y+67
# Prize: X=8400, Y=5400
# 
# Button A: X+26, Y+66
# Button B: X+67, Y+21
# Prize: X=12748, Y=12176
# 
# Button A: X+17, Y+86
# Button B: X+84, Y+37
# Prize: X=7870, Y=6450
# 
# Button A: X+69, Y+23
# Button B: X+27, Y+71
# Prize: X=18641, Y=10279", "\n")[[1]]  # Example

btna <- inp[seq(1, length(inp), 4)]
btnb <- inp[seq(2, length(inp), 4)]
prize <- inp[seq(3, length(inp), 4)]

as <- strsplit(btna, " ")
ax <- unname(sapply(gsub("X|Y|\\+|,", "", sapply(as, function(x) x[3])), as.numeric))
ay <- unname(sapply(gsub("X|Y|\\+|,", "", sapply(as, function(x) x[4])), as.numeric))

bs <- strsplit(btnb, " ")
bx <- unname(sapply(gsub("X|Y|\\+|,", "", sapply(bs, function(x) x[3])), as.numeric))
by <- unname(sapply(gsub("X|Y|\\+|,", "", sapply(bs, function(x) x[4])), as.numeric))

prizex <- sapply(strsplit(sapply(strsplit(prize, "="), function(x) x[2]), ", "), function(x) as.numeric(x[1]))
prizey <- as.numeric(sapply(strsplit(prize, "="), function(x) x[3]))

prizex <- 10000000000000 + prizex
prizey <- 10000000000000 + prizey


get_presses <- function(x_a, x_b, x_c, y_a, y_b, y_c, verbose = T) {
    divides <- (x_c / gcd(x_a, x_b)) %% 1 == 0 && (y_c / gcd(y_a, y_b)) %% 1 == 0
    
    if(!divides) {
        return(c(NA, NA))
    }
    
    res_x <- dio2d(x_a, x_b, x_c, verbose = verbose)
    res_y <- dio2d(y_a, y_b, y_c, verbose = verbose)
    
    n0 <- ceiling(-res_x[2] / res_x[1])  # minimise press_a while keeping it positive
    mod_n <- 1e0
    press_a <- res_x[1] * n0 + res_x[2]
    press_b <- res_x[3] * n0 + res_x[4]
    
    delta_y <- y_c - (press_a * y_a + press_b * y_b)
    deltas <- delta_y
    mods <- mod_n

    while(delta_y != 0) {
        press_a <- res_x[1] * n0 + res_x[2]
        press_b <- res_x[3] * n0 + res_x[4]
        delta_y <- y_c - (press_a * y_a + press_b * y_b)
        deltas <- c(delta_y, deltas)
        
        if(abs(deltas[1]) > abs(deltas[2])) {
            mod_n <- -mod_n
        }
        
        n0 <- n0 + mod_n
        
        d <- deltas[1:min(length(deltas), 5)]
        if(all(d > 0) || all(d < 0)) {
            mod_n <- mod_n * 2
        } else {
            if(length(mods) > 5 && all(abs(mods[1:5]) == 1)) {
                return(c(NA, NA))
            }
            
            if(mod_n > 0) {
                mod_n <- ceiling(mod_n / 2)
            } else if(mod_n < 0) {
                mod_n <- floor(mod_n / 2)
            }
        }
        
        mods <- c(mod_n, mods)
        
        if(verbose) {
            print(sprintf("delta_y: %s, n = %s, mod_n = %s", as.numeric(delta_y), as.numeric(n0), mod_n))
        }
    }
    
    return(c(as.numeric(press_a), as.numeric(press_b)))
}

press_mat <- t(sapply(1:length(prizex), function(i) {
    print(i)
    x_a <- ax[i]
    x_b <- bx[i]
    x_c <- prizex[i]
    
    y_a <- ay[i]
    y_b <- by[i]
    y_c <- prizey[i]
    
    get_presses(x_a, x_b, x_c, y_a, y_b, y_c, verbose = F)
}))


press_filt <- press_mat[!is.na(press_mat[,1]), ]
sum(rowSums(press_filt * matrix(rep(c(3, 1), nrow(press_filt)), ncol = 2, byrow = T)))
