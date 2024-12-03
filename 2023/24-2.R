rm(list=ls())
source("utilities.R")

library(Rmpfr)
library(numbers)

inp <- get_input("2023/24", parse = F, deesblake = F, cache = T)

inp <- strsplit("19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3", "\n")[[1]]

positions <- lapply(strsplit(sapply(strsplit(inp, " @ "), function(x) x[1]), ", "), as.numeric)
velos <- lapply(strsplit(sapply(strsplit(inp, " @ "), function(x) x[2]), ", "), as.numeric)

px <- mpfr(sapply(positions, function(x) x[1]), precBits = 128)
vx <- mpfr(sapply(velos, function(x) x[1]), precBits = 128)
py <- mpfr(sapply(positions, function(x) x[2]), precBits = 128)
vy <- mpfr(sapply(velos, function(x) x[2]), precBits = 128)
pz <- mpfr(sapply(positions, function(x) x[3]), precBits = 128)
vz <- mpfr(sapply(velos, function(x) x[3]), precBits = 128)

map <- c("x" = 1, "y" = 2, "z" = 3)
dim <- "x"
verbose <- T

# Part 2 ------------------------------------------------------------------


test_rv <- function(rv, dim = "x", verbose = T, return_rp = F) {
    
    p <- mpfr(sapply(positions, function(x) x[map[dim]]), precBits = 128)
    v <- mpfr(sapply(velos, function(x) x[map[dim]]), precBits = 128)
    
    if(verbose) print(rv)
    if(rv %in% v) {
        forced_pos <- unique(p[which(v == rv)])
        if(length(forced_pos) > 1) {
            return(FALSE)
        } else {
            if(any(!(v[p == forced_pos] %in% c(0, rv)))) {
                return(FALSE)
            }
            
            # Check that positions above current will intersect
            p_hi <- p[p > forced_pos]
            v_hi <- v[p > forced_pos]
            
            p_lo <- p[p < forced_pos]
            v_lo <- v[p < forced_pos]
            
            # Velocity must be in right direction (<= rv if hi, >= rv if lo)
            if(any(v_hi >= rv) || any(v_lo <= rv)) {
                return(FALSE)
            }
        }
    }
    
    # Continue establishing 2D Diophantine equations until last variable
    
    k <- p + v - rv
    x <- v - rv
    
    k <- k[x != 0]
    x <- x[x != 0]
    
    # Solve for initial two equations, then begin loop
    coef <- c(1, 0)
    
    rp_pot <- NULL
    for(i in 2:length(x)) {
        # dput(c(a[i-1] * coef[1], -a[i], k[i] - k[i-1] - a[i-1] * coef[2]))
        
        dio_a <- x[i-1] * coef[1]
        dio_b <- -x[i]
        dio_c <- k[i] - k[i-1] - x[i-1] * coef[2]
        
        sprintf("%sx + %sy = %s", asNumeric(dio_a), asNumeric(dio_b), 
                      asNumeric(dio_c))  # wolfram
        
        coef <- dio2d(dio_a, dio_b, dio_c, positive_yc = T, verbose)[3:4]
        
        if(is.na(coef[1])) {
            return(FALSE)
        }
        
        # At these coefficients, the dio2d algorithm breaks down, so we need to just return and
        # see if we can find any good rp values given the function we know from the limited hailstones we checked
        if(any(abs(coef) > 1e+12)) {
            rp_pot <- lapply(-50:50, function(n) k[i] + x[i]*(coef[1]*n + coef[2]))
            rp_pot <- new("mpfr", unlist(rp_pot))
            
            if(any(rp_pot > 1e+15) && any(rp_pot < -1e+15)) {
                break
            } else {
                print("rp_pot needs to be bigger, check it out manually")
                stop(rv)
            }
        }
    }
    
    if(is.null(rp_pot)) {
        rp_pot <- lapply(-10:10, function(n) k[i] + x[i]*(coef[1]*n + coef[2]))
        rp_pot <- new("mpfr", unlist(rp_pot))
    }
    
    if(rv %in% v) {
        n <- (forced_pos - k[length(k)] - (coef[2] * x[length(k)])) / (x[length(k)] * coef[1])
        
        if(n %% 1 == 0) {
            t_collide <- sapply(1:length(p), function(i) (forced_pos - p[i]) / (v[i] - rv))
            if(length(unique(t_collide)) == length(t_collide)) {
                if(verbose) print(t_collide)
                if(return_rp) return(forced_pos)
                print(rv)
                return(TRUE)
            } else {
                return(FALSE)
            }
        } else {
            return(FALSE)
        }
    }
    
    t_mat <- lapply(1:length(p), function(i) (rp_pot - p[i]) / (v[i] - rv))
    
    w <- which(sapply(1:length(rp_pot), function(i) {
        x <- mpfr(unlist(lapply(t_mat, function(x) x[i])))
        all(x >= 1 & abs(x %% 1 <= 0.0001))    
    }))
    
    if(any(w)) {
        t_collide <- rp_pot[w]
        if(length(unique(t_collide)) == length(t_collide)) {
            if(verbose) print(t_collide)
            if(return_rp) return(rp_pot[w])
            print(rv)
            return(TRUE)
        } else {
            return(FALSE)
        }
    } 
    
    return(FALSE)
}


dio2d <- function(a, b, c, positive_yc = FALSE, verbose = FALSE) {
    a <- mpfr(a, precBits = 128)
    b <- mpfr(b, precBits = 128)
    c <- mpfr(c, precBits = 128)
    
    if(a %% 1 != 0 || b %% 1 != 0 || c %% 1 != 0) {
        stop("Floating points in dio2d function, need to increase precBits?")
    }
    
    if((c / abs(GCD(asNumeric(a), asNumeric(b)))) %% 1 != 0) {
        if(verbose) print("No solutions, as c is not a multiple of the greatest common divisor of a and b.")
        return(NA)
    }
    
    # Shrink eqn if possible
    shrink <- GCD(GCD(asNumeric(a), asNumeric(b)), asNumeric(c))
    a <- a / shrink
    b <- b / shrink
    c <- c / shrink
    
    e <- extGCD(asNumeric(a), asNumeric(b))
    adj <- c / e[1]
    
    x0 <- adj * e[2]
    y0 <- adj * e[3]
    
    a <- a / e[1]
    b <- b / e[1]
    
    # general soln
    # x = x0 + n*b/gcd(a,b)
    # y = y0 - n*a/gcd(a,b)
    
    # scalar <- round(max(abs(c(x0 / b, y0 / a))))
    # 
    # if(x0 * b > 0 && y0 * a < 0) {
    #     scalar <- -scalar
    # }
    
    # scale general soln for y (as that is the coef we carry forwards, x may end up with large vals)
    scalar <- round(y0 / a)
    
    small_x0 <- x0 + b * scalar
    small_y0 <- y0 - a * scalar
    
    # Flip signs by multiplying n by -1 for both eqns
    if(b < 0 && -a < 0) {
        b <- -b
        a <- -a
    }
    
    if(positive_yc) {  # If we want to require y0 to be positive, manipulate constants here
        if(small_y0 < 0) {  
            small_x0 <- small_x0 + b
            small_y0 <- small_y0 - a
        }
    }
    
    if(verbose) print(sprintf("x = %sn + %s; y = %sn + %s", asNumeric(b), asNumeric(small_x0), asNumeric(-a), asNumeric(small_y0)))
    return(c(b, small_x0, -a, small_y0))
}


solve <- function(min_rv = -1000, max_rv = 1000, verbose = TRUE) {
    p <- sapply(positions, function(x) x[map["x"]])
    v <- sapply(velos, function(x) x[map["x"]])
    
    rng <- min_rv:max_rv
    res <- sapply(rng, function(rv) test_rv(rv, "x", verbose = T))
    possible_rv <- rng[res]
    
    for(rv in possible_rv) {
        possible_rp <- test_rv(rv, "x", return_rp = TRUE, verbose = F)
        
        for(rp in asNumeric(possible_rp)) {
            rp <- mpfr(rp, precBits = 128)
            
            t_collide <- asNumeric(mpfr(unlist(lapply(1:length(p), function(i) (rp - p[i]) / (v[i] - rv)))))
            
            # get Y coordinates of each hailstone at that time
            y_t <- py + vy * t_collide
            y_t <- asNumeric(y_t)
            
            rv_y <- ((y_t - shift(y_t, 1)) / (t_collide - shift(t_collide, 1)))[2:length(t_collide)]
            
            if(all(rv_y %% 1 == 0) && length(unique(rv_y)) == 1) {
                rv_y <- rv_y[1]
                
                # Find rv_z
                z_t <- pz + vz * t_collide
                
                rv_z <- (z_t[2] - z_t[1]) / (t_collide[2] - t_collide[1])
                
                # Work backwards to find rp_y, rp_z
                rp_y <- y_t - rv_y * t_collide
                rp_z <- z_t - rv_z * t_collide
                
                # Final check to ensure coordinates are correct
                stopifnot(length(unique(rp_y)) == 1 && length(unique(rp_z)) == 1)
                
                return(sprintf("rp = (%d, %d, %d), rv = (%d, %d, %d)", 
                               asNumeric(rp), asNumeric(rp_y[1]), asNumeric(rp_z[1]), 
                               asNumeric(rv), asNumeric(rv_y), asNumeric(rv_z)))
            }
        }
    }
    
    return("No solutions found")
}

solve(-50, 50, verbose = F)


# manual way of checking integer collisions -------------------------------

inc <- function(n, m) {
    # n = constant, m = coef of k
    # returns positive possible values of an initial rp that would collide with all hailstones, at various times
    
    r <- Reduce(intersect, lapply(1:length(n), function(i) {
        if(m[i] == 0) return(n[i])
        if(m[i] > 0) return(seq(n[i], 100000, m[i])) else return(rev(seq(100000, n[i], m[i])))
    }))[1:10]
    
    if(all(is.na(r))) {
        return(NA)
    }
    return(r)
}

n <- c(0, 1, 3)
m <- c(1, 4, 7)
inc(n, m)


