
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("", "\n")[[1]]  # Example
inp <- get_input("2016/07", parse = F, user = "blakeebets", cache = T)

has_abba <- function(s) {
    any(sapply(1:(nchar(s) - 3), function(i) {
        sec <- substr(s, i, i+3)
        ss <- strsplit(sec, "")[[1]]
        
        return(ss[1] == ss[4] && ss[2] == ss[3] && ss[1] != ss[2])        
    }))
}

supports <- function(x) {
    
    ext <- c()
    int <- c() 
    curr <- c()
    
    str <- strsplit(x, "")[[1]]
    for(i in str) {
        if(i == "[") {
            ext <- c(ext, paste(curr, collapse = ""))
            curr <- c()
        } else if(i == "]") {
            int <- c(int, paste(curr, collapse = ""))
            curr <- c()
        } else {
            curr <- c(curr, i)
        }
    }
    
    ext <- c(ext, paste(curr, collapse = "")) 
    
    abba_ext <- any(sapply(ext, has_abba))
    abba_int <- any(sapply(int, has_abba))
    
    return(abba_ext && !abba_int)
}

supports("abcabababxa[mnopacdxbdabba]qrrqst")

sum(sapply(inp, supports))

# 10:58 (60th)



# part 2 ------------------------------------------------------------------

aba <- function(s) {
    z <- lapply(1:(nchar(s) - 2), function(i) {
        sec <- substr(s, i, i+2)
        ss <- strsplit(sec, "")[[1]]
        
        if(ss[1] == ss[3] && ss[1] != ss[2]) {
            return(ss)
        }
    })
    
    z[!sapply(z, is.null)]
}


bab <- function(s, abas) {
    any(sapply(1:(nchar(s) - 2), function(i) {
        sec <- substr(s, i, i+2)
        ss <- strsplit(sec, "")[[1]]
        
        any(sapply(abas, function(a) {
            a[1] == ss[2] && a[2] == ss[1] && a[2] == ss[3]
        }))
    }))
}


supports2 <- function(x) {
    
    ext <- c()
    int <- c() 
    curr <- c()
    
    str <- strsplit(x, "")[[1]]
    for(i in str) {
        if(i == "[") {
            ext <- c(ext, paste(curr, collapse = ""))
            curr <- c()
        } else if(i == "]") {
            int <- c(int, paste(curr, collapse = ""))
            curr <- c()
        } else {
            curr <- c(curr, i)
        }
    }
    
    ext <- c(ext, paste(curr, collapse = "")) 
    abas <- unlist(lapply(ext, aba), recursive = F)
    
    any(sapply(int, function(s) bab(s, abas)))
}


sum(sapply(inp, supports2))

# 15:35 (27th)

