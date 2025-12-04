
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- get_input("2024/23", parse = F, user = "bjebert", cache = F)
inp

inp <- strsplit("kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn", "\n")[[1]]  # Example


conns <- strsplit(inp, "-")
conn <- list()

for(cn in conns) {
    conn[[cn[1]]] <- c(conn[[cn[1]]], cn[2])
    conn[[cn[2]]] <- c(conn[[cn[2]]], cn[1])    
}

ct <- conn[substr(names(conn), 1, 1) == "t"]

groups3 <- lapply(1:length(ct), function(i) {
    unique(lapply(unlist(sapply(ct[[i]], function(y) {
        lapply(intersect(conn[[y]], ct[[i]]), function(z) c(names(ct)[i], y, z))
    }), recursive = F), sort))
})

length(unique(lapply(unlist(groups3, recursive = F), sort)))

# 16:09

# Part 2 ------------------------------------------------------------------

comp <- sort(unique(unlist(conn)))

best <- NULL

unique(unlist(sapply(1:length(comp), function(i) {
    unlist(sapply(1:length(comp), function(j) {
        if(i == j) {
            return()
        }
        
        x <- comp[i]
        y <- comp[j]
        
        if(!(y %in% conn[[x]])) {
            return()
        }
        
        network <- intersect(conn[[x]], conn[[y]])
        
        if(length(network) == 11) {
            net_size <- sapply(network, function(n) length(intersect(network, conn[[n]])))
            
            if(min(net_size) == 10) {
                return(paste(sort(c(x, y, network)), collapse = ","))
            }    
        }
    }))
})))

# 34:01

