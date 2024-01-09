rm(list=ls())
source("utilities.R")
inp <- get_input("2023/25", parse = F, deesblake = F, cache = T)

inp <- strsplit("jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr", "\n")[[1]]

# Disconnect 3 wires and get 2 groups

from <- sapply(strsplit(inp, ": "), function(x) x[1])
to <- sapply(strsplit(inp, ": "), function(x) strsplit(x[2], " "))

wire_names <- sort(unique(c(from, unlist(to))))

wires <- setNames(lapply(wire_names, function(wn) {
    w <- which(wn == from)
    w_from <- c()
    
    if(length(w) > 0) {
        w_from <- to[[w]]
    }
    
    w_to <- from[which(sapply(to, function(x) wn %in% x))]
    
    return(sort(unique(c(w_to, w_from))))
}), wire_names)

# Can't brute force - too many options.

# Try to connect to other nodes, and note how we got there - if there is a path that seems common,
# that is probably one to eradicate

bfs <- function(start, wires, return_group_size = FALSE) {
    Q <- list(list(wire = start, path = start))
    v <- c(start)
    paths <- list()
    
    while(length(Q) > 0) {
        curr <- Q[[1]]
        Q <- Q[-1]
        
        nb <- wires[[curr[["wire"]]]]
        nb <- nb[!(nb %in% v)]
        
        v <- c(v, nb)
        
        paths[[curr[["wire"]]]] <- curr[["path"]][-1]
        
        Q <- c(Q, lapply(nb, function(x) list(wire = x, path = c(curr[["path"]], x))))
    }
    
    if(return_group_size) {
        return(length(paths))
    }
    
    conn <- lapply(paths[sapply(paths, length) >= 2], function(x) sapply(1:(length(x)-1), function(i) sprintf("%s-%s", x[i], x[i+1])))
    
    conn <- lapply(conn, function(x) {
        sapply(lapply(strsplit(x, "-"), sort), function(x) paste(x, collapse = "-"))
    })
    
    unlist(conn)
}

# Aggregate paths

conn_table <- rev(sort(table(unlist(lapply(wire_names, function(x) bfs(x, wires))))))

# Disconnect #1 connection and try again
conn_most_used <- strsplit(names(conn_table[1]), "-")[[1]]
conn1 <- conn_most_used[1]
conn2 <- conn_most_used[2]

wires_new <- copy(wires)
wires_new[[conn1]] <- setdiff(wires_new[[conn1]], conn2)
wires_new[[conn2]] <- setdiff(wires_new[[conn2]], conn1)

conn_table <- rev(sort(table(unlist(lapply(wire_names, function(x) bfs(x, wires_new))))))

# Disconnect #2 connection and try again
conn_most_used <- strsplit(names(conn_table[1]), "-")[[1]]
conn1 <- conn_most_used[1]
conn2 <- conn_most_used[2]

wires_new[[conn1]] <- setdiff(wires_new[[conn1]], conn2)
wires_new[[conn2]] <- setdiff(wires_new[[conn2]], conn1)

conn_table <- rev(sort(table(unlist(lapply(wire_names, function(x) bfs(x, wires_new))))))

# Disconnect #3 connection and count groups
conn_most_used <- strsplit(names(conn_table[1]), "-")[[1]]
conn1 <- conn_most_used[1]
conn2 <- conn_most_used[2]

wires_new[[conn1]] <- setdiff(wires_new[[conn1]], conn2)
wires_new[[conn2]] <- setdiff(wires_new[[conn2]], conn1)

prod(unique(sapply(wire_names[1:10], function(x) bfs(x, wires_new, return_group_size = T))))
