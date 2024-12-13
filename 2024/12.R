# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- get_input("2024/12", parse = F, user = "bjebert", cache = F)

inp <- strsplit("RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE", "\n")[[1]]  # Example

map <- mat2map(inp2mat(inp))

get_area <- function(plant) {
    sum(map == plant)
}

get_perimeter <- function(plant) {
    n <- names(map)[map == plant]
    sum(sapply(n, function(p) {
        nb <- get_4nb_str(p)
        
        # if nb is NOT same as plant then add one to perimeter
        sum(!(nb %in% names(map))) + sum(map[nb[nb %in% names(map)]] != plant)
    }))
}


get_group <- function(curr, p, horizontal = TRUE) {
    
    group <- curr
    left <- T
    right <- T
    i <- 1
    
    while(left || right) {
        if(horizontal) {
            apos <- pos2str(str2pos(curr) + move_map[["L"]] * i)
            bpos <- pos2str(str2pos(curr) + move_map[["R"]] * i)
        } else {
            apos <- pos2str(str2pos(curr) + move_map[["U"]] * i)
            bpos <- pos2str(str2pos(curr) + move_map[["D"]] * i)
        }
        if(!(apos %in% p)) {
            left <- F
        } else if(left) {
            group <- c(group, apos)
        }
        
        if(!(bpos %in% p)) {
            right <- F
        } else if(right) {
            group <- c(group, bpos)
        }
        
        i <- i + 1
    }
    return(group)
}


get_sides <- function(plant) {
    print(plant)
    n <- sort(names(map)[map == plant])
    
    fence_mat <- t(sapply(n, function(p) {
        nb <- get_4nb_str(p)
        fence_dirs <- unname(!(nb %in% names(map)) | map[nb] != plant)
    }))
    
    colnames(fence_mat) <- c("N", "S", "E", "W")
    
    sum(sapply(colnames(fence_mat), function(dir) {
        p <- rownames(fence_mat)[fence_mat[, dir]]
        gmap <- setNames(1:length(p), p)
        
        for(i in 1:length(p)) {
            if(gmap[i] != i) {
                next
            }
            
            grp <- get_group(p[i], p, dir %in% c("N", "S"))
            gmap[p %in% grp] <- min(gmap[p %in% grp], i)
        }
        
        length(unique(gmap))
        
        # check for straight lines    
    }))
}


get_regions <- function(plant) {
    # for each plant, see which ones we can reach?
    
    regions <- setNames(1:length(names(map)[map == plant]), names(map)[map == plant])
    
    for(i in 1:length(regions)) {
        if(regions[[i]] != i) {
            next
        }
        
        Q <- names(regions)[i]
        v <- names(regions)[i]
        
        while(length(Q) > 0) {
            curr <- Q[1]
            Q <- Q[-1]
            
            if(curr %in% names(regions)) {
                regions[[curr]] <- min(regions[[curr]], i)
            }
            
            nb <- get_4nb_str(curr)
            nb <- nb[!(nb %in% v)]
            nb <- nb[nb %in% names(map)]
            nb <- nb[map[nb] == plant]
            
            if(length(nb) > 0) {
                Q <- unique(c(Q, nb))
                v <- unique(c(v, nb))
            }
        }    
    }
    
    j <- 1
    for(rid in unique(regions)) {
        map[names(regions)[regions == rid]] <<- sprintf("%s%s", map[names(regions)[regions == rid]], j)
        j <- j + 1
    }
}


plants <- sort(unique(map))
regions <- sapply(plants, get_regions)
plants <- sort(unique(map))

areas <- sapply(plants, get_area)
sides <- sapply(plants, get_sides)

sum(areas * sides)

# perimeters <- sapply(plants, get_perimeter)

