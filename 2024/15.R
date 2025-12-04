
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- get_input("2024/15", parse = F, user = "bjebert", cache = F)
inp

inp <- strsplit("########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<", "\n")[[1]]  # Example

w <- which(inp == "")

map <- mat2map(inp2mat(inp[1:(w-1)]))

moves <- inp[(w+1):length(inp)]
moves <- unlist(strsplit(moves, ""))

walls <- names(map)[map == '#']
boxes <- names(map)[map == 'O']
fish <- names(map)[map == '@']

disp <- function() {
    print(map2mat(c(setNames(rep('[', length(boxL)), boxL),
                    setNames(rep(']', length(boxR)), boxR),
                    setNames(rep('#', length(walls)), walls),
                    setNames(rep('@', length(fish)), fish))))
}

for(m in moves) {
    # print(map2mat(c(setNames(rep('O', length(boxes)), boxes),
    #                 setNames(rep('#', length(walls)), walls),
    #                 setNames(rep('@', length(fish)), fish))))
    
    target <- str2pos(fish) + move_map[[m]]
    is_wall <- pos2str(target) %in% walls
    
    if(is_wall) {
        next
    }
    
    # find how many boxes we can move - if any.  we need a clean space underneath or a wall
    iter <- 1
    boxes_to_move <- c()
    
    # preemptively move fish
    oldfish <- copy(fish)
    fish <- pos2str(target)
    
    while(iter < 200) {
        newpos <- pos2str(str2pos(oldfish) + move_map[[m]] * iter)
        if(newpos %in% boxes) {
            boxes_to_move <- c(boxes_to_move, newpos)
        } else if(newpos %in% walls) {
            boxes_to_move <- NULL
            fish <- copy(oldfish)
            break
        } else {
            break
        }
        iter <- iter + 1
    }
    
    if(!is.null(boxes_to_move)) {
        
        bnew <- sapply(boxes_to_move, function(b) pos2str(str2pos(b) + move_map[[m]]))
        boxes[boxes %in% boxes_to_move] <- bnew
    }
}

b <- t(sapply(strsplit(boxes, ","), as.numeric)) - 1
sum(rowSums(b * matrix(rep(c(100, 1), length = nrow(b) * 2), ncol = 2, byrow = T)))


# part 2 ------------------------------------------------------------------


inp <- strsplit("#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^", "\n")[[1]]  # Example

w <- which(inp == "")

map <- mat2map(inp2mat(inp[1:(w-1)]))

moves <- inp[(w+1):length(inp)]
moves <- unlist(strsplit(moves, ""))

walls <- names(map)[map == '#']
boxes <- names(map)[map == 'O']
fish <- names(map)[map == '@']

# resize

wallmat <- t(sapply(strsplit(walls, ","), as.numeric))
wallmat[,2] <- wallmat[,2] * 2 - 1
wallmat_clone <- copy(wallmat)
wallmat_clone[,2] <- wallmat_clone[,2] + 1
wallmat <- rbind(wallmat, wallmat_clone)
walls <- sapply(1:nrow(wallmat), function(x) pos2str(wallmat[x,]))

t(sapply(strsplit(boxes, ","), as.numeric))

fold <- str2pos(fish)
fish <- pos2str(c(fold[1], fold[2] * 2 - 1))

boxmat <- t(sapply(strsplit(boxes, ","), as.numeric))
boxmat[,2] <- boxmat[,2] * 2 - 1
boxmat_clone <- copy(boxmat)
boxmat_clone[,2] <- boxmat_clone[,2] + 1

boxL <- sapply(1:nrow(boxmat), function(x) pos2str(boxmat[x,]))
boxR <- sapply(1:nrow(boxmat_clone), function(x) pos2str(boxmat_clone[x,]))


disp()

for(m in moves) {
    
    target <- str2pos(fish) + move_map[[m]]
    is_wall <- pos2str(target) %in% walls
    
    if(is_wall) {
        next
    }
    
    if(m %in% c("<", ">")) {
        
        # find how many boxes we can move - if any.  we need a clean space underneath or a wall
        iter <- 1
        boxes_to_move <- c()
        
        # preemptively move fish
        oldfish <- copy(fish)
        fish <- pos2str(target)
        
        while(iter < 200) {
            newpos <- pos2str(str2pos(oldfish) + move_map[[m]] * iter)
            if(newpos %in% c(boxL, boxR)) {
                boxes_to_move <- c(boxes_to_move, newpos)
            } else if(newpos %in% walls) {
                boxes_to_move <- NULL
                fish <- copy(oldfish)
                break
            } else {
                break
            }
            iter <- iter + 1
        }
        if(!is.null(boxes_to_move)) {
            bnew <- sapply(boxes_to_move, function(b) pos2str(str2pos(b) + move_map[[m]]))
            
            boxL[boxL %in% boxes_to_move] <- bnew[names(bnew) %in% boxL[boxL %in% boxes_to_move]]
            boxR[boxR %in% boxes_to_move] <- bnew[names(bnew) %in% boxR[boxR %in% boxes_to_move]]
        }
    } else if(m %in% c("v", "^")) {
        
        if(!(pos2str(target) %in% c(walls, boxL, boxR))) {  # empty space, just move it
            fish <- pos2str(target)
            next
        }
        
        # keep track of all boxes that are being moved
        boxes_to_move <- NULL
        iter <- 1
        oldfish <- copy(fish)
        fish <- pos2str(target)  # pre-emptively move; but if we hit a block before hitting all '.' then revert
        
        face <- copy(oldfish)
        
        while(iter < 200) {
            f <- sapply(face, function(x) pos2str(str2pos(x) + move_map[[m]]))
            
            if(any(f %in% walls)) {
                boxes_to_move <- NULL
                fish <- copy(oldfish)
                break
            }
            
            new_face <- NULL
            
            if(any(f %in% boxL)) {
                targetL <- f[f %in% boxL]
                new_face <- c(new_face, targetL)
                new_face <- c(new_face, sapply(targetL, function(x) pos2str(str2pos(x) + move_map[['>']])))
            }
            
            if(any(f %in% boxR)) {
                targetR <- f[f %in% boxR]
                new_face <- c(new_face, targetR)
                new_face <- c(new_face, sapply(targetR, function(x) pos2str(str2pos(x) + move_map[['<']])))
            }
            
            if(!any(c(f %in% c(boxL, boxR)))) {
                break
            }
            
            face <- copy(new_face)
            boxes_to_move <- c(boxes_to_move, new_face)
            
            iter <- iter + 1
        }
        
        if(!is.null(boxes_to_move)) {
            boxL[boxL %in% boxes_to_move] <- unname(sapply(boxL[boxL %in% boxes_to_move], function(b) pos2str(str2pos(b) + move_map[[m]])))
            boxR[boxR %in% boxes_to_move] <- unname(sapply(boxR[boxR %in% boxes_to_move], function(b) pos2str(str2pos(b) + move_map[[m]])))
        }
        
    }
    
    # disp()
    
}

b <- t(sapply(strsplit(boxL, ","), as.numeric)) - 1
sum(rowSums(b * matrix(rep(c(100, 1), length = nrow(b) * 2), ncol = 2, byrow = T)))

