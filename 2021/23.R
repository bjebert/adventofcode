label <- "784235916"

cups <- as.character(strsplit(label, "")[[1]])
cups <- c(cups, 10:1e6)

cup_list <- as.list(c(cups[2:1e6], cups[1]))
names(cup_list) <- cups

cup_prev <- as.list(c(cups[1e6], cups[1:(1e6-1)]))
names(cup_prev) <- cups

# Dummy -------------------------------------------------------------------


# label <- "389125467"
# 
# cups <- as.character(strsplit(label, "")[[1]])
# 
# cup_list <- as.list(c(cups[2:9], cups[1]))
# names(cup_list) <- cups
# 
# cup_prev <- as.list(c(cups[9], cups[1:8]))
# names(cup_prev) <- cups

# ... ---------------------------------------------------------------------

current <- names(cup_list)[1]

start <- Sys.time()

for(i in 1:1e7) {
    pickup_1 <- cup_list[[current]]
    pickup_2 <- cup_list[[pickup_1]]
    pickup_3 <- cup_list[[pickup_2]]
    
    current_cup <- as.numeric(current)
    destination_order <- ((current_cup - 1):(current_cup - 4) - 1) %% 9 + 1
    destination_cup <- as.character(destination_order[which(!(as.character(destination_order) %in% c(pickup_1, pickup_2, pickup_3)))[1]])

    z <- cup_prev[[pickup_1]]
    
    cup_prev[[cup_list[[destination_cup]]]] <- pickup_3
    cup_prev[[cup_list[[pickup_3]]]] <- cup_prev[[pickup_1]]
    cup_prev[[pickup_1]] <- destination_cup
    
    cup_list[[z]] <- cup_list[[pickup_3]]
    cup_list[[pickup_3]] <- cup_list[[destination_cup]]
    cup_list[[destination_cup]] <- pickup_1
    
    current <- cup_list[[current]]
} 

print(Sys.time() - start)



# print_list(cup_list)

print_list <- function(cup_list) {
    z <- names(cup_list)[1]
    s <- z
    for(i in 2:length(cup_list)) {
        z <- cup_list[[z]]
        s <- sprintf("%s%s", s, z)
    }
    s
}