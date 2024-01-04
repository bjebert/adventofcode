input <- readLines("22.txt")

deck_1 <- as.numeric(input[2:26])
deck_2 <- as.numeric(input[29:53])

# input <- strsplit("Player 1:
# 9
# 2
# 6
# 3
# 1
# 
# Player 2:
# 5
# 8
# 4
# 7
# 10", "\n")[[1]]
# 
# deck_1 <- as.numeric(input[2:6])
# deck_2 <- as.numeric(input[9:13])

# Part 2 ------------------------------------------------------------------


recursive_combat <- function(deck_1, deck_2, history = c(), this_game = 1, next_game = 2) {
    
    if(this_game != 1) {
        if(max(deck_1) > max(deck_2)) {
            return(list("winner" = 1,
                        "deck" = deck_1,
                        "history" = history,
                        "this_game" = this_game,
                        "next_game" = next_game))
        }
    }
    
    while(length(deck_1) > 0 && length(deck_2) > 0) {
        
        history_key <- sprintf("%s|%s|%s", paste(deck_1, collapse = ","), paste(deck_2, collapse = ","), this_game)
        if(length(history) == 100000) {
            browser()
        }
        
        if(history_key %in% history) {
            return(list("winner" = 1,
                        "history" = history,
                        "this_game" = this_game,
                        "next_game" = next_game))
        }
        
        history <- c(history, history_key)
        
        draw_1 <- deck_1[1]
        draw_2 <- deck_2[1]
        
        #' Recursive sub-combat to settle winner?
        
        if(length(deck_1) - 1 >= draw_1 && length(deck_2) - 1 >= draw_2) {
            result <- recursive_combat(deck_1[2:min(length(deck_1), (draw_1+1))], deck_2[2:min(length(deck_2), (draw_2+1))], history, next_game, next_game + 1)
            
            history <- result[["history"]]
            next_game <- result[["next_game"]]
            
            if(result[["winner"]] == 1) {
                deck_1 <- c(deck_1[2:length(deck_1)], deck_1[1], deck_2[1])
                deck_2 <- c(deck_2[2:length(deck_2)])
            } else if(result[["winner"]] == 2) {
                deck_2 <- c(deck_2[2:length(deck_2)], deck_2[1], deck_1[1])
                deck_1 <- c(deck_1[2:length(deck_1)])
            }
        } else if(deck_1[1] > deck_2[1]) {
            if(length(deck_1) > 1) {
                deck_1 <- c(deck_1[2:length(deck_1)], deck_1[1], deck_2[1])
            } else {
                deck_1 <- c(deck_1[1], deck_2[1])
            }
            
            if(length(deck_2) == 1) {
                deck_2 <- c()
                break
            }
            
            deck_2 <- c(deck_2[2:length(deck_2)])
            
        } else if(deck_2[1] > deck_1[1]) {
            if(length(deck_2) > 1) {
                deck_2 <- c(deck_2[2:length(deck_2)], deck_2[1], deck_1[1])
            } else {
                deck_2 <- c(deck_2[1], deck_1[1])
            }
            
            if(length(deck_1) == 1) {
                deck_1 <- c()
                break
            }
            
            deck_1 <- c(deck_1[2:length(deck_1)])
            
        } else {
            warning("Should not be falling through to here")
        }
    }
    
    if(length(deck_1) == 0) {
        return(list("winner" = 2,
                    "deck" = deck_2,
                    "history" = history,
                    "this_game" = this_game,
                    "next_game" = next_game))
    }
    
    if(length(deck_2) == 0) {
        return(list("winner" = 1,
                    "deck" = deck_1,
                    "history" = history,
                    "this_game" = this_game,
                    "next_game" = next_game))
    }
}


debug(recursive_combat)
undebug(recursive_combat)
result <- recursive_combat(deck_1, deck_2)

result[["deck"]]

sum(50:1 * result[["deck"]])

# Part 1 ------------------------------------------------------------------


while(length(player_1) > 0 || length(player_2) > 0) {
    
    if(player_1[1] > player_2[1]) {
        player_1 <- c(player_1[2:length(player_1)], player_1[1], player_2[1])
        
        if(length(player_2) == 1) {
            player_2 <- c()
            break
        }
        
        player_2 <- c(player_2[2:length(player_2)])
        
    } else {
        player_2 <- c(player_2[2:length(player_2)], player_2[1], player_1[1])
        
        
        if(length(player_1) == 1) {
            player_1 <- c()
            break
        }
        
        player_1 <- c(player_1[2:length(player_1)])
    }
}


sum(length(player_1):1 * player_1)
