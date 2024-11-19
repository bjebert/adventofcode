
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("", "\n")[[1]]  # Example
inp <- get_input("2015/21", parse = F, user = "blakeebets", cache = T)

shop <- strsplit("Weapons:    Cost  Damage  Armor
Dagger        8     4       0
Shortsword   10     5       0
Warhammer    25     6       0
Longsword    40     7       0
Greataxe     74     8       0

Armor:      Cost  Damage  Armor
Leather      13     0       1
Chainmail    31     0       2
Splintmail   53     0       3
Bandedmail   75     0       4
Platemail   102     0       5

Rings:      Cost  Damage  Armor
Damage +1    25     1       0
Damage +2    50     2       0
Damage +3   100     3       0
Defense +1   20     0       1
Defense +2   40     0       2
Defense +3   80     0       3", "\n")[[1]]

boss_hp <- 100
boss_dmg <- 8
boss_armor <- 2

fight <- function(hp, dmg, armor, boss_hp, boss_dmg, boss_armor) {
    while(TRUE) {
        
        # player turn
        dealt <- pmax(1, dmg - boss_armor)
        boss_hp <- boss_hp - dealt
        
        if(boss_hp <= 0) {
            return(1)
        }
        
        # boss turn
        dealt <- pmax(1, boss_dmg - armor)
        hp <- hp - dealt
        
        if(hp <= 0) {
            return(0)  # boss win
        }
    }
}

weapons <- list(c(8, 4), c(10, 5), c(25, 6), c(40, 7), c(74, 8))
armors <- list(c(0, 0), c(13, 1), c(31, 2), c(53, 3), c(75, 4), c(102, 5))
rings <- list(c(0, 0, 0), c(25, 1, 0), c(50, 2, 0), c(100, 3, 0), c(20, 0, 1), c(40, 0, 2), c(80, 0, 3))

ring_combos <- c(combn(rings, 0, simplify = F),
                 combn(rings, 1, simplify = F),
                 combn(rings, 2, simplify = F))

min_cost <- Inf

for(w in weapons) {
    for(a in armors) {
        for(r in ring_combos) {
            
            if(is.na(r)) {
                r_cost <- 0
                r_dmg <- 0
                r_armor <- 0 
            } else {
                r_cost <- sum(sapply(r, function(x) x[1]))
                r_dmg <- sum(sapply(r, function(x) x[2]))
                r_armor <- sum(sapply(r, function(x) x[3]))
            }
                        
            hp <- 100
            dmg <- w[2] + r_dmg
            armor <- a[2] + r_armor
            
            res <- fight(hp, dmg, armor, boss_hp, boss_dmg, boss_armor)
            
            if(res == 1) {
                cost <- w[1] + a[1] + r_cost
                
                if(cost < min_cost) {
                    min_cost <- cost
                    print(cost)
                }
            }
        }
    }
}

# 10:00 (3rd)

max_cost <- 0

for(w in weapons) {
    for(a in armors) {
        for(r in ring_combos) {
            
            r_cost <- sum(sapply(r, function(x) x[1]))
            r_dmg <- sum(sapply(r, function(x) x[2]))
            r_armor <- sum(sapply(r, function(x) x[3]))
            
            hp <- 100
            dmg <- w[2] + r_dmg
            armor <- a[2] + r_armor
            
            res <- fight(hp, dmg, armor, boss_hp, boss_dmg, boss_armor)
            
            cost <- w[1] + a[1] + r_cost
            
            if(res == 0) {
                # print(sprintf("%s / %s / %s / %s", res, dmg, armor, cost))
                if(cost > max_cost) {
                    max_cost <- cost
                    print(cost)
                }
            }
        }
    }
}

# 19:42 (17th)
