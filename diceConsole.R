## ---------------------------
## R> source('diceConsole.R')
## to stat game
## ---------------------------

rm(list=ls())
clear <- function() {
    if(tolower(.Platform$OS.type) == "windows")
        system("cls")
    else
        system("clear")
    invisible(NULL)
}
mkDice <- function(){
    x <- NULL
    show <- function(){
        if(x==6){
            if(sample(c(TRUE, FALSE), 1)){
                cat("|o o o|\n")
                cat("|     |\n")
                cat("|o o o|\n")
            } else {
                cat("|o   o|\n")
                cat("|o   o|\n")
                cat("|o   o|\n")
            }
        }
        if(x==5){
            cat("|o   o|\n")
            cat("|  o  |\n")
            cat("|o   o|\n")
        }
        if(x==4){
            cat("|o   o|\n")
            cat("|     |\n")
            cat("|o   o|\n")
        }
        if(x==3){
            if(sample(c(TRUE,FALSE), 1)){
                cat("|o    |\n")
                cat("|  o  |\n")
                cat("|    o|\n")
            } else {
                cat("|    o|\n")
                cat("|  o  |\n")
                cat("|o    |\n")
            }
        }
        if(x==2){
            if(sample(c(TRUE, FALSE), 1)){
                cat("|o    |\n")
                cat("|     |\n")
                cat("|    o|\n")
            } else {
                cat("|    o|\n")
                cat("|     |\n")
                cat("|o    |\n")
            }
        }
        if(x==1){
            cat("|     |\n")
            cat("|  o  |\n")
            cat("|     |\n")
        }
        invisible(NULL)
    }
    roll <- function(){
        x <<- sample(1:6, 1)
        show()
        invisible(x)
    }
#    roll()
    list(show=show, roll=roll)
}

board <- function(){
    status <- 1:9
    points <- 0

    show <- function(){
        ss <- as.character(status)
        ss[is.na(ss)] <- "-"
        cat("-------------------------------------\n")
        cat("| ", paste(ss, collapse=" | "), " |", sep="")
        cat("     ---> ", points, " <---", fill=TRUE)
        cat("-------------------------------------\n")
      
    }
    rm <- function(dice){
        wait <- TRUE
        while(wait){
            x <- readline(prompt="enter one number between 1-9 or two numbers between 1-6: ")
            if(nchar(x)==0){
                break
            }
            if(nchar(x)==2)
                x <- c(substr(x,1,1),substr(x,2,2))
            else if(nchar(x)==3)
                x <- c(substr(x,1,1),substr(x,3,3)) 
            x <- as.numeric(x)
           
            if((length(x)==1 && !(x %in% 1:9)) || (length(x)==2 && !all(x %in% 1:6)))
                cat("wrong input. try again.\n")
            else if((length(x)==1 && !(x == sum(dice) || x==abs(diff(dice)) || x==dice[1] || x==dice[2])) || (length(x)==2 && !((x[1]==dice[1] && x[2]==dice[2]) || (x[1]==dice[2] && x[2]==dice[1]))))
                cat("wrong input. try again.\n")
            else
                wait <- FALSE
        }
        if(length(x)==1 && !(x==sum(dice) || x==abs(diff(dice))))
            points <<- points+1
        
        for(i in seq_along(x)){
            if(x[i] %in% status)
                status[x[i]]  <<- NA
            else
                points <<- points+1
        }
        invisible(x)
    }
    Points <- function(){
        points
    }
    finish <- function(){
        all(is.na(status))
    }
    list(show=show, rm=rm, points=Points, finish=finish)
}

b <- board()
d1 <- mkDice()
d2 <- mkDice()

clear()
b$show()
cat("\nHow to play\n-----------\nroll the dices and enter one of:\n1. both numbers on the dices\n2. sum of both dices\n3. the positive difference of both dices.\n\neach number between 1-9 can be entered once. \notherwise, it increases the score.\n\n")

while(TRUE){
    readline(prompt="press [enter] to start: ")

    b <- board()
    d1 <- mkDice()
    d2 <- mkDice()
    
    while(!b$finish()){
        clear()
        b$show()
        cat(fill=TRUE)
        x <- readline(prompt="press [enter] to roll dices: ")
        clear()
        b$show()
        cat("\n\n")
        dd1 <- d1$roll()
        cat("-------", fill=TRUE)
        dd2 <- d2$roll()
        cat(fill=TRUE)
        b$rm(c(dd1,dd2))
        if(b$finish()){
            clear()
            ss <- sample(1:4, 1)
            if(ss==1)
                cat('                        ,.\n            .          :%%%.    .%%%.\n       __%%%(\        `%%%%%   .%%%%%\n     /a  ^  \'%        %%%% %: ,%  %%\"`\n    \'__..  ,\'%     .-%:     %-\'    %\n     ~~\"\"%:. `     % \'          .   `.\n         %% % `   %%           .%:  . \\.\n          %%:. `-\'   `        .%% . %: :\n          %(%,%...\"   `%,     %%\'   %% ) )\n           %)%%)%%\'   )%%%.....- \'   \"/ (\n           %(%)%%\ % / \\`%  \"%%% `   / \\))\n            %(%\'  % /-. \\      \'  \\ |-. \'.\n             `\\\'  |%   `()         \\|  `()\n                  ||    /          ()   /\n                  ()   0            |  o\n                   \\  /\\            o /\n                   o  `            /-|\n                ,-/ `           ,-/\n
', fill=TRUE)
            if(ss==2)
                cat("            ,=\"` `\"\"=,       o\n           /   ,==\"\"\"\'=;        , __\n   ~      /      ,--\'/=,)  o    \\`\\\\\"._     _,\n         |   .=\'/ <9(9.=\"       / _  |||;._//)\n        /     (J    ^\\ \\     o_/@ @  ///  |=(\n      .\'    .\' \\  \'=\'/  \'-.  ( (`__,     ,`\\|\n     /     /    \\`-;_      \\  \'.\\_/ |\\_.\'   \n~   /      |   /` _  \\      )   `\"\"```\n   | ,     ;  /`\\/ `\\ \\    /.-._///_\n   |/     \'   \\_,\\__/\\ \\.-\'.\'----\'`\n    \\|     \'.   \\     \\   /`-,   ~\n      `\\     _.-\'\\    (`-`  .\'\n        `-.-\' _.-\')__./,--\'\n     .--\'`,-\'`\'\"\"`    ` \\\n    /`\"`-`               |          ~\n   |                     /\n~  |     .-\'__         .\'\n    \\   ;\'\"`  `\"\"----\'`\n     \\   \\\n      \'.  `\\\n        )   `-.            ~\n       /       `-._\n      |     ,      `-,\n   ~  \\   .\' `\'\'----`\n       `.(\n         `", fill=TRUE)

            if(ss==3)
                cat("         .....\n         WWWWW\n        ((. .))    \n       ))) - (((	  \n     ((((`...')))       \n      ))))\\  /(((   	      \n      /    \\/    \\\n     / /\\      /\\ \\\n    / /  \\    /  \\ \\\n   @@@@  / \\/ \\  @@@@\n   (v   /      \\   v)  \n       @@@@@@@@@@\n      /          \\\n     /            \\\n    @@@@@@@@@@@@@@@@\n   /                \\\n  /                  \\\n @@@@@@@@@@@@@@@@@@@@@@\n /                    \\\n@@@@@@@@@@@@@@@@@@@@@@@@\n         v  v
", fill=TRUE)
            if(ss==4)
                cat("`\\\n  \\\\,\n   \\\\\\,^,.,,.\n   ,;7~((\\))`;;,,\n   ,(@\') ;)`))\\;;\',\n    )  . ),((  ))\\;,\n   /;`,,/7),)) )) )\\,,      ,,,... ,\n  (& )`   (,((,((;( ))\\,_,,;\'`    `\\\\,\n   `\"    ` ), ))),/( (            `)\\,\n          \'1/\';/;  `               ))),\n           (, (     /         )    ((/,\n          / \\                /     (((\'\n         ( 6--\\%  ,>     ,,,(     /\'))\\\'\n          \\,\\,/ ,/`----~`\\   \\    >,))))\'\n            \\/ /          `--7>\' /(((((\'\n            (,9             // /\'(\'((\\\\\\,\n             \\ \\,,         (/,/   \'\\`\\\\\'\\\n              `\\_)1        (_)Kk    `\\`\\\\`\\\n                `\\|         \\Z          `\\\n                  `          \"            `\n", fill=TRUE)
            cat("\n\n ----->> ", b$points(), " <<-----\n\n")
        }
    }
}
