rm(list=ls())
library(grid)
library(scales)
library(shiny)
library(htmlwidgets)

## A dice object with two dices--------------------------
## note the use of the lexical scoping ;-)
mkDices <- function(){
    x <- sample(1:6, 2, replace=TRUE)
    angle <- runif(2, min=0, max=360)
    count <- c(0,0)
    roll <- function(which=c(1,2)){
        if(any(which==1)){
            if(count[1]==0){
                x[1] <<- sample(1:6, 1, replace=TRUE)
                count[1] <<- 1
            }
            angle[1] <<- runif(1, min=0, max=360)
        } 
        if(any(which==2)){
            if(count[2]==0){
                x[2] <<- sample(1:6, 1, replace=TRUE)
                count[2] <<- 1
            }
            angle[2] <<- runif(1, min=0, max=360)
        }
    }
    
    plotDice1 <- function(w){
        x <- x[w]
        col <- if(count[w]==0) "lightblue" else "red"
        vp <- viewport(width=unit(.9, "npc"), height = unit(.9, "npc"))
        pushViewport(vp)
        grid.roundrect(gp=gpar(fill=alpha(col, .5), col="black", lwd=2))
        vp2 <- viewport(width=unit(.65, "npc"), height = unit(.65, "npc"))
        pushViewport(vp2)
        
        if(x==1)
            grid.circle(x=.5, y=.5, r=0.12, gp=gpar(fill="black",col="black"))
        
        if(x==2)
            grid.circle(x=c(1,0), y=c(0,1), r=0.12, gp=gpar(fill="black",col="black"))
       
        if(x==3)
            grid.circle(x=c(1,.5,0), y=c(0,.5,1), r=0.12, gp=gpar(fill="black",col="black"))
                
        if(x==4)
            grid.circle(x=c(0,0,1,1), y=c(0,1), r=0.12, gp=gpar(fill="black",col="black"))
        
        if(x==5)
            grid.circle(x=c(0,0,1,1,.5), y=c(0,1,0,1,.5), r=0.12, gp=gpar(fill="black",col="black"))
        
        if(x==6)
            grid.circle(x=c(0,0,0,1,1,1), y=c(0,.5,1), r=0.12, gp=gpar(fill="black",col="black"))
        popViewport(2)
        invisible(NULL)
}

    plotDices <- function(){
        grid.newpage()
        pushViewport(viewport(x=unit(.25, "npc"), width=unit(.5, "npc")))
        pushViewport(viewport(width=unit(.7, "npc"), height=unit(.7, "npc"), angle=angle[1]))
        plotDice1(1)
        popViewport(2)
        
        pushViewport(viewport(x=unit(.75, "npc"), width=unit(.5, "npc")))
        pushViewport(viewport(width=unit(.7, "npc"), height=unit(.7, "npc"), angle=angle[2]))
        plotDice1(2)
    }
    list(show=plotDices, roll=roll)
}

## ## some tests
## dd <- mkDices()
## dev.new(width=8, height=4, unit="in")
## dd$show()    # 2 blue
## dd$roll(1)
## dd$show()    # dice 1 is shown red because rolled
## dd$roll(1)   # dice 1 does NOT change value, because already rolled once
## dd$show()
## dd$roll(2)
## dd$show()
## dd$roll(2)
## dd$show()

## dd <- mkDices()
## dd$show()
## dd$roll(2)
## dd$show()
## dd$roll(2)
## dd$show()
## dd$roll(1)
## dd$show()
## dd$roll(1)
## dd$show()
## dev.off()

## a new Grob class for dynamic scaling of text size ---
resizingTextGrob <- function(...){
  grob(tg=textGrob(...), cl="resizingTextGrob")
}

drawDetails.resizingTextGrob <- function(x, recording=TRUE)
{
  grid.draw(x$tg)
}
preDrawDetails.resizingTextGrob <- function(x)
{
  h <- convertWidth(unit(1, "npc"), "mm", valueOnly=TRUE)
  fs <- rescale(h, to=c(40, 4), from=c(150, 10))
  pushViewport(viewport(gp = gpar(fontsize = 4*fs)))
}
postDrawDetails.resizingTextGrob  <- function(x) popViewport()

## ## some tests ---------------------------
## dev.new(width=4, height=4, unit="in")
## grid.newpage()
## g <- resizingTextGrob(label="test 1")
## grid.draw(g)
## ## resize plot window (width) with mouse for dynamic text scaling
## dev.off()

## A die object with two dices--------------------------
## note the use of the lexical scoping ;-)
board <- function(){
    status <- 1:9
    points <- 0
    plotBoard <- function(x){
        cols <- c("#00007F", "blue", "#007FFF", "cyan",
                  "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
        grid.newpage()
        pushViewport(viewport(width=unit(.95, "npc"), height=unit(.95, "npc"), clip=TRUE, gp=gpar(lwd=2)))
        grid.rect()
        for(i in 1:9){
            pushViewport(viewport(x=(i-1)/9, width=unit(1/9, "npc"), just="left", clip=TRUE))
            if(is.na(status[i]))
                grid.rect(gp=gpar(fill=alpha(cols[i], .7)))
            else{
                grid.rect()
                g <- resizingTextGrob(label=i)
                grid.draw(g)
            }
            popViewport()
        }
    }

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
    list(show=plotBoard, showConsole=show, rm=rm, points=Points, finish=finish)
}

## ## some tests
## dev.new(width=8, height=1, unit="in")
## b <- board()
## b$showConsole()
## b$show()
## b$rm(c(1,2)) # enter 12
## b$showConsole()
## b$show()
## b$rm(c(3,4)) # enter 34
## b$showConsole()
## b$show()
## b$rm(c(4,4)) # enter 8
## b$showConsole()
## b$show()


## SHINY ----------------------------------------
ui <- bootstrapPage(
    tags$script('
$(document).on("keyup", function(e) {
  if(e.keyCode == 13){
    Shiny.onInputChange("keyPressed", Math.random());
  }
});
'),
    tags$head(tags$script(src = 'message-handler.js;')),
    plotOutput("plotBoard", width="60%", height="auto"),
    br(),
    plotOutput("plot", height="auto"),
    br(),
    actionButton("roll1", "Roll 1"), 
    actionButton("roll2", "Roll 2"),
    br(),
    br(),
    radioButtons("choice", "\nChoose one:",
                 choices = c("Both"=1, 
                             "plus (+)" = 2,
                             "minus (-)" = 3,
                             "modulo (%%)" = 4,
                             "only one" = 5,
                             "none" = 6),
                 selected = 1, inline=TRUE),
    br(),
    textInput("txtIn", label = "")
)

server <- function(input, output, session) {
    dd <- mkDices()
    bb <- board()
    default1 <- default2 <- TRUE
    
    output$plotBoard <- renderPlot({
        bb$show()
    }, height = function() {
        session$clientData$output_plotBoard_width/9
    })
    
    observeEvent(input$roll1 || default1, {
        if(default1){
            default1 <<- FALSE
        } else {
            dd$roll(1)
        }
        output[["plot"]] <- renderPlot({
            dd$show()
        }, width= session$clientData$output_plot_width/2,
        height = function() {
            session$clientData$output_plot_width/4
        })
    })
    observeEvent(input$roll2  || default2, {
        if(default2){
            default2 <<- FALSE
        } else {
            dd$roll(2)
        }
        output[["plot"]] <- renderPlot({
            dd$show()
        }, width= session$clientData$output_plot_width/2,
        height = function() {
            session$clientData$output_plot_width/4
        })
    })

    txtIn <- reactiveVal()
    observeEvent(input[["keyPressed"]], {
      txtIn(input[["txtIn"]])
    })

}

shinyApp(ui, server)
