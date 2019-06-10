rm(list=ls())
library(grid)
library(scales)
library(shiny)
library(png)


## load images
camel <- readPNG("camel.png" )
uniquehorn <- readPNG("uniquehorn.png" )
frog <- readPNG("frog.png" )

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

## The game in one ------------------------------------
## note the use of the lexical scoping ;-)
board <- function(){

    wrong <- FALSE
    dices <- sample(1:6, 2, replace=TRUE)
    angle <- runif(2, min=0, max=360)
    count <- c(0,0)
    DiceRoll <- function(which=c(1,2)){
        if(any(which==1)){
            if(count[1]==0){
                dices[1] <<- sample(1:6, 1, replace=TRUE)
                count[1] <<- 1
            }
            angle[1] <<- runif(1, min=0, max=360)
        } 
        if(any(which==2)){
            if(count[2]==0){
                dices[2] <<- sample(1:6, 1, replace=TRUE)
                count[2] <<- 1
            }
            angle[2] <<- runif(1, min=0, max=360)
        }
    }
    DiceReset <- function(){
        dices <<- sample(1:6, 2, replace=TRUE)
        angle <<- runif(2, min=0, max=360)
        count <<- c(0,0)
    }
    
    DicePlot1 <- function(w){
        x <- dices[w]
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

    DicePlot <- function(){
#        grid.newpage()
        pushViewport(viewport(x=unit(.25, "npc"), width=unit(.4, "npc")))
        pushViewport(viewport(width=unit(.7, "npc"), height=unit(.7, "npc"), angle=angle[1]))
        DicePlot1(1)
        popViewport(2)
        
        pushViewport(viewport(x=unit(.75, "npc"), width=unit(.4, "npc")))
        pushViewport(viewport(width=unit(.7, "npc"), height=unit(.7, "npc"), angle=angle[2]))
        DicePlot1(2)
        popViewport(2)
    }


    status <- 1:9
    points <- 0
    BoardPlot <- function(){
        cols <- c("#00007F", "blue", "#007FFF", "cyan",
                  "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
#        grid.newpage()
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
        popViewport()
    }

    BoardConsole <- function(){
        ss <- as.character(status)
        ss[is.na(ss)] <- "-"
        cat("-------------------------------------\n")
        cat("| ", paste(ss, collapse=" | "), " |", sep="")
        cat("     ---> ", points, " <---", fill=TRUE)
        cat("-------------------------------------\n")
      
    }
    BoardRm <- function(x, what=1){
        none <- FALSE
        if(nchar(x)==2)
            x <- c(substr(x,1,1),substr(x,2,2))
        else if(nchar(x)==3)
            x <- c(substr(x,1,1),substr(x,3,3)) 
        x <- as.numeric(x)

        if(any(is.na(x)) && what!=5){
            wrong <<- TRUE
            what <- 0
        }
        
        if(what==1){
            if(!(length(x)==2 && x[1]!= x[2] && (all(x==dices) || all(rev(x)==dices))))
                wrong <<- TRUE
        }

        if(what==2){
            if(!(length(x)==1 && x==sum(dices)))
                wrong <<- TRUE
        }

        if(what==3){
            if(!(length(x)==1 && x==abs(diff(dices))))
                wrong <<- TRUE
        }

        if(what==4){
            if(!(length(x)==1 && (x==dices[1] || x==dices[2])))
                wrong <<- TRUE
            points <<- points + 1
        }

        if(what==5){
            points <<- points + 2
            none <- TRUE
        }

        if(none)
            DiceReset()
        else
            if(!wrong){
                for(i in seq_along(x)){
                    if(x[i] %in% status)
                        status[x[i]]  <<- NA
                }
                DiceReset()
            }
    }
    Points <- function(){
        points
    }
    finish <- function(){
        all(is.na(status))
    }
    
    Plot <- function(){
        grid.newpage()
        pushViewport(viewport(y=unit(.9, "npc"), height=unit(.2, "npc")))
        BoardPlot()
        popViewport()
        pushViewport(viewport(y=unit(.35, "npc"), height=unit(.7, "npc"),
                              width=unit(1, "npc")))
        if(finish()){
            pushViewport(viewport())
            if(sample(c(TRUE,FALSE), 1))
                grid.raster(uniquehorn, gp=gpar(alpha=.5))
            else
                grid.raster(frog, gp=gpar(alpha=.5))
            popViewport()
            return(NULL)
        }
        if(wrong){
            pushViewport(viewport())
            grid.raster(camel, gp=gpar(alpha=.5))
            popViewport()
            wrong <<- FALSE
        }
        DicePlot()
        popViewport(1)
    }
    
    list(plot=Plot,
         boardRm=BoardRm,
         diceRoll=DiceRoll,
         diceReset=DiceReset)
}




## ## some tests
## dev.new(width=10, height=6, unit="in")
## b <- board()
## b$plot()
## b$diceRoll(1)
## b$boardRm("5") # enter 12
## b$plot()



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
    plotOutput("plot", width="50%", height="auto"),
    br(),
    actionButton("roll1", "Roll 1"), 
    actionButton("roll2", "Roll 2"),
    br(),
    br(),
    radioButtons("choice", "\nChoose one:",
                 choices = c("Both"=1, 
                             "sum (+)" = 2,
                             "difference (-)" = 3,
                             "only one" = 4,
                             "none" = 5),
                 selected = 1, inline=TRUE),
    textInput("txtIn", label = "", placeholder = "")
)

server <- function(input, output, session) {
    bb <- board()
    default1 <- default2 <- TRUE
    
    output$plot <- renderPlot({
        bb$plot()
    }, height = function() {
        session$clientData$output_plot_width*.6
    })
    
    observeEvent(input$roll1 || default1, {
        if(default1){
            default1 <<- FALSE
        } else {
            bb$diceRoll(1)
        }
        output$plot <- renderPlot({
            bb$plot()
        }, height = function() {
            session$clientData$output_plot_width*.6
        })
    })
    observeEvent(input$roll2  || default2, {
        if(default2){
            default2 <<- FALSE
        } else {
            bb$diceRoll(2)
        }
        output$plot <- renderPlot({
            bb$plot()
        }, height = function() {
            session$clientData$output_plot_width*.6
        })
    })

    txtIn <- reactiveVal()
    observeEvent(input[["keyPressed"]], {
      txtIn(input[["txtIn"]])
      bb$boardRm(txtIn(), input$choice)
      output$plot <- renderPlot({
          bb$plot()
      }, height = function() {
          session$clientData$output_plot_width*.6
      })
      updateTextInput(session,"txtIn", value="")
    })
}

shinyApp(ui, server)
