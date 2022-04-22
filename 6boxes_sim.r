library(ggplot2)
library(gridExtra)
library(scales)
library(comprehenr)
library(glue)
library(tcltk)

# open X11 window for GUI
X11(title = "6 boxes toy model", width=10, height=5)

# first extraction
t = 1

# initial uniform probability
p_H       = list()
p_H[[t]]  = to_vec(for(i in 0:5) 1/6)

# box probability conditioned by extracted color
p_H_E      = data.frame(
    w = to_vec(for(i in 0:5) i/5),
    b = to_vec(for(i in 0:5) 1-i/5)
)

cat('\n################################################################\n')
cat('                 Welcome to the Six Boxes Game')
cat('\n################################################################\n\n')

cat("In this application you are the audience!\n")
cat("One box will be selected and you can extract n balls from it \n")
cat("Your choices are:\n")
cat("      - number of extractions (n) \n")
cat("      - quit (q)\n\n")

repeat{
    # read extraction result from user input
    cat(glue("Press a valid key:"))
    input = readLines("stdin", n=1);

    if(input=="q"){
        cat("\n\n\n")
        cat("################################################################\n")
        cat("#                                                              #\n")
        cat("#       May the stochasticity be with you! ฅʕᵔﻌᵔʔ              #\n")
        cat("#                                                              #\n")
        cat("################################################################\n")
        cat("\n\n\n")
        stop()
    }else if(round(as.numeric(input))==as.numeric(input)){
        t = 1

        # initialize results df
        df = data.frame(matrix(ncol = 8, nrow = 0))
        colnames(df) = c(" ", "E", to_vec(for(i in  0:5) glue("H{i}")))
        df[t, ]       = c("prob"," ", round(p_H[[t]], 4))

        # sort box
        i_H = sample(1:6, 1)

        # loop over input=n extraction
        for(t in 2:input){
            # perform extraction
            e = ifelse(rbinom(n=1, size=1, prob=p_H_E$w[i_H]), "w", "b")
            q
            # probability of the extraction
            p_E      = sum(to_vec(for(i in 1:6) eval(parse(text=paste("p_H_E$", e)))[i]*p_H[[t-1]][i]))

            # boxes probability at t extraction
            p_H[[t]] = to_vec(for(i in 1:6)  eval(parse(text=paste("p_H_E$", e)))[i]*p_H[[t-1]][i]/p_E)

            # save results
            df[t, ]       = c("prob", e, round(p_H[[t]], 4))
        }

        # print probabilities
        print(df)

        # plot
        options(repr.plot.width=15, repr.plot.height=10)   
        plots = list()
        for (i in 1:6){
            p <- ggplot() +   
                theme_linedraw() +                                            
                geom_point(aes(x=1:t-1, y=to_vec(for(t_i in 1:t) p_H[[t_i]][i])), size=2.5, color="cornflowerblue" ) +   
                geom_line(aes(x=1:t-1, y=to_vec(for(t_i in 1:t) p_H[[t_i]][i])), linetype="dashed", color="steelblue") +                                                          
                theme(text = element_text(size=11)) +
                ggtitle(glue("H{i-1}")) +
                theme(plot.title = element_text(hjust = 0.5))  +
                ylab("probability") +
                ylim(0, 1) +
                xlab("extraction")
                #scale_x_continuous(breaks=1:t-1, labels=1:t-1)
                
            plots[[i]] = ggplotGrob(p)
        }

        grid.arrange(grobs=plots, ncol=3)

    }else if(input=="print"){
        print(df)
    }else{
        cat("invalid input, please try again: (b -> black, w-> white, q-> quit\n")
    }
}