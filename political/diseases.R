# =========================================================================== #
#                            LIBRARIES AND SET UP                             #
# =========================================================================== #
library(ggplot2)
library(deSolve)
library(reshape2)
library(plotly)

# =========================================================================== #
#                                  FUNCTION                                   #
# =========================================================================== #
sirModel <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S + I + R
    lambda <- beta * (I / N) 
    dS <- -lambda * S
    dI <- lambda * S - gamma * I
    dR <- gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}

# =========================================================================== #
#                                  Throtha                                    #
# =========================================================================== #
pop <- 3000

iniStateVal <- c(S = pop, I = 1, R = 0)
parameters <- c(gamma = 0.07, beta = 0.55)
time <- seq(from = 1, to = 52 * 3, by = 1)




#Solving the differential equations
output<-as.data.frame(ode(y=initial_state_values,func = sir_model,parms=parameters,times = time))

output$R <- output$R * 0.01

output <- round(output)

# View(output)



out_long=melt(output,id="time")
# To plot the proportion of susceptible, infected and recovered individuals over time
ggplotly(
  ggplot(data = out_long,          
       aes(x = time, y = value / pop, colour = variable, group = variable)) +  
  geom_line() +xlab("Time (days)")+ylab("Proportion of the population")+scale_color_discrete(name="State")
)
# from https://towardsdatascience.com/extending-the-basic-sir-model-b6b32b833d76
