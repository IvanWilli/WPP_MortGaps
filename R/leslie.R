library(tidyverse)
# project pop for 3 years given pop with 3 ages
# surv ratio
Sx <- matrix(runif(9),3,3) %>% as.tibble()
# fertility
Fx <- matrix(runif(9),3,3) %>% as.tibble()
# initial pop
Px <- 1:3
# arrange surv and fert as leslie mat
do_leslie <- function(Sx,Fx){
     ages <- length(Sx)
     leslie <- matrix(0,ages,ages,T)
     leslie[1,] <- Fx
     leslie[row(leslie)-1 == col(leslie)] = Sx[-1]
     leslie
} 
# projected pop 
Px %>% bind_cols(
     map2(.x = Sx, .y = Fx, ~do_leslie(.x,.y))%>% 
          accumulate(function(x,y) y %*% x) %>% 
          map_df(~.x %*% Px)
     )



