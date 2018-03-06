# Assignment 2
# Question 1


# generate some random data with some given parameter
gen.mod <- function(n = 100, b0 =2, b1 = 3, sd.err =1){
  predictor <- runif(n) - 0.5
  response <- b0 + b1 * predictor + rnorm(n, mean = 0, sd = sd.err)
  return(data.frame(response = response, predictor = predictor))
}

MakeParameterMatrix <- function(n_list, b0_list, b1_list, sd.err_list){
  # get coefficient for each predictor
  df <- matrix(ncol = 5, nrow =  (length(n_list)*
                                       length(b0_list)*
                                       length(b1_list)*
                                       length(sd.err_list)), NA)
  df <- as.data.frame(df)
  names(df) <- c("n","b0","b1","sd.err","t.value")
  
  row = 1
  for(i in 1:length(n_list)){
    for(j in 1:length(b1_list)){
      for(k in 1:length(b0_list)){
        for(l in 1:length(sd.err_list)){
          m1 <- lm(response ~ predictor , 
                   data = gen.mod(n_list[[i]], b0_list[[j]], b1_list[[k]], sd.err_list[[l]]))
          
         df$n[[row]] <- n_list[[i]]
         df$b0[[row]] <- b0_list[[j]]
         df$b1[[row]] <- b1_list[[k]]
         df$sd.err[[row]] <- sd.err_list[[l]]
         df$t.value[[row]] <- summary(m1)[[4]][[6]]
         row = row + 1
        }
      }
    }
  }
  return(df)
}



# Question 2
# Produce data frame with function

n_list <- seq(100,1000,300)
b0_list<- seq(1,5)
b1_list <- seq(1,5)
sd.err_list <- seq(1,3)

estimates <- MakeParameterMatrix(n_list,b0_list,b1_list,sd.err_list)

estimates

# Question 3
# Show the results visually
estimates$n <- as.character(estimates$n)
g <- ggplot(data = estimates,mapping = aes(x = b1, y = b0, colour = t.value, size = sd.err)) +
  geom_jitter() + 
  facet_wrap(~n)

g + scale_color_gradientn(colours = c("red","purple","green"))
