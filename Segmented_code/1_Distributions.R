# Distributions: This code chunk is to build functions 
#                to visualize the distributions of each variable
library(pacman)
p_load(MASS, tidyverse, caret, corrplot, mlbench, e1071, ggridges, rlang,
       tidytext)

## Variables distribution
### This function will identify the type of each variable
### and put it into a matrix
variable.types <- function(data){
  
  numeric_variable   <- vector()
  factor_variable    <- vector()
  character_variable <- vector()

  for(name in colnames(data)){
    
    if(class(data[name][[1]])[1] %in% c('ordered', 'factor')){
      factor_variable <- c(factor_variable, name)
    }
    else if(class(data[name][[1]]) %in% c('numeric', 'integer')){
      numeric_variable <- c(numeric_variable, name)
    }
    else{
      character_variable <- c(character_variable, name)}
  }
  
  return(list('Characters' = character_variable,
              'Factors'    = factor_variable, 
              'Numeric'    = numeric_variable))
}


## Numeric 
### This function will plot the distribution
### of the numeric variables given
plot.numeric <- function(data, numeric_variable, graph = 'hist'){
  
  if(length(numeric_variable) == 0){
     message('Error: numeric_variable is empty')} 
  

  if(graph == 'both'){
    
    for(name in numeric_variable){
      skewed_values <- round(skewness(data[,name][[1]], na.rm = T), 2)
      plot <- density(data[, name][[1]], na.rm = T)
      par(mfrow=c(2,1))
      hist(data[, name][[1]], xlab = name,
           main = paste('Variable:', name,'\nSkweness:', skewed_values))
      
      plot(plot, main = NA)
      polygon(plot, col = 'darkblue')
    }
  }
  else if(graph == 'hist'){
    
    for(name in numeric_variable){
      skewed_values <- round(skewness(data[,name][[1]], na.rm = T), 2)
      hist(data[, name][[1]], xlab = name,
           main =paste('Variable:', name,'\nSkweness:', skewed_values))
    }
  }
  else if(graph == 'density'){
    
    for(name in numeric_variable){
      skewed_values <- round(skewness(data[,name][[1]], na.rm = T), 2)
      plot <- density(data[, name][[1]], na.rm = T)
      plot(plot,
           main = paste('Variable:', name,'\nSkweness:', skewed_values))
      polygon(plot, col = 'darkblue')
    }
  }
  else{message('Error: options for graph are hist, density, and both')}
}

## Factors 
### This function will plot the distribution
### of the nominal variables given
plot.factors <- function(data, factor_variable){

  for(name in factor_variable){
    
    factor_levels <- levels(addNA(data[,name][[1]]))
    factor_length <- length(factor_levels) 

    data_summary <- data %>%
      mutate(total = n()) %>%
      group_by_at(name,) %>%
      summarise(percent = round(n()/mean(total)*100, 1),
                no_rows = n()) %>% 
      ungroup()  %>%
      mutate(maximum = max(no_rows),
             ratio = round(maximum/no_rows, 2),
             labels = paste0('n =', no_rows, '; ratio =', ' ', ratio))
    
    plot <- data_summary %>% 
      ggplot(aes_string(x = 'percent', y = name, label = 'labels')) +
      geom_bar(stat = 'identity', fill =ifelse(data_summary$percent < 10, 'darkred', 'darkgray'))  + 
      geom_text(hjust = ifelse(data_summary$percent <= 10, -0.2, 1.5 ),
                color = ifelse(data_summary$ratio >= 20, 'darkred', 
                               ifelse(data_summary$percent < 10, 'darkgray','white'))) +
      geom_vline(xintercept = 10, linetype = 'dashed') +
      scale_x_continuous(expand = c(0,0)) +
      ggtitle(paste('Variable:', name, ';', 'Levels: ', factor_length)) + 
      xlab('Percentage (%)') +
      theme(panel.background = element_blank(),
            axis.title.y = element_blank())

    print(plot)
  }
}
