# Distributions: This code chunk is to build functions 
# to visualize the distributions of each variable

## Variables distribution
### This function will identify the type of each variable
### and put it into a matrix
variable.types <- function(data){
  
  numeric_variable   <- vector()
  factor_variable    <- vector()
  character_variable <- vector()
  
  for(name in colnames(data)){
    if(class(data[, name])[1] %in% c('ordered', 'factor')){
      
      factor_variable <- c(factor_variable, name)
    }
    else if(class(data[, name])[1] %in% c('numeric', 'integer')){
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
  
  ifelse(length(numeric_variable) == 0,
         message('Error: numeric_variable is empty'), 
         skewed_values <- round(apply(data[numeric_variable], 2, skewness), 2))
  
  if(graph == 'both'){
    
    for(name in numeric_variable){
      plot <- density(data[, name])
      par(mfrow=c(2,1))
      hist(data[, name], xlab = name,
           main = paste('Variable:', name,'\nSkweness:', skewed_values[name]))
      
      plot(plot, main = NA)
      polygon(plot, col = 'darkblue')
    }
  }
  else if(graph == 'hist'){
    
    for(name in numeric_variable){
      hist(data[, name], xlab = name,
           main =paste('Variable:', name,'\nSkweness:', skewed_values[name]))
    }
  }
  else if(graph == 'density'){
    
    for(name in numeric_variable){
      plot <- density(data[, name])
      plot(plot,
           main = paste('Variable:', name,'\nSkweness:', skewed_values[name]))
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
    factor_levels <- levels(data[,name])
    factor_length <- length(factor_levels) 
    
    data <- data %>%
      mutate(total = n()) %>%
      group_by_at(name) %>%
      summarise(percent = round(n()/mean(total)*100, 1),
                no_rows = n()) %>% 
      ungroup() %>%
      mutate(maximum = max(no_rows),
             ratio = round(maximum/no_rows, 2),
             labels = paste0('n =', no_rows, '; ratio =', ' ', ratio),
             factors = fct_reorder(factor_levels, percent, .desc = FALSE))
    
    plot <- data %>% 
      ggplot(aes_string(x = 'percent', y = 'factors', label = 'labels')) +
      geom_bar(stat = 'identity', fill =ifelse(data$percent < 10, 'darkred', 'darkgray'))  + 
      geom_text(hjust = ifelse(data$percent <= 10, -0.2, 1.5 ),
                color = ifelse(data$ratio >= 20, 'darkred', 
                               ifelse(data$percent < 10, 'darkgray','white'))) +
      geom_vline(xintercept = 10) +
      scale_x_continuous(expand = c(0,0)) +
      ggtitle(paste('Variable:', name, ';', 'Levels: ', factor_length)) + 
      xlab('Percentage (%)') +
      theme(panel.background = element_blank(),
            axis.title.y = element_blank())
    
    print(plot)
  }
}