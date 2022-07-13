library(pacman)
p_load(MASS, tidyverse, caret, corrplot, mlbench, e1071, ggridges)

# Data 
# Exploration
## Variable type identification
variables.types <- function(data){
  numeric_variable <- vector()
  factor_variable <- vector()
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

## Numeric variables
### Skewness

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

## Nominal variables
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

## Missing values
na.by.columns <- function(data, visual = 'graph'){
  all_na <- data %>%
  mutate_all(as.character) %>%
  gather(key = 'column', value = 'value') %>% 
  group_by(column) %>%
  summarise(na_percent = round(sum(is.na(value))/n()*100,2),
            na_count   = sum(is.na(value))) %>%
  mutate(column = fct_reorder(column, na_percent))

  plot <- all_na %>% 
    ggplot(aes(x = na_percent, y = column)) +
    geom_bar(stat = 'identity') + 
    scale_x_continuous(expand = c(0, 0)) + 
    ggtitle('NA% in each column')
  
  if(visual == 'graph'){return(plot)}
  else if(visual == 'matrix'){return(all_na)}
  else if(visual == 'both'){return(list('graph' = plot, 'matrix' = all_na))}
  else{message('Error: options for visual are graph, matrix or both')}
}

target.proportion.na <- function(data, target, visual = 'graph'){

  predictor_na <- data %>%
    mutate_all(as.character) %>%
    gather(2:36, key = 'column', value = 'value') %>% 
    filter(is.na(value)) %>%
    group_by(column) %>%
    mutate(total_count = n()) %>%
    group_by_at('column', target) %>%
    summarise(group_count = n(),
              na_percent  = group_count/mean(total_count)*100) %>%
    mutate(column = fct_reorder(column, na_percent))

  plot <- predictor_na %>% 
    ggplot(aes_string(x = 'na_percent', y = 'column', label = 'group_count', fill = target)) +
    geom_bar(stat = 'identity')

  if(visual == 'graph'){return(plot)}
  else if(visual == 'matrix'){return(predictor_na)}
  else if(visual == 'both'){return(list('graph' = plot, 'matrix' = predictor_na))}
  else{message('Error: options for visual are graph, matrix or both')}
}

na.by.target <- function(data, target, visual = 'graph'){
  predictor_na_label <- data %>%
  mutate_all(as.character) %>%
  gather(2:36, key = 'column', value = 'value') %>% 
  group_by_at('column', target) %>%
  mutate(total_count = n(),
         Is_NA = ifelse(is.na(value), 'Yes', 'No')) %>%
  group_by_at('column', target, 'Is_NA') %>%
  summarise(group_count = n(),
            na_percent  = group_count/mean(total_count)*100) %>%
  mutate(column = fct_reorder(column, na_percent))


 plot <- predictor_na_label %>% 
  ggplot(aes(x = na_percent, y = column, label = group_count, fill = Is_NA)) +
  geom_bar(stat = 'identity') + facet_grid(reformulate(target))  + 
   scale_fill_manual(values = c('gray','darkred')) + 
   scale_x_continuous(breaks = c(50,100))
 
 if(visual == 'graph'){return(plot)}
 else if(visual == 'matrix'){return(predictor_na)}
 else if(visual == 'both'){return(list('graph' = plot, 'matrix' = predictor_na))}
 else{message('Error: options for visual are graph, matrix or both')}
}

# Relations among variables

## Correlations
cor_plot <- function(data, methods = 'spearman', numeric_variables){
  correlations <- round(cor(data[, numeric_variables], method = methods, use = "complete.obs"), 2)

  corrplot(correlations, method = 'color', type = 'lower', order = "hclust",
           addCoef.col = "white", number.cex= 7/ncol(correlations),
           tl.col="black", tl.srt = 360)
}

# Numeric - factors
graph_numeric_factor <- function(data, numeric_var, factor_var, graph = 'point'){
  l <- length(numeric_var)
  point <- function(df = data, number, numeric, factors){
    df %>% 
    ggplot(aes_string(y = number, x = numeric, color = factors)) +
    geom_point(alpha = 0.3)}
  violin  <- function(df = data, number, numeric, factors){
    df %>% 
       ggplot(aes_string(y = factors, x = numeric, fill = factors)) +
      geom_density_ridges(alpha = 0.3)
  }
  box_plot <- function(df = data, number, numeric, factors){
    df %>% 
      ggplot(aes_string(x = factors, y = numeric, fill = factors)) +
      geom_boxplot(alpha = 0.3)}
  
  var_done <- vector()
  for(var_number in numeric_var){
    i <- 0  
    for(var_factor in factor_var){
    if(graph == 'point'){
      for(var_number2 in numeric_var){
      if(i <= 1){    
      i <- i + 1
      plot <-point(number = var_number, numeric = var_number2, factors = var_factor)
      print(plot)  
      }}}
    else if(graph == 'violin'){
        plot <- violin(numeric = var_number, factors = var_factor)
        print(plot) 
    }
    else if(graph == 'boxplot'){
      plot <- box_plot(numeric = var_number, factors = var_factor)
      print(plot)
      }
    else{message('Error: options for graph are violin, boxplot or point')}
    }
  }}

graph_numeric_outcome <- function(data, numeric_var, outcome){
  l <- length(numeric_var)
  point <- function(df = data, outcome, numeric){
    df %>% 
      ggplot(aes_string(y = outcome, x = numeric)) +
      geom_point(alpha = 0.3)}
  i <- 0  
  for(var_number in numeric_var){
  if(i <= l){    
    i <- i + 1
    plot <- point(outcome = outcome, numeric = var_number)
    print(plot)  
  }}}
  