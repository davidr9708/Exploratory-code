# Missing values: This code chunk will let you visualize the missing values

## Missing values in each column
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


## Proportion of Missing values belonging to each class of the target
target.proportion.na <- function(data, target, visual = 'graph'){
  
  predictor_na <- data %>%
    mutate_all(as.character) %>%
    gather(1:(ncol(data)-1), key = 'column', value = 'value') %>% 
    filter(is.na(value)) %>%
    group_by(column) %>%
    mutate(total_count = n()) %>%
    group_by_at(.vars = c('column', target[1])) %>%
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

## Missing values by target: Shows the proportion of na/nonan in each
## class of the target
na.by.target <- function(data, target, visual = 'graph'){
  predictor_na_label <- data %>%
    mutate_all(as.character) %>%
    gather(1:(ncol(data)-1), key = 'column', value = 'value') %>% 
    group_by_at(.vars = c(target[1],'column')) %>%
    mutate(total_count = n(),
           Is_NA = ifelse(is.na(value), 'Yes', 'No')) %>%
    group_by_at(.vars = c('column', target[1], 'Is_NA')) %>%
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

