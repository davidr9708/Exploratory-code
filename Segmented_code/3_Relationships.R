# Relations among variables
library(tidytext)
library(tidyverse)
library(corrplot)
## Correlations
cor_plot <- function(data, method = 'spearman', numeric_variables){
  correlations <- round(cor(data[, numeric_variables], method = method, use = "complete.obs"), 2)
  
  corrplot(correlations, method = 'color', type = 'lower', order = "hclust",
           addCoef.col = "white", number.cex= 7/ncol(correlations),
           tl.col="black", tl.srt = 360)
}

# Numeric - factors
graph_numeric_factor <- function(data, numeric_var, factor_var, outcome_var, graph = 'point'){
  l <- length(numeric_var)
  
  point <- function(df = data, number, numeric, factors = factor_var, outcome = outcome_var){
    df %>% 
      ggplot(aes_string(y = number, x = numeric, color = outcome)) +
      geom_point(alpha = 0.3, na.rm = TRUE) +
      facet_wrap(reformulate(factors)) + 
      ggtitle(paste("Factor:", factors)) +
      theme(plot.background  = element_blank(),
            panel.background = element_blank(),
            axis.ticks.y = element_blank())}
  
  violin  <- function(df = data, number, numeric, factors = factor_var, outcome = outcome_var){
    df %>% 
      ggplot(aes_string(y = outcome, x = numeric, fill = outcome)) +
      geom_density_ridges(alpha = 0.3) +
      facet_wrap(reformulate(factors)) + 
      ggtitle(paste("Factor:", factors)) +
      theme(plot.background  = element_blank(),
            panel.background = element_blank(),
            axis.ticks.y = element_blank())
  }
  box_plot <- function(df = data, number, numeric, factors = factor_var, outcome = outcome_var){
    
    df %>% 
      ggplot(aes_string(x = outcome, y = numeric, fill = outcome)) +
      geom_boxplot(alpha = 0.3) +
      facet_wrap(reformulate(factors)) + 
      ggtitle(paste("Factor:", factors)) +
      theme(plot.background  = element_blank(),
            panel.background = element_blank(),
            axis.ticks.y = element_blank())}

  heat.map <- function(df = data, number, numeric, factors = factor_var, outcome = outcome_var){
    
    df %>% 
      ggplot(aes_string(x = factors, y = outcome, fill = numeric)) +
      geom_tile()  + 
      ggtitle(paste("Factor:", factors)) +
      theme(plot.background  = element_blank(),
            panel.background = element_blank(),
            axis.ticks.y = element_blank())}
  
  var_done <- vector()
  for(var_number in numeric_var){
    i <- 0  
    for(fact in factor_var){
    if(graph == 'point'){
      for(var_number2 in numeric_var){
        if(i <= 1){    
          i <- i + 1
          plot <-point(number = var_number, numeric = var_number2, factors = fact)
          print(plot)  
      }}}
    else if(graph == 'violin'){
      plot <- violin(numeric = var_number, factors = fact)
      print(plot) 
    }
    else if(graph == 'heatmap'){
      plot <- heat.map(numeric = var_number, factors = fact)
      print(plot) 
    }
    
    else if(graph == 'boxplot'){
      plot <- box_plot(numeric = var_number, outcome = outcome_var)
      print(plot)
    }
    else{message('Error: options for graph are violin, boxplot, heatmap or point')}
    }
  }
}
# Factor - factor
graph_factor_factor <- function(data, factor_var, outcome_var, graph = 'point'){
  clean_factors <- factor_var[-which(factor_var == outcome_var)]
  prop_sum <- round(prop.table(table(data[outcome_var][1]))*100,2)
  print(prop_sum)
  
  valid_ci <- function(x) {
    corrected <- ifelse(x > 0 & x < 100, x, ifelse(x < 0, 0, 100))
    return(corrected)}
  
  for(name in clean_factors){
    factor_levels <- levels(addNA(data[,name][[1]]))
    
    summary_data <- data  %>%
      group_by_at(c(name)) %>%
      mutate(total_fact = n()) %>%
      group_by_at(c(name, outcome_var)) %>%
      summarise(percent = round(n()/mean(total_fact)*100, 1),
                no_rows = n(),
                total_fact = mean(total_fact)) %>%
      rename('outcome' = outcome_var,
             'Variable' = name) %>%
      group_by(outcome) %>%
      mutate(ranking = rank(percent),
             error = 1.96*sqrt((percent)*(100-percent)/total_fact),
             low_conf = valid_ci(percent - error),
             up_conf = valid_ci(percent + error),
             limit = prop_sum[outcome],
             Variable = fct_reorder(Variable,ranking))
    print(summary_data)
      
    if(graph == 'bar'){
      plot <- summary_data %>% 
      ggplot(aes_string(x = 'percent', y = 'Variable', fill = 'outcome')) +
      geom_bar(stat = 'identity') + 
      geom_text(aes(label = paste('n =', total_fact), x = 105), color = 'darkgray') +
      scale_x_continuous(expand = c(0, 0), limits = c(0,120), breaks = seq(0,100, 25)) + 
      ggtitle(name) +
      theme(plot.background  = element_blank(),
            panel.background = element_blank(),
            axis.title = element_blank(),
            axis.ticks.y = element_blank())
    }
    if(graph == 'point'){ 
     plot <- summary_data %>% 
        arrange(percent) %>%
        ggplot(aes_string(x = 'percent',y = 'Variable', color = 'outcome'))  + 
        geom_errorbarh(aes(xmin=low_conf, xmax=up_conf),height =0.2)+
        geom_point(size = 3) + 
        geom_vline(aes(xintercept = limit), linetype='dashed', color = 'gray')  +
        scale_x_continuous(expand = c(0, 0), limits = c(0,120), breaks = seq(0,100, 25)) + 
        scale_y_reordered() +
        ggtitle(name) +
        facet_wrap(.~outcome, scales = 'free') +
        theme(plot.background  = element_blank(),
              panel.background = element_blank(),
              axis.title = element_blank(),
              axis.ticks.y = element_blank()) 
    }
    print(plot)
  
  }}
