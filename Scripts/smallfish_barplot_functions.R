## -----------------------------------------------------------------------------
# Function to generate fish bar plot for restoration runs
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Function for percent diff Bar plot -> 1 alt and baselines
## -----------------------------------------------------------------------------
PerDiffPlot <- function(df, # data frame to plot
                        x_var, # string of x variable
                        y_var, # string of y variable
                        fill_var, # string of fill variable
                        title, # string of plot title
                        y_lab, # string of y label
                        x_lab, # string of x label
                        min_limit, # value of minimum value for y scale
                        max_limit) { # value of maximum value for y value
  diff_plot <- ggplot(data = df,
                      aes(x = !!sym(x_var), y = !!sym(y_var),
                          fill = !!sym(fill_var))) + 
    geom_bar(stat = "identity", position = "dodge",
             width = 0.7, colour = "black") + 
    labs(y = y_lab, x = x_lab, title = title,
         fill = "percent change \nfrom baseline") +
    scale_y_continuous(limits = c(min_limit, max_limit)) +
    theme(axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          plot.title = element_text(size = 20),
          axis.text.x = element_text(size = 12, angle = 70, hjust = 1), 
          axis.text.y = element_text(size = 12), 
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 15), 
          legend.title.align = 0.5,
          # Set margins for plot - wider on right side to make space fore legend
          plot.margin = margin(1, .5, 1, .5, unit = "in"))
  
  return(diff_plot)
}

## -----------------------------------------------------------------------------
## Function for percent diff Bar plot -> alts with 1 baseline
## -----------------------------------------------------------------------------

PerDiffPlotAlts <- function(df, # data frame to plot
                            x_var, # string of x variable
                            y_var, # string of y variable
                            fill_var, # string of fill variable
                            title, # string of plot title
                            y_lab, # string of y label
                            x_lab, # string of x label
                            min_limit, # value of minimum value for y scale
                            max_limit) { # value of maximum value for y scale
  bar_pal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
  diff_plot <- ggplot(data = df,
                             aes(x = !!sym(x_var), y = !!sym(y_var),
                                 fill = !!sym(fill_var))) + 
    geom_bar(stat = "identity", position = "dodge",
             width = 0.7, colour = "black") + 
    scale_fill_manual(values = bar_pal) +
    labs(y = y_lab, x = x_lab, title = title,
         fill = "percent change \nfrom baseline") +
    scale_y_continuous(limits = c(min_limit, max_limit)) +
    theme(axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          plot.title = element_text(size = 20),
          axis.text.x = element_text(size = 12, angle = 70, hjust = 1), 
          axis.text.y = element_text(size = 12), 
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 15), 
          legend.title.align = 0.5,
          # Set margins for plot - wider on right side to make space fore legend
          plot.margin = margin(1, .5, 1, .5, unit = "in")) 
  
  return(diff_plot)
}
