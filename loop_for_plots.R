

library(ggplot2)
library(gridExtra)
library(patchwork)


# for creating a  plot function then looping over 

create_plots <- function(data, y_var, x_vars, family = "gaussian") {
        plots <- list()
        
        for (x_var in x_vars) {
                # Create custom x-label
                x_label <- if (x_var == "Emergence.time") {
                        "sqrt Emergence time (sec)"
                } else if (x_var == "Time.spent.in.outer.area..walls") {
                        "sqrt Time spent in outer area (sec)"
                } else if  (x_var == "Time.spent.in.front.of.the.gate"){
                        "sqrt Time spent in front of the gate(sec)"
                } else {
                        gsub("\\.", " ", x_var)
                }
                
                # Create custom y-label
                y_label <- if (y_var == "Emergence.time") {
                        "sqrt Emergence time (sec)"
                } else if (y_var == "Time.spent.in.outer.area..walls") {
                        "sqrt Time spent in outer area (sec)"
                } else if   (y_var == "Time.spent.in.front.of.the.gate"){
                        "sqrt Time spent in front of the gate(sec)"
                } else {
                        gsub("\\.", " ", y_var)
                }
                
                p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
                        geom_point() +
                        geom_smooth(method = "glm", method.args = list(family = family), se = TRUE) +
                        labs(
                                x = x_label,
                                y = y_label
                        ) +
                        theme_classic() +
                        theme(
                                axis.line = element_line(color = "black"),
                                axis.ticks = element_line(color = "black"),
                                panel.grid = element_blank(),
                                text = element_text(size = 10)
                        )
                
                plots[[x_var]] <- p
        }
        
        return(plots)
}



# List of y-variables to iterate over
y_variables <- c("Emergence.time", "Time.spent.in.outer.area..walls", 
                 "Time.spent.in.front.of.the.gate","Surfacing", "Vertical.movement", 
                 "Chamber.switching", "Crossing.the.center")

# List of x-variables to use for plotting
x_variables <- c("Emergence.time", "Time.spent.in.outer.area..walls", 
                 "Time.spent.in.front.of.the.gate", "Surfacing", "Chamber.switching", 
                 "Vertical.movement", "Crossing.the.center")

# List of y-variables to iterate over
y_variables <- c("Emergence.time", "Time.spent.in.outer.area..walls", 
                 "Time.spent.in.front.of.the.gate", "Surfacing", "Vertical.movement", 
                 "Chamber.switching", "Crossing.the.center")

# List of x-variables to use for plotting
x_variables <- c("Emergence.time", "Time.spent.in.outer.area..walls", 
                 "Time.spent.in.front.of.the.gate", "Surfacing", "Chamber.switching", 
                 "Vertical.movement", "Crossing.the.center")

# Counter for naming the files
plot_count <- 1

# Loop over each y-variable, create plots, and save them
for (y_var in y_variables) {
        # Determine the appropriate GLM family based on y_var
        family <- if (y_var %in% c("Emergence.time", "Time.spent.in.outer.area..walls", 
                                   "Time.spent.in.front.of.the.gate")) {
                "gaussian"
        } else {
                "poisson"
        }
        
        # Filter out the y_var from x_variables to avoid using the same variable as both x and y
        filtered_x_vars <- setdiff(x_variables, y_var)
        
        # Create plots using the create_plots function
        plots <- create_plots(data, y_var, filtered_x_vars, family = family)
        
        # Combine the plots into one figure with wrap_plots
        combined_plot <- wrap_plots(plots, ncol = 3)
        
        # Generate a file name based on the plot count and y_var name
        file_name <- paste0("combined_plot_", plot_count, "_", y_var, ".png")
        
        # Save the combined plot to a file
        ggsave(file_name, plot = combined_plot, width = 12, height = 8)
        
        # Increment the plot count
        plot_count <- plot_count + 1
}


