
library(ggplot2)
library(gridExtra)
library(patchwork)




rm(list = ls())
getwd()

setwd("C:/Users/depha/OneDrive/Mahseer_behaviour_paper")
dir()
#function tranfrom data @copilot
data <-  read.csv("population1_vinitha_2months.csv")
head(data)
names(data)
str(data)
summary(data)







# Emergence.time ~ Time.spent.in.outer.area..walls + Time.spent.in.front.of.the.gate + Chamber.switching 

plot1 <- ggplot(data, aes(x = Time.spent.in.outer.area..walls, y = Emergence.time)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(,
             x = " Time spent in outer area walls",
             y = " emergence time")+ theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),  # Removes background grid lines
                text = element_text(size = 12)  # Adjusts text size
        )


plot2 <- ggplot(data, aes(x = Time.spent.in.front.of.the.gate, y = Emergence.time)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(,
             x = " Time spent in front of the gate",
             y = " emergence time") + theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),  # Removes background grid lines
                text = element_text(size = 12)  # Adjusts text size
        )

                
                
plot3 <- ggplot(data, aes(x = Chamber.switching, y = Emergence.time)) +
                        geom_point() +
                        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
                        labs(,
                             x = "Chamber.switching",
                             y = " Emergence.time") + theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),  # Removes background grid lines
                text = element_text(size = 12)  # Adjusts text size
        )

                        )
plot1+plot2+plot3




plot1 <- ggplot(data, aes(x = Time.spent.in.outer.area..walls, y = Emergence.time)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(
                x = "Time spent in outer area walls",
                y = "Emergence time"
        ) + theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),
                text = element_text(size = 12)
        )

plot2 <- ggplot(data, aes(x = Time.spent.in.front.of.the.gate, y = Emergence.time)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(
                x = "Time spent in front of the gate",
                y = "Emergence time"
        ) + theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),
                text = element_text(size = 12)
        )

plot3 <- ggplot(data, aes(x = Chamber.switching, y = Emergence.time)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(
                x = "Chamber.switching",
                y = "Emergence.time"
        ) + theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),
                text = element_text(size = 12)
        )

combined_plot <- plot1 + plot2 + plot3 + plot_layout(ncol = 1)
print(combined_plot)





columns_to_transform <- c("Emergence.time", "Time.spent.in.outer.area..walls", "Time.spent.in.chamber.A", "Time.spent.in.front.of.the.gate")

data <- sqrt_transform2(data, columns_to_transform)

setwd("C:/Users/depha/OneDrive/Mahseer_behaviour_paper")
dir()
#function tranfrom data @copilot
data <-  read.csv("population1_vinitha_2months.csv")
head(data)
names(data)
str(data)
summary(data)


plot1 <- ggplot(data, aes(x = Emergence.time, y = Surfacing)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "poisson"), se = TRUE) +
        labs(
                x = "Time spent in outer area walls",
                y = "Emergence time"
        ) + theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),
                text = element_text(size = 12)
)
# write me a function that with take a variable in the x axis and all the other variables one by one in the y axis making 6 plots @copilot

plots_fun <- function(data, x_var) {
        plots <- list()
        for (y_var in names(data)) {
                if (y_var != x_var) {
                        plot <- ggplot(data, aes_string(x = x_var, y = y_var)) +
                                geom_point() +
                                geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
                                labs(
                                        x = x_var,
                                        y = y_var
                                ) + theme_classic() +
                                theme(
                                        axis.line = element_line(color = "black"),
                                        axis.ticks = element_line(color = "black"),
                                        panel.grid = element_blank(),
                                        text = element_text(size = 12)
                                )
                        plots[[y_var]] <- plot
                }
        }
        return(plots)
}

#plot the function should save the plots in a list and return the list @copilot
plots_fun <- function(data, x_var) {
        plots <- list()
        for (y_var in names(data)) {
                if (y_var != x_var) {
                        plot <- ggplot(data, aes_string(x = x_var, y = y_var)) +
                                geom_point() +
                                geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
                                labs(
                                        x = x_var,
                                        y = y_var
                                ) + theme_classic() +
                                theme(
                                        axis.line = element_line(color = "black"),
                                        axis.ticks = element_line(color = "black"),
                                        panel.grid = element_blank(),
                                        text = element_text(size = 12)
                                )
                        plots[[y_var]] <- plot
                }
        }
        return(plots)
}



plots <- plots_fun(data, "Emergence.time")
print(plots)




plots_fun <- function(data, x_var) {
        plots <- list()
        for (y_var in names(data)) {
                if (y_var != x_var) {
                        if (y_var %in% c("Emergence.time", "Time.spent.in.outer.area..walls", 
                                         "Time.spent.in.chamber.A", "Time.spent.in.front.of.the.gate")) {
                                # Gaussian distribution
                                plot <- ggplot(data, aes_string(x = x_var, y = y_var)) +
                                        geom_point() +
                                        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
                                        labs(
                                                x = x_var,
                                                y = y_var
                                        ) + theme_classic() +
                                        theme(
                                                axis.line = element_line(color = "black"),
                                                axis.ticks = element_line(color = "black"),
                                                panel.grid = element_blank(),
                                                text = element_text(size = 12)
                                        )
                        } else {
                                # Poisson distribution
                                plot <- ggplot(data, aes_string(x = x_var, y = y_var)) +
                                        geom_point() +
                                        geom_smooth(method = "glm", method.args = list(family = "poisson"), se = TRUE) +
                                        labs(
                                                x = x_var,
                                                y = y_var
                                        ) + theme_classic() +
                                        theme(
                                                axis.line = element_line(color = "black"),
                                                axis.ticks = element_line(color = "black"),
                                                panel.grid = element_blank(),
                                                text = element_text(size = 12)
                                        )
                        }
                        plots[[y_var]] <- plot
                }
        }
        return(plots)
}









# fist plot




plot1 <- ggplot(data, aes(x = Emergence.time , y = Time.spent.in.outer.area..walls)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(,
             x = " sqrt Emergence time(sec)",
             y = " sqrt Time spent in outer area(sec) ")+ theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),  # Removes background grid lines
                text = element_text(size = 10)  # Adjusts text size
        )
plot2 <- ggplot(data, aes(x = Time.spent.in.front.of.the.gate, y = Time.spent.in.outer.area..walls )) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(,
             x = " sqrt Time spent in front of the gate(sec)",
             y = " sqrt Time spent in outer area(sec) ") + theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),  # Removes background grid lines
                text = element_text(size = 10)  # Adjusts text size
        )
plot3 <- ggplot(data, aes(x = Chamber.switching, y = Time.spent.in.outer.area..walls)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(,
             x = "Chamber switching",
             y = " sqrt Time spent in outer area(sec) ") + theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),  # Removes background grid lines
                text = element_text(size = 10)  # Adjusts text size
        )
plot4 <- ggplot(data, aes(x = Surfacing, y = Time.spent.in.outer.area..walls)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(,
             x = "Surfacing",
             y = " sqrt Time spent in outer area(sec) ") + theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),  # Removes background grid lines
                text = element_text(size = 10)  # Adjusts text size
        )
plot5 <- ggplot(data, aes(x = Crossing.the.center, y = Time.spent.in.outer.area..walls)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(,
             x = "Crossing the center",
             y = " sqrt Time spent in outer area(sec) ") + theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),  # Removes background grid lines
                text = element_text(size = 10)  # Adjusts text size
        )
plot6 <- ggplot(data, aes(x = Vertical.movement, y = Time.spent.in.outer.area..walls)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(,
             x = "Vertical movement",
             y = " sqrt Time spent in outer area(sec) ") + theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),  # Removes background grid lines
                text = element_text(size = 10)  # Adjusts text size
        )
#combined plot
combined_plot <- plot1 + plot2 + plot3 + plot4 + plot5 + plot6 + plot_layout(ncol = 3)
print(combined_plot)
























plot1 <- ggplot(data, aes(x = Time.spent.in.outer.area..walls , y = Emergence.time)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(,
             x = "sqrt Time spent in outer area(sec)",
             y = " sqrt Emergence time(sec) ")+ theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),  # Removes background grid lines
                text = element_text(size = 10)  # Adjusts text size
        )
plot2 <- ggplot(data, aes(x = Time.spent.in.front.of.the.gate, y = Emergence.time )) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(,
             x = "sqrt Time spent in front of the gate(sec) ",
             y = "sqrt Emergence.time(sec)") + theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),  # Removes background grid lines
                text = element_text(size = 10)  # Adjusts text size
        )
plot3 <- ggplot(data, aes(x = Chamber.switching, y = Emergence.time)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(,
             x = "Chamber switching",
             y = "sqrt  Emergence.time(sec)") + theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),  # Removes background grid lines
                text = element_text(size = 10)  # Adjusts text size
        )
plot4 <- ggplot(data, aes(x = Surfacing, y = Emergence.time)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(,
             x = "Surfacing",
             y = "sqrt Emergence.time(sec) ") + theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),  # Removes background grid lines
                text = element_text(size = 10)  # Adjusts text size
        )
plot5 <- ggplot(data, aes(x = Crossing.the.center, y = Emergence.time)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(,
             x = "Crossing the center",
             y = "sqrt Emergence.time(sec) ") + theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),  # Removes background grid lines
                text = element_text(size = 10)  # Adjusts text size
        )
plot6 <- ggplot(data, aes(x = Vertical.movement, y = Emergence.time)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(,
             x = "Vertical movement",
             y = "sqrt Emergence.time(sec) ") + theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),  # Removes background grid lines
                text = element_text(size = 10)  # Adjusts text size
        )
#combined plot
combined_plot <- plot1 + plot2 + plot3 + plot4 + plot5 + plot6 + plot_layout(ncol = 3)
print(combined_plot)
   


#plot2

plot1 <- ggplot(data, aes(x = Time.spent.in.outer.area..walls, y = Emergence.time )) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(,
             x = " sqrt Time spent in outer area(sec)",
             y = " sqrt Emergence time(sec) ")+ theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),  # Removes background grid lines
                text = element_text(size = 10)  # Adjusts text size
        )
plot2 <- ggplot(data, aes(x = Time.spent.in.front.of.the.gate, y = Emergence.time )) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(,
             x = " sqrt Time spent in front of the gate(sec)",
             y = " sqrt  Emergence.time(sec) ") + theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),  # Removes background grid lines
                text = element_text(size = 10)  # Adjusts text size
        )
plot3 <- ggplot(data, aes(x = Chamber.switching, y = Emergence.time)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(,
             x = "Chamber switching",
             y = " sqrt  Emergence.time(sec) ") + theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),  # Removes background grid lines
                text = element_text(size = 10)  # Adjusts text size
        )
plot4 <- ggplot(data, aes(x = Surfacing, y = Emergence.time)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(,
             x = "Surfacing",
             y = " sqrt  Emergence.time(sec) ") + theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),  # Removes background grid lines
                text = element_text(size = 10)  # Adjusts text size
        )
plot5 <- ggplot(data, aes(x = Crossing.the.center, y = Emergence.time)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(,
             x = "Crossing the center",
             y = " sqrt  Emergence.time(sec) ") + theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),  # Removes background grid lines
                text = element_text(size = 10)  # Adjusts text size
        )
plot6 <- ggplot(data, aes(x = Vertical.movement, y = Emergence.time)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(,
             x = "Vertical movement",
             y = " sqrt  Emergence.time(sec) ") + theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),  # Removes background grid lines
                text = element_text(size = 10)  # Adjusts text size
        )
#combined plot
combined_plot <- plot1 + plot2 + plot3 + plot4 + plot5 + plot6 + plot_layout(ncol = 3)
print(combined_plot)

#plot3



plot1 <- ggplot(data, aes(x = Emergence.time, y = Time.spent.in.front.of.the.gate)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(
                x = "sqrt Emergence time (sec)",
                y = "sqrt Time spent in front of the gate (sec)") +
        theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),
                text = element_text(size = 10)
        )

plot2 <- ggplot(data, aes(x = Time.spent.in.outer.area..walls, y = Time.spent.in.front.of.the.gate)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(
                x = "sqrt Time spent in outer area(sec)",
                y = "sqrt Time spent in front of the gate (sec)") +
        theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),
                text = element_text(size = 10)
        )

plot3 <- ggplot(data, aes(x = Chamber.switching, y = Time.spent.in.front.of.the.gate)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(
                x = "Chamber switching",
                y = "sqrt Time spent in front of the gate (sec)") +
        theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),
                text = element_text(size = 10)
        )

plot4 <- ggplot(data, aes(x = Surfacing, y = Time.spent.in.front.of.the.gate)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(
                x = "Surfacing",
                y = "sqrt Time spent in front of the gate (sec)") +
        theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),
                text = element_text(size = 10)
        )

plot5 <- ggplot(data, aes(x = Crossing.the.center, y = Time.spent.in.front.of.the.gate)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(
                x = "Crossing the center",
                y = "sqrt Time spent in front of the gate (sec)") +
        theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),
                text = element_text(size = 10)
        )

plot6 <- ggplot(data, aes(x = Vertical.movement, y = Time.spent.in.front.of.the.gate)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(
                x = "Vertical movement",
                y = "sqrt Time spent in front of the gate (sec)") +
        theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),
                text = element_text(size = 10)
        )

# Combine the plots
combined_plot <- plot1 + plot2 + plot3 + plot4 + plot5 + plot6 + plot_layout(ncol = 3)
print(combined_plot)



plot1 <- ggplot(data, aes(x = Emergence.time, y = Time.spent.in.front.of.the.gate)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(
                x = "sqrt Emergence time (sec)", 
                y = "sqrt Time spent in front of the gate (sec)") +
        theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),
                text = element_text(size = 10)
        )

plot2 <- ggplot(data, aes(x = Time.spent.in.outer.area..walls, y = Time.spent.in.front.of.the.gate)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(
                x = "sqrt Time spent in outer area(sec)",
                y = "sqrt Time spent in front of the gate (sec)") +
        theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),
                text = element_text(size = 10)
        )

plot3 <- ggplot(data, aes(x = Chamber.switching, y = Time.spent.in.front.of.the.gate)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(
                x = "Chamber switching",
                y = "sqrt Time spent in front of the gate (sec)") +
        theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),
                text = element_text(size = 10)
        )

plot4 <- ggplot(data, aes(x = Surfacing, y = Time.spent.in.front.of.the.gate)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(
                x = "Surfacing",
                y = "sqrt Time spent in front of the gate (sec)") +
        theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),
                text = element_text(size = 10)
        )

plot5 <- ggplot(data, aes(x = Crossing.the.center, y = Time.spent.in.front.of.the.gate)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(
                x = "Crossing the center",
                y = "sqrt Time spent in front of the gate (sec)") +
        theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),
                text = element_text(size = 10)
        )

plot6 <- ggplot(data, aes(x = Vertical.movement, y = Time.spent.in.front.of.the.gate)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = TRUE) +
        labs(
                x = "Vertical movement",
                y = "sqrt Time spent in front of the gate (sec)") +
        theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),
                text = element_text(size = 10)
        )

# Combine the plots
combined_plot <- plot1 + plot2 + plot3 + plot4 + plot5 + plot6 + plot_layout(ncol = 3)
print(combined_plot)





plot1 <- ggplot(data, aes(x = Emergence.time, y = Chamber.switching)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "poisson"), se = TRUE) +
        labs(
                x = "sqrt Emergence time (sec)", 
                y = "Chamber switching") +
        theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),
                text = element_text(size = 10)
        )

plot2 <- ggplot(data, aes(x = Time.spent.in.outer.area..walls, y = Chamber.switching)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "poisson"), se = TRUE) +
        labs(
                x = "sqrt Time spent in outer area(sec)",
                y = "Chamber switching") +
        theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),
                text = element_text(size = 10)
        )

plot3 <- ggplot(data, aes(x =Time.spent.in.front.of.the.gate, y = Chamber.switching )) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "poisson"), se = TRUE) +
        labs(
                x = "sqrt Time spent in front of the gate (sec)",
                y = "Chamber switching") +
        theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),
                text = element_text(size = 10)
        )

plot4 <- ggplot(data, aes(x = Surfacing, y = Chamber.switching)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "poisson"), se = TRUE) +
        labs(
                x = "Surfacing",
                y = "Chamber.switching") +
        theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),
                text = element_text(size = 10)
        )

plot5 <- ggplot(data, aes(x = Crossing.the.center, y = Chamber.switching)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "poisson"), se = TRUE) +
        labs(
                x = "Crossing the center",
                y = "Chamber.switching") +
        theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),
                text = element_text(size = 10)
        )

plot6 <- ggplot(data, aes(x = Vertical.movement, y = Chamber.switching)) +
        geom_point() +
        geom_smooth(method = "glm", method.args = list(family = "poisson"), se = TRUE) +
        labs(
                x = "Vertical movement",
                y = "Chamber.switching") +
        theme_classic() +
        theme(
                axis.line = element_line(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),
                text = element_text(size = 10)
        )

# Combine the plots
combined_plot <- plot1 + plot2 + plot3 + plot4 + plot5 + plot6 + plot_layout(ncol = 3)
print(combined_plot)








# Usage
x_variables <- c("Emergence.time", "Time.spent.in.outer.area..walls", 
                 "Chamber.switching", "Surfacing", "Crossing.the.center", 
                 "Vertical.movement")

# For the first set of plots
plots1 <- create_plots(data, "Time.spent.in.front.of.the.gate", x_variables)

# For the second set of plots
plots2 <- create_plots(data, "Chamber.switching", x_variables, family = "poisson")

# Combine the plots
combined_plot1 <- wrap_plots(plots1, ncol = 3)
combined_plot2 <- wrap_plots(plots2, ncol = 3)

# Print the combined plots
print(combined_plot1)

print(combined_plot2)

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

# List of y-variables to loop over
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

