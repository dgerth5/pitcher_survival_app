# server
library(shiny)
library(tidyverse)
library(ggplot2)

posterior_data <- readRDS("model_data.RDS")
post_alpha <- posterior_data$post_alpha
post_beta <- posterior_data$post_beta

server <- function(input, output) {
  
  get_pred_matrix <- function(new_data, alpha_mat, beta_mat){
    linpred <- exp(beta_mat %*% new_data)
    new_m <- matrix(rep(seq(0, 0.99, 0.01), times = length(alpha_mat)),
                    nrow = length(alpha_mat),
                    byrow = TRUE)
    final <- as.data.frame(qweibull(new_m, alpha_mat, linpred))
    return(final)
  }
  
  output$survivalPlot <- renderPlot({
    pitcher1 <- c(1, input$Age_Row1, input$IP_Row1, input$k_Row1, input$b_Row1, as.numeric(input$starter_Row1))
    pitcher2 <- c(1, input$Age_Row2, input$IP_Row2, input$k_Row2, input$b_Row2, as.numeric(input$starter_Row2))
    
    pm <- as.data.frame(get_pred_matrix(pitcher1, post_alpha, post_beta)[1:50,])
    pm$name <- "Pitcher1"
    pm2 <- as.data.frame(get_pred_matrix(pitcher2, post_alpha, post_beta)[1:50,])
    pm2$name <- "Pitcher2"
    
    plot_df <- rbind(pm, pm2)
    
    # Define x-axis values
    x_values <- seq(1, .01, by = -0.01)
    
    # Create an empty plot with appropriate labels and limits
    plot(rep(0, length(x_values)), x_values, type = "n", ylab = "Survival Probability", xlab = "Innings Pitched", xlim = c(0, 2000))
    
    # Loop through each row and plot the lines
    for (i in 1:nrow(plot_df)) {
      if (plot_df$name[i] == "Pitcher1") {
        lines(as.numeric(plot_df[i, 1:(ncol(plot_df)-1)]), x_values, col = "#1E88E5")
      } else {
        lines(as.numeric(plot_df[i, 1:(ncol(plot_df)-1)]), x_values, col = "#D81B60")
      }
    }
    
    # Add a legend
    legend("topright", legend = c("Pitcher1", "Pitcher2"), col = c("#1E88E5", "#D81B60"), lty = 1)
    
  })
}
