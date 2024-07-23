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
    final <- qweibull(new_m, alpha_mat, linpred)
    return(final)
  }
  
  plot_df_fn <- function(mat){
    col_medians <- apply(mat, 2, function(x) quantile(x, 0.5))
    col_10th_percentile <- apply(mat, 2, function(x) quantile(x, 0.1))
    col_90th_percentile <- apply(mat, 2, function(x) quantile(x, 0.9))
    plot_data <- data.frame(
      Variable = seq(0.99, 0, -0.01),
      Median = col_medians,
      Q10 = col_10th_percentile,
      Q90 = col_90th_percentile
    )
    return(plot_data)
  }
  
  output$survivalPlot <- renderPlot({
    pitcher1 <- c(1, input$Age_Row1, input$IP_Row1, input$k_Row1, input$b_Row1, as.numeric(input$starter_Row1))
    pitcher2 <- c(1, input$Age_Row2, input$IP_Row2, input$k_Row2, input$b_Row2, as.numeric(input$starter_Row2))
    
    pm <- get_pred_matrix(pitcher1, post_alpha, post_beta)
    pm2 <- get_pred_matrix(pitcher2, post_alpha, post_beta)
    
    plot_data <- plot_df_fn(pm)
    plot_data2 <- plot_df_fn(pm2) %>%
      rename("Median_1" = Median,
             "Q10_1" = Q10,
             "Q90_1" = Q90) %>%
      select(-Variable)
    
    final_df <- bind_cols(plot_data, plot_data2)
    
    ggplot(final_df, aes(x = Variable)) +
      geom_ribbon(aes(ymin = Q10, ymax = Q90), fill = "orange", alpha = 0.2, na.rm = TRUE) +
      geom_line(aes(y = Median, group = 1, color = "Pitcher 1"), size = 1, na.rm = TRUE) +
      geom_line(aes(y = Q10, group = 1), color = "grey", size = 0.5, na.rm = TRUE) +
      geom_line(aes(y = Q90, group = 1), color = "grey", size = 0.5, na.rm = TRUE) +
      geom_ribbon(aes(ymin = Q10_1, ymax = Q90_1), fill = "green", alpha = 0.2, na.rm = TRUE) +
      geom_line(aes(y = Median_1, group = 1, color = "Pitcher 2"), size = 1, na.rm = TRUE) +
      geom_line(aes(y = Q10_1, group = 1), color = "grey", size = 0.5, na.rm = TRUE) +
      geom_line(aes(y = Q90_1, group = 1), color = "grey", size = 0.5, na.rm = TRUE) +
      labs(y = "Innings Pitched Until Retirement", x = "Survival Probability", color = "Legend") +
      scale_y_continuous(limits = c(0, 1000), expand = c(0, 0)) +
      scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
      scale_color_manual(values = c("Pitcher 1" = "blue", "Pitcher 2" = "red")) +
      theme_minimal() +
      coord_flip() +
      theme(legend.position = "right")
    
  })
}
