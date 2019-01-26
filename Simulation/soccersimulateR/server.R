library(ggplot2)
library(dplyr)
library(patchwork)

# Define server logic for soccer winning percentage simulate app ----
server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$winning_percentage <- renderPlot({
    res <- matrix(0, nrow=input$n, ncol=1)
    
    for (n in 1:input$n) {
      res_a <- runif(input$time)
      res_b <- runif(input$time)
      
      score_a <- input$score_A + sum(res_a < (input$expected_score_A / 90))
      score_b <- input$score_B + sum(res_b < (input$expected_score_B / 90))
      
      if (score_a == score_b) { #引き分け
        res[n, 1] <- "DRAW"
      } else if (score_a > score_b) { #Aの勝ち
        res[n, 1] <- "WIN"
      } else  #Aの負け
        res[n, 1] <- "LOSE"
    }
    #集計&plot
    res %>% 
      as.data.frame() %>% 
      dplyr::rename(result=V1) %>%
      dplyr::group_by(result) %>% 
      dplyr::summarise(rate = n() / input$n) %>% 
      transform(result = factor(result, levels = c("WIN", "DRAW", "LOSE"))) %>%
      ggplot(aes(x=result, y=rate, fill=result)) +
      geom_col() +
      geom_text(aes(label=round(rate, 3), vjust=-0.2))
  })
  
  # Generate a summary of the data ----
  output$distribution_score <- renderPlot({
    res <- matrix(0, nrow=input$n, ncol=2)
    
    for (n in 1:input$n) {
      res_a <- runif(input$time)
      res_b <- runif(input$time)
      
      score_a <- input$score_A + sum(res_a < (input$expected_score_A / 90))
      score_b <- input$score_B + sum(res_b < (input$expected_score_B / 90))
      
      res[n, 1] <- score_a
      res[n, 2] <- score_b
      } 
    #集計&plot
    res %>% 
      as.data.frame() %>% 
      dplyr::rename(scoreA=V1, scoreB=V2) -> tmp
    
    tmp %>% 
      dplyr::select(scoreA) %>% 
      dplyr::group_by(scoreA) %>% 
      dplyr::summarise(rate = n() / input$n) %>% 
      ggplot(aes(x=scoreA, y=rate)) +
      geom_line() +
      geom_point() + 
      ggtitle("team A") -> p1
    
    tmp %>% 
      dplyr::select(scoreB) %>% 
      dplyr::group_by(scoreB) %>% 
      dplyr::summarise(rate = n() / input$n) %>% 
      ggplot(aes(x=scoreB, y=rate)) +
      geom_line() +
      geom_point() + 
      ggtitle("team B") -> p2
    
    p1 / p2
  })
  
  # Generate an HTML table view of the data ----
  output$percentage_transition <- renderPlot({
    res <- matrix(0, nrow=input$n, ncol=1)
    
    df <- data.frame(matrix(rep(NA, 4), nrow=1))[numeric(0), ]
    colnames(df) <- c("DRAW", "LOSE", "WIN", "remaining_time")
    
    for (t in input$time:1) {
      
      for (n in 1:input$n) {
        
        res_a <- runif(t)
        res_b <- runif(t)
        
        score_a <- input$score_A + sum(res_a < input$expected_score_A / 90)
        score_b <- input$score_B + sum(res_b < input$expected_score_B / 90)
        
        if (score_a == score_b) { #引き分け
          res[n, 1] <- "DRAW"
        } else if (score_a > score_b) { #aの勝ち
          res[n, 1] <- "WIN"
        } else  #aの負け
          res[n, 1] <- "LOSE"
      }
      
      tmp <- 
        res %>% 
        as.data.frame() %>% 
        dplyr::rename(result=V1) %>%
        dplyr::group_by(result) %>% 
        dplyr::summarise(rate = n() / input$n * 100) %>%
        tidyr::spread(key = result, value = rate) %>% 
        dplyr::mutate(remaining_time=t)
      
      df <- dplyr::bind_rows(df, tmp)
      
      res <- matrix(0, nrow=input$n, ncol=1)
    }
    
    df %>% 
      tidyr::gather(key = result, value = rate, WIN, DRAW, LOSE) %>%
      transform(result = factor(result, levels = c("LOSE", "DRAW", "WIN"))) %>%
      ggplot(aes(x = remaining_time, y = rate, color = result)) +
      geom_line(position="stack") + 
      geom_ribbon(aes(ymin = 0, ymax = 1, group = result, fill = result), position = "stack", alpha = 0.7) +
      scale_x_reverse() + 
      xlab("remaining_time[min]") +
      ylab("rate[%]")
  })
  
}