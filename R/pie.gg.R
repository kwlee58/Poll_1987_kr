pie.gg <-
function(df, ggtitle = "", font.family = ""){
  n <- length(names(df))
  y.coord <- cumsum(df$Freq)
  pie.label <- paste(levels(df$vote), format(df$Freq, big.mark = ","), 
                     sep = "\n") 
  p1 <- ggplot(df, aes(x = "", 
                       y = Freq, 
                       fill = vote)) 
  p2 <- p1 + 
    geom_bar(width = 1, 
             stat = "identity", 
             position = position_stack(reverse = TRUE))
  pie.1 <- p2 + 
    coord_polar(theta = "y", 
                start = 3 * pi / 2, 
                direction = -1)
  pie.2 <- pie.1 + 
    scale_y_continuous(name = "", 
                       breaks = NULL) +
    scale_x_discrete(name = "") 
  pie.3 <- pie.2 +
    scale_fill_manual(name = "", 
                      values = rainbow(n)[n:1])
  pie.4 <- pie.3 +
    theme_void(base_family = font.family)
  pie.5 <- pie.4 +
    guides(fill = "none")
  pie.6 <- pie.5 +
    geom_text(aes(y = y.coord/2), 
              label = pie.label, 
              family = font.family, 
              position = position_stack(reverse = TRUE))
  pie.7 <- pie.6 +
    ggtitle(ggtitle) + 
    theme(plot.margin = unit(c(1, 1, 1.5, 1), "lines"), 
          plot.title = element_text(hjust = 0.5))
  return(pie.7)
}
