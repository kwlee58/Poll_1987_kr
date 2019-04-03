mosaic_gg <- function(tbl, base_family ="", ggtitle ="", xlab =""){
tbl.df <- as.data.frame(tbl)
tbl.sum <- tapply(tbl.df$Freq, tbl.df[, 2], sum)
tbl.p.m <- prop.table(tbl.sum)
tbl.p <- prop.table(tbl)
tbl.p.2 <- prop.table(tbl, margin = 2)
tbl.p.df <- as.data.frame(tbl.p)
tbl.p.df$width <- tbl.p.m[match(tbl.p.df[, 2], names(tbl.p.m))]
tbl.p.df$height <- as.data.frame(tbl.p.2)$Freq
tbl.p.df$label.height <- unlist(tapply(tbl.p.df$height, tbl.p.df[, 2], cumsum))
x.center <- (cumsum(tbl.p.m) + c(0, head(cumsum(tbl.p.m), -1)))/2
tbl.p.df$center <- x.center[match(tbl.p.df[, 2], names(x.center))]
m1 <- ggplot(tbl.p.df, aes(x = center, y = height, width = width)) + 
  geom_bar(aes(fill = vote), stat = "identity", col = "white", size = 1, position = position_stack(reverse = TRUE)) 
m1
m2 <- m1 + 
  theme_bw(base_family = base_family)
m2
m3 <- m2 + 
  geom_text(aes(x = center, y = 1.05), label = tbl.p.df[, 2], family = base_family)
m3
m4 <- m3 + 
  geom_text(aes(x = center, y = label.height/2), label = format(tbl.df$Freq, big.mark = ","), position = position_stack())
m4
x.breaks <- c(0, ifelse(cumsum(tbl.p.m) < 0.1, 0.0, cumsum(tbl.p.m)))
x.label <- format(x.breaks, digits = 2, nsmall = 2)
m5 <- m4 + 
  scale_x_continuous(name = xlab, breaks = x.breaks, label = x.label) + 
  scale_y_continuous(name = "Pros or Cons") + 
  scale_fill_manual(name = "Pros or Cons", values = rainbow(2)[2:1], guide = guide_legend(reverse = TRUE)) +
  ggtitle(ggtitle) +
  theme(plot.margin = unit(c(1, 2, 1, 1), "lines"))
m5
return(m5)
}
