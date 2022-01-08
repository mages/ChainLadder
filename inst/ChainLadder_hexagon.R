library(hexSticker)
library(ggplot2)
DF1 <- data.frame(x=c(0,0,1), y=c(0,1,1))
DF2 <- data.frame(x=c(0,1,1), y=c(0,1,0))
p <- ggplot(DF1, aes(x=x, y=y)) + 
  geom_polygon(col="black", fill="deepskyblue") + 
  geom_polygon(data=DF2, col="black", fill="white")
p <- p + theme_transparent()  + theme_void() + theme(legend.position = 'none')
p
sticker(p, package="ChainLadder", p_size=20, s_x=1, s_y=.75, s_width=1, s_height=0.91,
        h_fill="black", h_color="lightgrey",
        filename="man/figures/logo.png")
