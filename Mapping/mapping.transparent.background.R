p <- ggplot() + geom_polygon(data = plotData, aes(x=long, y = lat, group = group), color="black", size=0.5, fill="palegreen") + 
  coord_fixed(1.3)+
  theme_bw() +
  theme(panel.border = element_blank(),
        legend.key = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))+
  xlab("")+
  ylab("")
ggsave(p,file="H:/Mapping/Tutorial/cartographic/Own Plots/Transparent.png", bg = "transparent")