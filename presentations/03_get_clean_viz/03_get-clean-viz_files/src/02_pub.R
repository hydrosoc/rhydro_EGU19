# https://rpubs.com/Koundy/71792

theme_pub <- function(base_size=14, base_family="Roboto Condensed") {
        library(grid)
        library(ggthemes)
        library(extrafont)
        
        extrafont::loadfonts("win", quiet = TRUE)
        (theme_foundation(base_size=base_size, base_family=base_family)
                + theme(plot.title = element_text(face = "bold",
                                                  size = rel(1.2), hjust = 0.5),
                        text = element_text(),
                        panel.background = element_rect(colour = NA),
                        plot.background = element_rect(colour = NA),
                        panel.border = element_rect(colour = NA),
                        axis.title = element_text(face = "bold",size = rel(1)),
                        axis.title.y = element_text(angle=90,
                                                    vjust =2,
                                                    margin = margin(t = 0,
                                                              r = 0.4,
                                                              b = 0,
                                                              l = 0, unit = "cm")),
                        axis.title.x = element_text(margin = margin(t = 0.4,
                                                                    r = 0.0,
                                                                    b = 0.0,
                                                                    l = 0.0, unit = "cm")),
                        axis.text = element_text(), 
                        axis.line = element_line(colour="black"),
                        axis.ticks = element_line(),
                        panel.grid.major = element_line(colour="#f0f0f0"),
                        panel.grid.minor = element_blank(),
                        legend.key = element_rect(colour = NA),
                        # legend.position = "bottom",
                        # legend.direction = "horizontal",
                        # legend.key.size = unit(0.75, "cm"),
                        # legend.key.width = unit(1.5, "cm"),
                        legend.margin = margin(0.1,0.1,0.1,0.1, "cm"),
                        # legend.box.margin = margin(t = 0.5, unit = "cm"),
                        legend.title = element_text(face="italic"),
                        plot.margin=unit(c(2,2,2,2),"mm"),
                        strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
                        strip.text = element_text(face="bold")
                ))
        
}

