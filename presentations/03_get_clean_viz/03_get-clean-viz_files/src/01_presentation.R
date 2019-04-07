theme_presi <- function (base_size = 12, base_family = "") 
{
        
        
        theme_bw(base_size = base_size, base_family = base_family) %+replace% 
                theme(plot.title = element_text(size = rel(1.4), face = 'bold',
                                                margin = margin(t = 0, r = 0, b = 15, l = 0)),
                        
                        axis.title = element_text(size = rel(1.4), face ='bold'),
                      axis.text = element_text(size = rel(1.2)),
                      
                      axis.ticks = element_line(colour = "black"), 
                      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0),
                                                  angle = 90),
                      axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 1, l = 0)),
                      
                      legend.title = element_text(size = rel(1.2), face = 'bold'),
                      legend.text = element_text(size = rel(1)),
                      legend.key = element_rect(colour = "grey80"),
                      panel.background = element_rect(fill = "white", 
                                                      colour = NA), 
                      panel.border = element_rect(fill = NA, 
                                                  colour = "black"),
                      panel.grid = element_blank(),
                      strip.background = element_rect(fill = "grey80", 
                                                      colour = "grey50", 
                                                      size = 0.4),
                      strip.text = element_text(size = rel(1.2), face = 'bold'))
        
}
