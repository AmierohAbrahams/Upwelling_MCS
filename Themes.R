# Themes

theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            #legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

library("grid")
theme_sharp <- function(base_size = 10, base_family = "")
{
  theme(line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
        rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
        text = element_text(family = base_family, face = "plain",
                            colour = "black", size = base_size,
                            hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
        axis.text = element_text(size = rel(0.9), colour = "black"),
        axis.text.x = element_text(vjust = 1),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_text(vjust = 0),
        axis.title.y = element_text(angle = 90, vjust = 0.3),
        axis.ticks = element_line(colour = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks.margin = unit(0.1, "cm"),
        axis.line = element_line(),
        legend.background = element_blank(),
        legend.margin = unit(0.2, "cm"),
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.key.size = unit(1.2, "lines"),
        legend.key.height = NULL,
        legend.key.width = NULL,
        legend.text = element_text(size = rel(0.9)),
        legend.text.align = NULL,
        legend.title = element_text(face = "bold", hjust = 0),
        legend.title.align = 0.5,
        legend.position = "right",
        legend.direction = NULL,
        legend.justification = "center",
        legend.box = NULL,
        panel.background = element_rect(fill = "white", colour = "black", size = 0.5),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", size = 0.2, colour = "black"),
        panel.grid.minor = element_blank(),
        panel.margin = unit(0.25, "lines"),
        strip.text = element_text(size = rel(1.0)),
        strip.background = element_rect(fill = "white", colour = NA),
        strip.text.x = element_text(),
        strip.text.y = element_text(angle = -90),        
        plot.background = element_rect(colour = "white"),
        plot.title = element_text(size = rel(1.2), vjust = 1),
        plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
        complete = TRUE
  )
}
