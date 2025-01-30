#* plot function ----
theme_mine_single <- function(gg_input_single) 
{ 
  gg_input_single +
    geom_hline(aes(yintercept = 0.5), 
               colour = "black", 
               linetype = 2, 
               size = .4) +
    geom_point(size = 1,
               position = position_dodge(0.3)) +
    geom_errorbar(aes(ymax = lower, 
                      ymin = upper), 
                  width = 0.05,
                  position = position_dodge(0.3)) +
    theme_bw() +
    scale_x_discrete(position="top") +
    xlab("") +
    ylab("Marginal Means") +
    guides(color = "none") + 
    scale_color_manual(values = viridis::inferno(10))+
    theme(strip.background = element_rect(colour="grey90", fill=NA),
          panel.background = element_rect(colour="grey90"),
          strip.text.y.left = element_text(angle = 0, color = "grey30",size = 9,face="bold",hjust = T),
          axis.ticks = element_line(color = "grey70"),
          strip.text.x = element_text(angle = 0, color = "grey30", size = 9),
          strip.text.y = element_text(angle = 0, color = "grey30", size = 9),
          axis.title = element_text(color = "grey30", size = 9),
          panel.border = element_rect(colour = "grey90"),
          axis.text = element_text(size = 6))
}



theme_mine_double <- function(gg_input_double, base_size = 10.5)
{
  gg_input_double + 
    scale_color_grey(end = 0.6) +
    scale_shape_manual(values =c(0,3)) +
    geom_hline(aes(yintercept = 0.5), 
               colour = "black", 
               linetype = 2, 
               size = .4) +
    geom_errorbar(aes(ymax = lower, 
                      ymin = upper), 
                  width = 0.05,
                  position = position_dodge(0.3)) +
    theme_bw(base_size = base_size) +
    scale_x_discrete(position="top") +
    xlab("") +
    ylab("Marginal Means") +
    #guides(color = "none") + 
    #scale_color_manual(values = viridis::inferno(10))+
    theme(strip.background = element_rect(colour="grey70", fill=NA),
          panel.background = element_rect(colour="grey90"),
          strip.text.y.left = element_text(angle = 0, color = "grey30",size = 11,face="bold"),
          axis.ticks = element_line(color = "grey50"),
          strip.text.x = element_text(angle = 0, color = "grey30", size = 11),
          strip.text.y = element_text(angle = 0, color = "grey30", size = 11),
          axis.title = element_text(color = "grey30", size = 11),
          panel.border = element_rect(colour = "grey90"),
          legend.position = "bottom", legend.text = element_text(colour = "grey30"), legend.title = element_text(colour = "grey30"))
}  
  
theme_mine_triple <- function(gg_input_triple, base_size = 10.5)
  {
  gg_input_triple + 
      scale_color_grey(end = 0.6) +
      scale_shape_manual(values =c(0,3, 15)) +
      geom_hline(aes(yintercept = 0.5), 
                 colour = "black", 
                 linetype = 2, 
                 size = .4) +
      geom_errorbar(aes(ymax = lower, 
                        ymin = upper), 
                    width = 0.05,
                    position = position_dodge(0.3)) +
      theme_bw(base_size = base_size) +
      scale_x_discrete(position="top") +
      xlab("") +
      ylab("Marginal Means") +
      #guides(color = "none") + 
      #scale_color_manual(values = viridis::inferno(10))+
      theme(strip.background = element_rect(colour="grey70", fill=NA),
            panel.background = element_rect(colour="grey90"),
            strip.text.y.left = element_text(angle = 0, color = "grey30",size = 11,face="bold"),
            axis.ticks = element_line(color = "grey50"),
            strip.text.x = element_text(angle = 0, color = "grey30", size = 11),
            strip.text.y = element_text(angle = 0, color = "grey30", size = 11),
            axis.title = element_text(color = "grey30", size = 11),
            panel.border = element_rect(colour = "grey90"),
            legend.position = "bottom", legend.text = element_text(colour = "grey30"), legend.title = element_text(colour = "grey30"))  
}


theme_mine_quad <- function(gg_input_quad, base_size = 10.5)
{
  gg_input_quad + 
    scale_color_grey(end = 0.6) +
    scale_shape_manual(values =c(0, 3, 15, 20)) +
    geom_hline(aes(yintercept = 0.5), 
               colour = "black", 
               linetype = 2, 
               size = .4) +
    geom_errorbar(aes(ymax = lower, 
                      ymin = upper), 
                  width = 0.05,
                  position = position_dodge(0.3)) +
    theme_bw(base_size = base_size) +
    scale_x_discrete(position="top") +
    xlab("") +
    ylab("Marginal Means") +
    #guides(color = "none") + 
    #scale_color_manual(values = viridis::inferno(10))+
    theme(strip.background = element_rect(colour="grey70", fill=NA),
          panel.background = element_rect(colour="grey90"),
          strip.text.y.left = element_text(angle = 0, color = "grey30",size = 11,face="bold"),
          axis.ticks = element_line(color = "grey50"),
          strip.text.x = element_text(angle = 0, color = "grey30", size = 11),
          strip.text.y = element_text(angle = 0, color = "grey30", size = 11),
          axis.title = element_text(color = "grey30", size = 11),
          panel.border = element_rect(colour = "grey90"),
          legend.position = "bottom", legend.text = element_text(colour = "grey30"), legend.title = element_text(colour = "grey30"))  
}


################################################################# 
#* To show contrast ----
#################################################################
theme_mine_single_contrast <- function(gg_input_single_contrast) 
{ 
  gg_input_single_contrast +
    geom_hline(aes(yintercept = 0), 
               colour = "black", 
               linetype = 2, 
               size = .4) +
    geom_point(size = 1,
               position = position_dodge(0.3)) +
    geom_errorbar(aes(ymax = lower, 
                      ymin = upper), 
                  width = 0.05,
                  position = position_dodge(0.3)) +
    theme_bw() +
    scale_x_discrete(position="top") +
    xlab("") +
    ylab("Marginal Means") +
    guides(color = "none") + 
    scale_color_manual(values = viridis::inferno(10))+
    theme(strip.background = element_rect(colour="grey90", fill=NA),
          panel.background = element_rect(colour="grey90"),
          strip.text.y.left = element_text(angle = 0, color = "grey30",size = 9,face="bold",hjust = T),
          axis.ticks = element_line(color = "grey70"),
          strip.text.x = element_text(angle = 0, color = "grey30", size = 9),
          strip.text.y = element_text(angle = 0, color = "grey30", size = 9),
          axis.title = element_text(color = "grey30", size = 9),
          panel.border = element_rect(colour = "grey90"),
          axis.text = element_text(size = 6))
}



theme_mine_double_contrast <- function(gg_input_double_contrast, base_size = 10.5)
{
  gg_input_double_contrast + 
    scale_color_grey(end = 0.6) +
    scale_shape_manual(values =c(0,3)) +
    geom_hline(aes(yintercept = 0), 
               colour = "black", 
               linetype = 2, 
               size = .4) +
    geom_errorbar(aes(ymax = lower, 
                      ymin = upper), 
                  width = 0.05,
                  position = position_dodge(0.3)) +
    theme_bw(base_size = base_size) +
    scale_x_discrete(position="top") +
    xlab("") +
    ylab("Marginal Means") +
    #guides(color = "none") + 
    #scale_color_manual(values = viridis::inferno(10))+
    theme(strip.background = element_rect(colour="grey70", fill=NA),
          panel.background = element_rect(colour="grey90"),
          strip.text.y.left = element_text(angle = 0, color = "grey30",size = 11,face="bold"),
          axis.ticks = element_line(color = "grey50"),
          strip.text.x = element_text(angle = 0, color = "grey30", size = 11),
          strip.text.y = element_text(angle = 0, color = "grey30", size = 11),
          axis.title = element_text(color = "grey30", size = 11),
          panel.border = element_rect(colour = "grey90"),
          legend.position = "bottom", legend.text = element_text(colour = "grey30"), legend.title = element_text(colour = "grey30"))
}  

theme_mine_triple_contrast <- function(gg_input_triple_contrast, base_size = 10.5)
{
  gg_input_triple_contrast + 
    scale_color_grey(end = 0.6) +
    scale_shape_manual(values =c(0,3, 15)) +
    geom_hline(aes(yintercept = 0), 
               colour = "black", 
               linetype = 2, 
               size = .4) +
    geom_errorbar(aes(ymax = lower, 
                      ymin = upper), 
                  width = 0.05,
                  position = position_dodge(0.3)) +
    theme_bw(base_size = base_size) +
    scale_x_discrete(position="top") +
    xlab("") +
    ylab("Marginal Means") +
    #guides(color = "none") + 
    #scale_color_manual(values = viridis::inferno(10))+
    theme(strip.background = element_rect(colour="grey70", fill=NA),
          panel.background = element_rect(colour="grey90"),
          strip.text.y.left = element_text(angle = 0, color = "grey30",size = 11,face="bold"),
          axis.ticks = element_line(color = "grey50"),
          strip.text.x = element_text(angle = 0, color = "grey30", size = 11),
          strip.text.y = element_text(angle = 0, color = "grey30", size = 11),
          axis.title = element_text(color = "grey30", size = 11),
          panel.border = element_rect(colour = "grey90"),
          legend.position = "bottom", legend.text = element_text(colour = "grey30"), legend.title = element_text(colour = "grey30"))  
}


theme_mine_quad_contrast <- function(gg_input_quad_contrast, base_size = 10.5)
{
  gg_input_quad_contrast + 
    scale_color_grey(end = 0.6) +
    scale_shape_manual(values =c(0, 3, 15, 20)) +
    geom_hline(aes(yintercept = 0), 
               colour = "black", 
               linetype = 2, 
               size = .4) +
    geom_errorbar(aes(ymax = lower, 
                      ymin = upper), 
                  width = 0.05,
                  position = position_dodge(0.3)) +
    theme_bw(base_size = base_size) +
    scale_x_discrete(position="top") +
    xlab("") +
    ylab("Marginal Means") +
    #guides(color = "none") + 
    #scale_color_manual(values = viridis::inferno(10))+
    theme(strip.background = element_rect(colour="grey70", fill=NA),
          panel.background = element_rect(colour="grey90"),
          strip.text.y.left = element_text(angle = 0, color = "grey30",size = 11,face="bold"),
          axis.ticks = element_line(color = "grey50"),
          strip.text.x = element_text(angle = 0, color = "grey30", size = 11),
          strip.text.y = element_text(angle = 0, color = "grey30", size = 11),
          axis.title = element_text(color = "grey30", size = 11),
          panel.border = element_rect(colour = "grey90"),
          legend.position = "bottom", legend.text = element_text(colour = "grey30"), legend.title = element_text(colour = "grey30"))  
}