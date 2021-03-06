# Scrip 3: Manuscript figures
# Alejandro de la Fuente Pinero
# contact: alejandro.delafuentepinero1@my.jcu.edu.au


# Load libraries ----------------------------------------------------------


-----------------------
library(tidyverse)
library(patchwork)
-----------------------
  
setwd("/abundance_vs_suitability/data")


# Load results ------------------------------------------------------------


results <- read.csv("results.csv")


# figure 2 ----------------------------------------------------------------


(fig2a <- results %>% ggplot()+
   scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1), limits = c(0,1), labels = c(0,0.25,0.5,0.75,1))+
   scale_y_continuous(breaks = c(0,0.5,1,1.5,2,2.5,3))+
   geom_density(aes(x = obsDen_HS_spearman), fill = "dark grey", col = "black", alpha = 0.6)+
   labs(x = expression(paste("abundance-suitability correlation (",rho,")")), y = "")+
   theme(panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         panel.background = element_blank(), 
         axis.line = element_line(colour = "black"),
         axis.title = element_text(size = 14),
         axis.text = element_text(size = 12, colour = "black"),
         legend.title = element_blank(),
         legend.position = c(0.15,0.90),
         strip.background = element_blank(),
         strip.text.x = element_blank())
 
)

(fig2b <- results %>% ggplot()+
    scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1), limits = c(0,1), labels = c(0,0.25,0.5,0.75,1))+
    scale_y_continuous(breaks = c(0,0.5,1,1.5,2,2.5,3))+
    geom_density(aes(x = obsDen_HS_deviance_explained_gam), fill = "black", col = "black", alpha = 0.6)+
    labs(x = expression(paste("Predictive power")), y = "")+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12, colour = "black"),
          legend.title = element_blank(),
          legend.position = c(0.15,0.90),
          strip.background = element_blank(),
          strip.text.x = element_blank())
  
)



(fig2c <- results %>% ggplot()+
    scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1), limits = c(0,1), labels = c(0,0.25,0.5,0.75,1))+
    scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9))+
    geom_density(aes(x = obsDen_HS_spearman), fill = "dark grey", col = "black", alpha = 0.6)+
    geom_density(aes(x = obsDen_HS_deviance_explained_gam), fill =  "black", col = "black", alpha = 0.6)+
    #labs(x = expression(paste("abundance-suitability relationship (",rho,")")), y = "")+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          axis.title = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text = element_text(size = 12, colour = "black"),
          legend.title = element_blank(),
          legend.position = c(0.15,0.90),
          strip.background = element_blank(),
          strip.text.x = element_blank())+
    facet_wrap(~taxa, scales = "free_y")
  
)


add_global_label <- function(pwobj, Xlab = NULL, Ylab = NULL, Xgap = 0.03, Ygap = 0.03, ...) {
  ylabgrob <- patchwork::plot_spacer()
  if (!is.null(Ylab)) {
    ylabgrob <- ggplot() +
      geom_text(aes(x = .5, y = .5), label = Ylab, angle = 90, ...) +
      theme_void()
  }
  if (!is.null(Xlab)) {
    xlabgrob <- ggplot() +
      geom_text(aes(x = .5, y = .5), label = Xlab, ...) +
      theme_void()
  }
  if (!is.null(Ylab) & is.null(Xlab)) {
    return((ylabgrob + patchworkGrob(pwobj)) + 
             patchwork::plot_layout(widths = 100 * c(Ygap, 1 - Ygap)))
  }
  if (is.null(Ylab) & !is.null(Xlab)) {
    return((ylabgrob + pwobj) + 
             (xlabgrob) +
             patchwork::plot_layout(heights = 100 * c(1 - Xgap, Xgap),
                                    widths = c(0, 100),
                                    design = "
                                   AB
                                   CC
                                   "
             ))
  }
  if (!is.null(Ylab) & !is.null(Xlab)) {
    return((ylabgrob + pwobj) + 
             (xlabgrob) +
             patchwork::plot_layout(heights = 100 * c(1 - Xgap, Xgap),
                                    widths = 100 * c(Ygap, 1 - Ygap),
                                    design = "
                                   AB
                                   CC
                                   "
             ))
  }
  return(pwobj)
}




fig2 <- (fig2a / fig2b) |fig1c

fig2<- fig2 %>% add_global_label(Ylab=expression(paste("Density of values")), 
                          Ygap=0.05,
                          size = 5,
                          family = "Arial")

fig1

# figure 3 ----------------------------------------------------------------

(fig3a <- results %>% ggplot(aes(pot_dispersal2, obsDen_HS_spearman))+
    geom_point(size = 2, aes(shape = taxa))+
    labs(x = "Potential dispersal index", y = expression(paste("Observed density ~ habitat suitability (",rho,")")))+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 12, colour = "black"),
          legend.title = element_blank(),
          legend.position = c(0.85,0.28),
          strip.background = element_blank(),
          strip.text.x = element_blank())+
    geom_smooth(method = "lm", colour = "black")
  
) 

(fig3b <- results %>% ggplot(aes(log_mass, obsDen_HS_spearman))+
    geom_point(size = 2, aes(shape = taxa))+
    labs(x = "log(mass) (g)", y = expression(paste("Observed density ~ habitat suitability (",rho,")")))+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.title.y = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12, colour = "black"),
          legend.title = element_blank(),
          legend.position = "none",
          strip.background = element_blank(),
          strip.text.x = element_blank())+
    geom_smooth(method = "lm", colour = "black")
  
)

(fig3c <- results %>% ggplot(aes(log_presence, obsDen_HS_spearman))+
    geom_point(size = 2, aes(shape = taxa))+
    labs(x = "log(occurrences)", y = expression(paste("Observed density ~ habitat suitability (",rho,")")))+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12, colour = "black"),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "none",
          strip.background = element_blank(),
          strip.text.x = element_blank())+
    geom_smooth(method = "lm", colour = "black")
  
)


(fig3d <- results %>% ggplot(aes(realized_dist, obsDen_HS_spearman))+
    geom_point(size = 2, aes(shape = taxa))+
    labs(x = expression(paste("log(realised distribution) (Km"^2,")")), y = expression(paste("Observed density ~ habitat suitability (",rho,")")))+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12, colour = "black"),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "none",
          strip.background = element_blank(),
          strip.text.x = element_blank())+
    geom_smooth(method = "lm", colour = "black")
  
)





wrap_plots(fig3a,fig3b, fig3c, fig3d,ncol=2) %>% add_global_label(Ylab=expression(paste("abundance-suitability relationship (   ",rho," )")), 
                                                                  Ygap=0.05,
                                                                  size = 5)

#################
##End of script##
#################