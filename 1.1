# pubplots

#PUBPLOTS VERSION 1.1: independent nonparametric data

ppI <- function(dat) {
  library(ggplot2)
  dat$Group <- as.factor(dat$Group) #formatting data
  
  #theme for publication
  theme_Publication <- function(base_size=15, base_family="Helvetica") {
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
            legend.direction = "horizontalhe",
            legend.key.size= unit(0.2, "cm"),
            legend.spacing = unit(c(0,0,0,0), "cm"),
            legend.title = element_text(face="italic"),
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
  
  #generate boxplot
  bp <- ggplot(dat, aes(Group, Value)) +
    stat_boxplot(geom = "errorbar", width = 0.25) +
    geom_boxplot() +
    theme_Publication()
  
  # We'll be using the Mann-Whitney U test to determine significance
  
  #2 groups
  if(length(unique(dat$Group))=="2"){
    #running the test btwn each group
    test1_2 <- wilcox.test(which(dat$Group==1),which(dat$Group==2), paired=FALSE)$p.val
    
    #defining data frames for significance bars
    df1 <- data.frame(a = c(1:4), b = c(1:4))
    
    #now to add the significance bars
    bp + 
      geom_line(data=df1,aes(x = c(1,1,2,2), y = c(max(dat$Value)+15,max(dat$Value)+16,max(dat$Value)+16,max(dat$Value)+15))) +
      annotate("text", x = 1.5, y = max(dat$Value)+20, label = if(test1_2<=0.05)"*"else"n.s.", size = 5)
  }
  
  #if there are 3 groups
  if(length(unique(dat$Group))=="3"){
    
    #running the test btwn each group
    test1_2 <- wilcox.test(which(dat$Group==1),which(dat$Group==2), paired=FALSE)$p.val
    test1_3 <- wilcox.test(which(dat$Group==1),which(dat$Group==3), paired=FALSE)$p.val
    test2_3 <- wilcox.test(which(dat$Group==2),which(dat$Group==3), paired=FALSE)$p.val
    
    #defining data frames for significance bars
    df1 <- data.frame(a = c(1:5), b = c(1:5))
    df2 <- data.frame(a = c(1:4), b = c(1:4))
    df3 <- data.frame(a = c(1:4), b = c(1:4))
    
    #now to add the significance bars
    bp + geom_line(data=df1,aes(x = c(1,1,2,3,3), y = c(max(dat$Value)+25,max(dat$Value)+26,max(dat$Value)+26,max(dat$Value)+26,max(dat$Value)+25))) +
      annotate("text", x = 2, y = max(dat$Value)+30, label = if(test1_3<=0.05)"*"else"n.s.", size = 5) + 
      geom_line(data=df2,aes(x = c(1,1,2,2), y = c(max(dat$Value)+15,max(dat$Value)+16,max(dat$Value)+16,max(dat$Value)+15))) +
      annotate("text", x = 1.5, y = max(dat$Value)+20, label = if(test1_2<=0.05)"**"else"n.s.", size = 5) +
      geom_line(data=df3,aes(x = c(2,2,3,3), y = c(max(dat$Value)+5,max(dat$Value)+6,max(dat$Value)+6,max(dat$Value)+5))) +
      annotate("text", x = 2.5, y = max(dat$Value)+10, label = if(test2_3<=0.05)"***"else"n.s.", size = 5)}
  
  else {
    print("Too many or not enough groups. Please only enter dataframes with 2 or 3 groups.")
  }
}
