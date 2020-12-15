#### Basic plot auxiliaries ####


library(ggplot2)
library(ggthemes)
library(grid)
library(gridExtra)
library(gtable)
library(plyr)
library(sm)
library(scales)


tj_theme_bwnl<-function(){
  theme_bw()+theme(legend.position = 'none')
}

tj_symlog_trans <- function(base = 10, thr = 1, scale = 1){
  trans <- function(x)
    ifelse(abs(x) < thr, x, sign(x) * 
             (thr + scale * suppressWarnings(log(sign(x) * x / thr, base))))
  
  inv <- function(x)
    ifelse(abs(x) < thr, x, sign(x) * 
             base^((sign(x) * x - thr) / scale) * thr)
  
  breaks <- function(x){
    sgn <- sign(x[which.max(abs(x))])
    if(all(abs(x) < thr))
      pretty_breaks()(x)
    else if(prod(x) >= 0){
      if(min(abs(x)) < thr)
        sgn * unique(c(pretty_breaks()(c(min(abs(x)), thr)),
                       log_breaks(base)(c(max(abs(x)), thr))))
      else
        sgn * log_breaks(base)(sgn * x)
    } else {
      if(min(abs(x)) < thr)
        unique(c(sgn * log_breaks()(c(max(abs(x)), thr)),
                 pretty_breaks()(c(sgn * thr, x[which.min(abs(x))]))))
      else
        unique(c(-log_breaks(base)(c(thr, -x[1])),
                 pretty_breaks()(c(-thr, thr)),
                 log_breaks(base)(c(thr, x[2]))))
    }
  }
  trans_new(paste("symlog", thr, base, scale, sep = "-"), trans, inv, breaks)
}


tj_theme_publ<-function (base_size = 12, base_family = "sans",version=1) 
{ if (version==1) {
  ret<-(theme_foundation(base_size = base_size, base_family = base_family) + 
     theme(line = element_line(), 
           rect = element_rect(fill = ggthemes_data$fivethirtyeight["ltgray"], linetype = 0, colour = NA), 
           text = element_text(colour = ggthemes_data$fivethirtyeight["dkgray"]), 
           axis.title = element_text(size=18,face="bold"),
           axis.title.y = element_text(angle=90),
           axis.text = element_text(size=14,face="bold"),
           axis.ticks = element_blank(),
           axis.line.x = element_line(),
           axis.line.y = element_blank(), 
           #legend.position="none",
           #panel.grid = element_line(colour = NULL), 
           panel.grid.major = element_line(colour = ggthemes_data$fivethirtyeight["medgray"]), 
           panel.grid.minor = element_blank(), 
           plot.title = element_text(hjust = 0, size = rel(1.75), face = "bold"), 
           plot.margin = unit(c(1,1, 1, 1), "lines"), 
           strip.background = element_rect(),
           strip.text = element_text(hjust = 0, size = rel(1), face = "bold") ))
  } else if (version==2) {
    bgcolor = "default"
    bgcol <- ggthemes_data$hc$bg[bgcolor]
    ret <- theme(rect = element_rect(fill = bgcol, linetype = 0,colour = NA), 
                 text = element_text(size = base_size, family = base_family), 
                 title = element_text(size=20,hjust = 0.5,face="bold"), 
                 axis.title.x = element_text(size=18,hjust = 0.5,face="bold"), 
                 axis.title.y = element_text(size=18,hjust = 0.5,face="bold"),
                 axis.text = element_text(size=16,face="bold"),
                 axis.ticks.y=element_blank(),
                 axis.ticks.x = element_line(),
                 axis.ticks.length=unit(0.2,"cm"),
                 axis.line = element_line(size = 1,colour="black",linetype="solid"),
                 panel.grid.major.y = element_line(color = "gray"), 
                 panel.grid.minor.y = element_blank(), 
                 panel.grid.major.x = element_blank(), 
                 panel.grid.minor.x = element_blank(), 
                 panel.border = element_blank(), 
                 panel.background = element_blank(), 
                 legend.position = "right")
  } else if (version==3) {
    bgcolor = "default"
    bgcol <- ggthemes_data$hc$bg[bgcolor]
    ret <- theme(rect = element_rect(fill = bgcol, linetype = 0,colour = NA), 
                 text = element_text(size = base_size, family = base_family), 
                 title = element_text(size=20,hjust = 0.5,face="bold"), 
                 axis.title.x = element_blank(),#element_text(size=18,hjust = 0.5,face="bold"), 
                 axis.title.y = element_blank(),#element_text(size=18,hjust = 0.5,face="bold"),
                 axis.text = element_text(size=16,face="bold"),
                 axis.text.y = element_blank(),
                 axis.ticks.y=element_blank(),
                 axis.ticks.x = element_line(),
                 axis.ticks.length=unit(0.2,"cm"),
                 axis.line = element_line(size = 1,colour="black",linetype="solid"),
                 axis.line.y = element_blank(),
                 panel.grid.major.y = element_blank(), 
                 panel.grid.minor.y = element_blank(), 
                 panel.grid.major.x = element_blank(), 
                 panel.grid.minor.x = element_blank(), 
                 panel.border = element_blank(), 
                 panel.background = element_blank(), 
                 legend.position = "right")
  }  else {
    bgcolor = "darkunica"
    bgcol <- ggthemes_data$hc$bg[bgcolor]
    ret <- theme(rect = element_rect(fill = bgcol, linetype = 0,colour = NA), 
                 text = element_text(size = base_size, family = base_family), 
                 title = element_text(size=20,hjust = 0.5,face="bold"), 
                 axis.title.x = element_text(size=18,hjust = 0.5,face="bold"), 
                 axis.title.y = element_text(size=18,hjust = 0.5,face="bold"),
                 axis.text = element_text(size=16,face="bold"),
                 panel.grid.major.y = element_line(color = "gray"), 
                 panel.grid.minor.y = element_blank(), 
                 panel.grid.major.x = element_blank(), 
                 panel.grid.minor.x = element_blank(), 
                 panel.border = element_blank(), 
                 panel.background = element_blank(), 
                 legend.position = "none")
      ret <- (ret + theme(rect = element_rect(fill = bgcol), 
                          text = element_text(colour = "#A0A0A3"), 
                          title = element_text(colour = "#FFFFFF"), 
                          axis.title.x = element_text(colour = "#A0A0A3"), 
                          axis.title.y = element_text(colour = "#A0A0A3"), 
                          panel.grid.major.y = element_line(color = "gray"), 
                          legend.title = element_text(colour = "#A0A0A3")))
  }
  ret
}



### functions ####
#plots - time, var, par, method
tj_get_legend<-function(myggplot){
  tmp <- ggplotGrob(myggplot + theme(legend.position="right"))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}



tj_fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  #print(l)
  #print(class(l)=="character")
  if (class(l)=="character"){
    l=as.numeric(l)
  }
  l <- format(l, digits=3,scientific = TRUE)
  l <- gsub("0e\\+00","0",l)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  l <- gsub("\\+", "", l)
  # return this as an expression
  parse(text=l)
}


tj_fancy2_scientific<-function(plot,digits=2){
  temp_labelsx=ggplot_build(plot)$layout$panel_ranges[[1]]$y.labels
  temp_labelsy=ggplot_build(plot)$layout$panel_ranges[[1]]$y.labels
  
  coeff_x=as.numeric(names(sort(table(floor(log10(as.numeric(temp_labelsx)))),decreasing = TRUE))[1])
  coeff_y=as.numeric(names(sort(table(floor(log10(as.numeric(temp_labelsy)))),decreasing = TRUE))[1])
  
  new_labelsx=round(as.numeric(temp_labelsx)/(10^coeff_x),digits=digits)
  new_labelsy=round(as.numeric(temp_labelsy)/(10^coeff_y),digits=digits)
  
  plot+scale_x_continuous(labels=new_labelsx,breaks=as.numeric(temp_labelsx))+scale_y_continuous(labels=new_labelsy,breaks=as.numeric(temp_labelsy))
}