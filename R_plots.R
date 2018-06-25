############ R plot########################
# fucntions for plot in R

###########################################
# Render a polyline with gradient color
install.packages("RColorBrewer")
library(RColorBrewer)
Color_line <- function(plot_title, X, Y, colsch,fineness,line_width){
  # Plot_title: the title of the plot
  # X : Xs of the points
  # Y : Ys of the points
  # colsch: Color scheme, e.g. c("black", "red")
  # fineness: fineness of the render
  # line_width: width of the line 
  if(fineness < 2){
    print("The fineness cannot be smaller than 2.")
  }else{
    temp_dim=length(X)
    # render the line based on the color scheme
    temp_dim = floor(temp_dim / fineness)
    temp_col = colorRampPalette(colsch)
    render_col=temp_col(temp_dim)
    scr_rate=(max(X)-min(X))/(max(Y)-min(Y))
    x11(width=10*scr_rate, height=10) 
    plot.new()
    frame()
    plot.window(xlim =c(min(X), max(X)+2), ylim = c(min(Y), max(Y)))
    title(main = plot_title, xlab = "X", ylab = "Y" )
    axis(1, labels = TRUE, lwd = 2)
    axis(2, labels = TRUE, lwd = 2)
    for (temp_seg in 1:temp_dim) {
        lines(X[c(((temp_seg-1)*5+1):(temp_seg*5+1))], Y[c(((temp_seg-1)*5+1):(temp_seg*5+1))], type = "l", col=render_col[temp_seg], lwd=line_width)
    }
    if(length(X)>temp_dim*5){
      lines(X[c((temp_dim*5+1):length(X))], Y[c((temp_dim*5+1):length(Y))], type = "l", col=render_col[temp_seg], lwd=line_width)
    }
  }
  # save the figure
    savePlot(filename = plot_title, type = "tiff") 
  }
} 