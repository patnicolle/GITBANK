add_raster_legend2 <-function(cols,limits,labelss=NULL,x='bottomleft',add=TRUE,
                              spt.cex=2,pch=15,itle='',
                              plot_loc=c(0.22,0.78,0.07,0.10),...) {
  
  cal_frac_of_plot_covered <- function(index=1:2) {
    xlims=par("usr")[index]
    xlims[3]=xlims[2]-xlims[1]
    return(xlims)
  }
  
  if (add) {
    xlims=cal_frac_of_plot_covered(1:2)
    ylims=cal_frac_of_plot_covered(3:4)
  } else {
    xlims=c(0,1,1)
    ylims=c(0,1,1)
    plot(c(0,1),c(0,1))
  }
  nlims=length(limits)+1
  
  x=xlims[1]+plot_loc[1]*xlims[3]+(plot_loc[2]-plot_loc[1])*xlims[3]*(matrix(1:nlims,nlims,1)-0.5)/nlims
  
  y=c(ylims[1]+ylims[3]*plot_loc[3],ylims[1]+ylims[3]*plot_loc[4])
  
  par(xpd=NA)
  image(x=x,y=y,z=matrix(1:nlims,nlims,2),col=cols[1:nlims],add=TRUE,
        xaxt='n',yaxt='n',xlab='',ylab='', xdp=TRUE)
  
  dx=(x[2]-x[1])/2
  xp=c(min(x)-dx,min(x)-dx,max(x)+dx,max(x)+dx)
  
  yp=c(y[1]-diff(y)/2,y[2]+diff(y)/2)
  polygon(xp,c(yp,rev(yp)),xpd=NA) ##xpd=TRUE
  #===============================
  ytop=yp[2]+diff(y) + diff(y)*0.2
  ybottom=yp[1]-diff(y) - diff(y)*0.2
  xt=c(x[1]-dx,x[1],x+dx)
  yt=c(ytop,rep(c(ytop,ybottom),length.out=length(xt)-1))
  if (is.null(labelss)) {
    labelss=c(paste("<",limits[1],sep=""),"",limits,paste(">",tail(limits,n=1),sep=""))
  #  if (limits[1]==0) {
   #   labelss[1]=""
    #  labelss[2]='0'
     # labelss[3]=""
    #}
  } else {
    if(class(labelss)=='list' && length(labelss)==1) labelss=labelss[[1]]
    
    if (limits[1]==0) {
      labelss=c("",labelss[1],"",labelss[-1],"")
    } else {
      labelss=c(labelss[1],"",labelss[-1],"")
    }
    
    if (length(labelss)==(length(xt)-2)) labelss=c("",labelss,"")
    if (length(labelss)==(length(xt)-1)) labelss=c("","",labelss[-2])
  }
  if (length(labelss)==0) labelss=rep("",length(xt))
  text(x=xt,y=yt,labelss,xpd=NA, cex=spt.cex)
  #===============================
}