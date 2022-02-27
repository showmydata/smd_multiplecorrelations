my_points <- function(x, y, x1, y1, sp, nvar, lw, smoothness, digits, perc_rank, dotint, panelcolor, panelcolor2, tintmaxcorr, ...){
  data=isolate_complete_pairs(x,y); if (perc_rank) {data=perc_rank(data)}; x=data[,1]; y=data[,2]; # re-rank data for correct spearman line fits
  a=cor(x=data[,1], y=data[,2]) / tintmaxcorr; if (a>=1) a=.99 else if (a<=-1) a=-.99 
  if (dotint==0) points(x1,y1,...) 
  else if (dotint==1) {
    b=col2rgb(panelcolor)/255; c=rgb(b[1],b[2],b[3],alpha=abs(a)); points(x1,y1,...); 
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = c); points(x1,y1,...); 
  }
  else if (dotint==2) {
    if (a>0) {b=col2rgb(panelcolor)/255; c=rgb(b[1],b[2],b[3],alpha=a); points(x1,y1,...); rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = c); points(x1,y1,...)}
    else {b=col2rgb(panelcolor2)/255; c=rgb(b[1],b[2],b[3],alpha=-a); points(x1,y1,...); rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = c); points(x1,y1,...)}
  }
}

my_line <- function(x, y, x1, y1, sp, nvar, lw, smoothness, digits, perc_rank, dotint, panelcolor, panelcolor2, tintmaxcorr, ...){
  data=isolate_complete_pairs(x,y); if (perc_rank) {data=perc_rank(data)}; x=data[,1]; y=data[,2]; # re-rank data for correct spearman line fits
  a=cor(x=data[,1], y=data[,2]) / tintmaxcorr; 
  if (a>=1) a=.99 else if (a<=-1) a=-.99 
  if (dotint==0) points(x1,y1,...) 
  else if (dotint==1) {
    b=col2rgb(panelcolor)/255; c=rgb(b[1],b[2],b[3],alpha=abs(a)); points(x1,y1,...); rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = c); points(x1,y1,...); 
  }
  else if (dotint==2) {
    if (a>0) {b=col2rgb(panelcolor)/255; c=rgb(b[1],b[2],b[3],alpha=a); points(x1,y1,...); rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = c); points(x1,y1,...)}
    else {b=col2rgb(panelcolor2)/255; c=rgb(b[1],b[2],b[3],alpha=-a); points(x1,y1,...); rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = c); points(x1,y1,...)}
  }
  abline(lm(y~x),lwd=lw,col="blue")
}

my_curve <- function(x, y, x1, y1, sp, nvar, lw, smoothness, digits, perc_rank, dotint, panelcolor, panelcolor2, tintmaxcorr, ...){
  data=isolate_complete_pairs(x,y); if (perc_rank) {data=perc_rank(data)}; x=data[,1]; y=data[,2]; # re-rank data for correct spearman line fits
  a=cor(x=data[,1], y=data[,2]) / tintmaxcorr; if (a>=1) a=.99 else if (a<=-1) a=-.99 
  if (dotint==0) points(x1,y1,...) 
  else if (dotint==1) {
    b=col2rgb(panelcolor)/255; c=rgb(b[1],b[2],b[3],alpha=abs(a)); points(x1,y1,...); rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = c); points(x1,y1,...); 
  }
  else if (dotint==2) {
    if (a>0) {b=col2rgb(panelcolor)/255; c=rgb(b[1],b[2],b[3],alpha=a); points(x1,y1,...); rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = c); points(x1,y1,...)}
    else {b=col2rgb(panelcolor2)/255; c=rgb(b[1],b[2],b[3],alpha=-a); points(x1,y1,...); rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = c); points(x1,y1,...)}
  }
  if(length(unique(x))>4) { 
    data=cbind(x,y)
    smoothingSpline = smooth.spline(na.omit(data), spar=smoothness, tol=.1)
    lines(smoothingSpline,lw=lw,col="red") 
    }
}

my_lineandcurve <- function(x, y, x1, y1, sp, nvar, lw, smoothness, digits, perc_rank, dotint, panelcolor, panelcolor2, tintmaxcorr, ...){
  data=isolate_complete_pairs(x,y); if (perc_rank) {data=perc_rank(data)}; x=data[,1]; y=data[,2]; # re-rank data for correct spearman line fits
  a=cor(x=data[,1], y=data[,2]) / tintmaxcorr; if (a>=1) a=.99 else if (a<=-1) a=-.99 
  if (dotint==0) points(x1,y1,...) 
  else if (dotint==1) {
    b=col2rgb(panelcolor)/255; c=rgb(b[1],b[2],b[3],alpha=abs(a)); points(x1,y1,...); rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = c); points(x1,y1,...); 
  }
  else if (dotint==2) {
    if (a>0) {b=col2rgb(panelcolor)/255; c=rgb(b[1],b[2],b[3],alpha=a); points(x1,y1,...); rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = c); points(x1,y1,...)}
    else {b=col2rgb(panelcolor2)/255; c=rgb(b[1],b[2],b[3],alpha=-a); points(x1,y1,...); rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = c); points(x1,y1,...)}
  }
  abline(lm(y~x),lwd=lw,col="blue")
  if(length(unique(x))>4) {
    data=cbind(x,y)
    smoothingSpline = smooth.spline(na.omit(data), spar=smoothness, tol=.1)
    lines(smoothingSpline,lw=lw,col="red")
  }
}