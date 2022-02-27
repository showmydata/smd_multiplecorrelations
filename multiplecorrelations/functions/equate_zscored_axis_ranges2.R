equate_zscored_axis_ranges2 <- function(data, cushion=.1, equate=FALSE, range=NULL, ...){
  # Selects axis ranges that all span the same z range 
  mins=apply(data,2,min,na.rm=TRUE); # determine min for each variable
  maxs=apply(data,2,max,na.rm=TRUE); # determine max for each variable
  means=apply(data,2,mean,na.rm=TRUE); # determine mean for each variable
  sds=apply(data,2,sd,na.rm=TRUE); # determine sd for each variable
  if (equate==FALSE) { # if user has not specified to equate ranges, do the normal routine
    zranges=(maxs-mins)/sds # determine the range for each variable in sd unuts
    maxzrange=max(zranges) # determine the max range in sd units
    midranges=apply(cbind(mins,maxs),1,mean) # find the midrange for each variable
    ranges=cbind(midranges-(maxzrange/2+cushion)*sds,midranges+(maxzrange/2+cushion)*sds) # specify range of each variable counting out in both directions from its midrange
  } else { # else, if they HAVE specified to equate ranges
    if (is.numeric(range)) { # if they've specified something use it! 
      mins[]=range[1] # put user-specified min into all variables
      maxs[]=range[2] # put user-specified max into all variables
      ranges=cbind(mins,maxs)
    } else { # if they haven't specified something, do something reasonable! 
      mins[]=min(mins) # put overall min into all variables
      maxs[]=max(maxs) # put overall max into all variables
      unexpandedranges=maxs-mins
      ranges=cbind(mins-unexpandedranges*cushion,maxs+unexpandedranges*cushion)
    }
  }
  return(ranges)
}