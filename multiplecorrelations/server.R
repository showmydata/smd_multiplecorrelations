# multiple correlations
library(shiny)
#library(psychometric)
library(stringr)
source("functions/equate_zscored_axis_ranges2.R") # Need
source("functions/panel_cor2.R") 
source("functions/points_and_lines.R") 
source("functions/pairs2.R")
source("functions/perc_rank.R")                   # Redundant with 2corr
source("functions/isolate_complete_pairs.R")
source("functions/jitter_by_percent_min.R")       # Keep this! 
#library(tidyverse)

shinyServer( # Function title (don't think this is necessary)
  function(input, output) { # Create the function
# Grabs user-specified height and width
    output$ui_plot <- renderUI({plotOutput("contents", width = input$plotsize*8, height = input$plotsize*8)})
    output$contents <- renderPlot( { # Call Shiny function that makes the plot
# # Process any pasted data (removed 4/10/21, replaced with the below)
#       a=input$myData
#       if(a=="") {t=read.csv("FacesFinal4.csv",check.names=FALSE); v=colnames(t)} 
#       else {
#         t=process_pasted_data(a); v=colnames(t) 
#         if(is.na(t[1,1]) & is.na(t[1,2])) t=t[2:nrow(t),]
#       }
# Process any pasted data (new)
      if(input$myData>"") {
        t <- read.table(text = input$myData, sep = '\t', header = TRUE); 
        t=lapply(t, as.numeric); 
        t=as.data.frame(t);
        tt <- read.table(text = input$myData, sep = '\t', header = FALSE); 
        tt=lapply(tt, as.numeric); 
        tt=as.data.frame(tt);
        # The entire top row should be NA if they've entered variable names ... 
        # if so, assume header, if not, don't assume header
        if (all(is.na(tt[1,]))) t=t else t=tt
      } else {
        t=read.csv("FacesFinal4.csv",check.names=FALSE); 
      } 
      v=colnames(t); v=gsub("\\.", " ", as.character(v))
# Transform data into percentile ranks
    if (input$spearman==TRUE) t=perc_rank(t) 
# Figure out max correlation
    tintcors=cor(t, use = "pairwise.complete.obs"); 
    diag(tintcors) = NA
    maxtintcorr = max(abs(tintcors), na.rm = TRUE)
# Jitter data percent of minimum difference between points for each column
    t1=jitter_by_percent_min(t,input$jitter_perc,input$spearman)
# Get variable labels
    if(input$variablelabels=="") colnames(t)=v 
    else {
      rng=input$variablelabels
      rng=unlist(strsplit(rng,","))
      if (length(rng) < length(v)) {
        rng2=rng
        rng2[(length(rng)+1):length(v)]=v[(length(rng)+1):length(v)]
      } 
      else {
        rng2=rng[1:length(v)]
      }
      colnames(t)=rng2 
    }
# Choose axis ranges
    if(input$equateaxes==TRUE & input$axisranges>"") {                       # if user wants to equate axes and has typed something into axis range
        therange=as.numeric(unlist(strsplit(input$axisranges,',')))          # process the typing
        if(length(therange)==2 & is.numeric(therange)) {                     # if two long and both numeric
          if(therange[1]<therange[2]) erange=therange else erange=NULL       # then if 1<2, use, else don't
        } else erange=NULL # else don't use typing
    } else erange=NULL     # else don't use variable
    ### IN PROGRESS
    # ranges=equate_zscored_axis_ranges2(t, cushion=.1, equate=input$equateaxes, range=erange) # Could in very rare cases be cutting off data points
    ### WORKING ON THE ABOVE
    ranges=equate_zscored_axis_ranges2(t, cushion=.1, equate=input$equateaxes, range=erange) # Could in very rare cases be cutting off data points
    nc=ncol(t) # find number of columns
    xlim=array(0,c(nc,nc,2)); ylim=array(0,c(nc,nc,2));
    for (i in 1:ncol(t)) { # for each column of graphs
      for (j in 1:ncol(t)) { # for each graph in this column of graphs
        xlim[j,i,]=ranges[j,] # set the x range
        ylim[j,i,]=ranges[i,] # set the y range
      }
    }
# Draw the scatterplot
    if(input$tint==FALSE) {tintcolor=rgb(1,1,1); tintcolor2=rgb(1,1,1); dotint=0} else if (input$tint2==FALSE) {tintcolor=input$color1; tintcolor2=rgb(1,1,1); dotint=1} else {tintcolor=input$color1; tintcolor2=input$color2;  dotint=2}
    if(input$fit=="line") p="my_line" else if (input$fit=="curve") p="my_curve" else if (input$fit=="both") p="my_lineandcurve" else if (input$fit=="none") p="my_points"
    if(input$upper=="stats") u="panel_cor2" else if (input$upper=="data") u=p else if (input$upper=="neither") u=NULL
    if(input$lower=="stats") l="panel_cor2" else if (input$lower=="data") l=p else if (input$lower=="neither") l=NULL
    ticks=c(FALSE,FALSE); if(input$lower=="data") ticks[1]=TRUE; if(input$upper=="data") ticks[2]=TRUE; 
    adj=(input$ticklabelsize+10)/39 # a hack to adjust the bottom tick labels to match the left tick labels as they get bigger & smaller
    makemyplot <- function() {
      par(pty="s")
      pairs2(t, panel=p, cex.axis=(input$ticklabelsize+1)/25, adj=adj, 
             upper.panel=u, lower.panel=l, xlim=xlim, ylim=ylim, jdata=t1, 
             pch=as.numeric(input$dottype), cex=input$dotsize/20, sp=input$spearman, 
             col=rgb(red=0.0, green=0.0, blue=0.0, alpha=input$dotopacity/100),
             main=input$graphtitle, cex.main=3, lw=input$lw/10, smoothness=input$smoothness/100,
             digits=input$digits, perc_rank=input$spearman, ticks=ticks, 
             dotint=dotint, panelcolor=tintcolor, panelcolor2=tintcolor2, tintmaxcorr=maxtintcorr)
    }
    makemyplot()
# Save as 'filename' the 'content'
    output$down <- downloadHandler(
      filename =  function() {
        paste("myplot", input$filetype, sep=".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        if(input$filetype == "png")
          png(file, units="in", width=input$plotsize/10, height=input$plotsize/10, res=500) # make png file
        else
          pdf(file, width=input$plotsize/10, height=input$plotsize/10) # open the pdf device
        makemyplot()
        dev.off()  # turn the device off
      })
  })
  })