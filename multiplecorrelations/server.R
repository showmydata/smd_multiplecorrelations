# multiple correlations
library(shiny)
#library(psychometric)
library(stringr)
library(psych)
library(readr)
source("functions/equate_zscored_axis_ranges2.R") # Need
source("functions/panel_cor2.R") 
source("functions/points_and_lines.R") 
source("functions/pairs2.R")
source("functions/perc_rank.R")                   # Redundant with 2corr
source("functions/isolate_complete_pairs.R")
source("functions/jitter_by_percent_min.R")       # Keep this! 

source("functions/make_url.R") 
source("functions/parse_url.R") 
source("functions/add_data_link_to_url.R")
source("functions/get_data_from_url.R")
library("gsheet")

shinyServer( # Initiate the shiny server
  function(input, output, session) { # Create the function -- added 'session' for URL project 3/22/24
    
# Re-render UI with user-specified height and width
  output$ui_plot <- renderUI({plotOutput("contents", width = input$plotsize*8, height = input$plotsize*8)})
  
# Run function that makes the plot
  output$contents <- renderPlot( { # Call Shiny function that makes the plot

# Process any pasted data (new)
      if(input$myData>"") {
        # Next 3 lines added 8/15/23
        v=unlist(strsplit(input$myData,"\n")); v=unlist(strsplit(v[1],"\t")); # Read 'header' exactly, regardless of characters
        if(!all(is.na(as.numeric(v)))) for (i in 1:length(v)) v[i]=paste("column ",i); # If 'header' has any numbers (is not all words), replace with "column i"
        d0=gsub(",","",input$myData); d0=gsub("'","",d0); d0=gsub("‘","",d0); d0=gsub("’","",d0); d0=gsub('"',"",d0); d0=gsub("“","",d0); d0=gsub("”","",d0) # Replace various characters that produce errors
        for (i in 1:length(v)) { vv=v[i]; # For each variable label
        if (nchar(vv)>20) { # If the variable label is >20 length, add a carriage return at the last space before the 20th character
          b=unlist(gregexpr(' ', vv)); c=max(b[b<20]); vv=paste(substr(vv,1,c-1), "\n", substr(vv,c+1,nchar(vv)), sep=""); v[i]=vv
        }}
        
        t <- read.table(text = d0, sep = '\t', header = TRUE); 
        t=lapply(t, as.numeric); 
        t=as.data.frame(t);
        tt <- read.table(text = d0, sep = '\t', header = FALSE); 
        tt=lapply(tt, as.numeric); 
        tt=as.data.frame(tt);
        # The entire top row should be NA if they've entered variable names ... 
        # if so, assume header, if not, don't assume header
        if (all(is.na(tt[1,]))) t=t else t=tt
      } else {
        t=read.csv("FacesFinal4.csv",check.names=FALSE); # Formerly got column names produced by read.table, now replaced by new lines above
        t=as.data.frame(get_data_from_url(t,session,input$datalink))
        v=colnames(t);
        t=lapply(t, as.numeric); 
        t=as.data.frame(t);
      } 
    # 3/27/24 -- copy-pasted from one correlation -- hoping it will deal with periods and other characters in google sheet
    v=gsub(".", " ", v, fixed=TRUE); v=gsub(",","",v); v=gsub("'","",v); v=gsub("‘","",v); v=gsub("’","",v); v=gsub('"',"",v); v=gsub("“","",v); v=gsub("”","",v) # Replace various characters that produce errors
    # for (i in 1:length(v)) { vv=v[i]; # For each variable label
    # if (nchar(vv)>20) { # If the variable label is >20 length, add a carriage return at the last space before the 20th character
    #   b=unlist(gregexpr(' ', vv)); c=max(b[b<20]); vv=paste(substr(vv,1,c-1), "\n", substr(vv,c+1,nchar(vv)), sep=""); v[i]=vv
    # }}
    
# Transform data into percentile ranks
    if (input$spearman==TRUE) t=perc_rank(t) 
# Figure out max correlation
    if (input$setmaxtintcorr) maxtintcorr=input$maxtintcorr
    else {
      tintcors=cor(t, use = "pairwise.complete.obs"); 
      diag(tintcors) = NA
      maxtintcorr = max(abs(tintcors), na.rm = TRUE)
    }
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
    cushion=input$cushion/100
    if(input$equateaxes==TRUE & input$axisranges>"") {                       # if user wants to equate axes and has typed something into axis range
        therange=as.numeric(unlist(strsplit(input$axisranges,',')))          # process the typing
        if(length(therange)==2 & is.numeric(therange)) {                     # if two long and both numeric
          if(therange[1]<therange[2]) erange=therange else erange=NULL       # then if 1<2, use, else don't
        } else erange=NULL # else don't use typing
    } else erange=NULL     # else don't use variable
    ### IN PROGRESS
    # ranges=equate_zscored_axis_ranges2(t, cushion=.1, equate=input$equateaxes, range=erange) # Could in very rare cases be cutting off data points
    ### WORKING ON THE ABOVE
    ranges=equate_zscored_axis_ranges2(t, cushion=cushion, equate=input$equateaxes, range=erange) # Could in very rare cases be cutting off data points
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
             pch=as.numeric(input$dottype), cex=input$dotsize/20,
             col=rgb(red=0.0, green=0.0, blue=0.0, alpha=input$dotopacity/100),
             main=input$graphtitle, cex.main=3, lw=input$lw/10, smoothness=input$smoothness/100,
             digits=input$digits, perc_rank=input$spearman, ticks=ticks, 
             dotint=dotint, panelcolor=tintcolor, panelcolor2=tintcolor2, tintmaxcorr=maxtintcorr,
             showp=input$showp, poly=input$poly, cats=input$cats)
    }
    makemyplot()
    
    settings=reactiveValuesToList(input);
    theurl=make_url(settings, get_all=FALSE, 
                    datalink=input$datalink, 
                    appurl="https://showmydata.shinyapps.io/multiplecorrelations"); 
    #theurl=gsub("\\n","\n",theurl,fixed=TRUE); theurl=gsub("\n","newline",theurl,fixed=TRUE); #NEW
    output$clip <- renderUI({ rclipButton(inputId = "clipbtn", icon = icon("clipboard"), 
                                          label = "Copy link with current settings", 
                                          clipText = theurl)}) 
    
    
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
  
  # Get link, Make link, Add URL
  observe({ urlstring=session$clientData$url_search; if (urlstring!="") session <- parse_url(urlstring, session) }) # updates session
  
  })