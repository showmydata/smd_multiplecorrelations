panel_cor2 <- function(x, y, x1, y1, nvar, lw, smoothness, digits = 2, perc_rank, showp, poly, cats, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr)); par(usr = c(0, 1, 0, 1)) # Not sure what this was supposed to accomplish
  data=isolate_complete_pairs(x,y); # isolate complete pairs
  if (perc_rank) {data=perc_rank(data)} # re-rank data with just these pairs for correct spearman correlations
  # compute correlation coefficient & n
  if (poly & sum(complete.cases(unique(data[,1])))<=cats & sum(complete.cases(unique(data[,2]))) ) {a=polychoric(data); r=round(a$rho[2,1],digits=digits)}
  else r=round(cor(data[,1],data[,2]),digits=digits)
  n=length(data[,1])
  #cis_r=CIr(r, n = n, level = .95)
  ct = cor.test(data[,1],data[,2]); 
  cis_r=round(ct$conf.int,3)
  pv=signif(ct$p.value,3)
  lowerci_r=round(cis_r[1],digits=digits)
  upperci_r=round(cis_r[2],digits=digits)
  # generate text
  num1 <- format(c(r, 0.123456789), digits = digits, scientific = FALSE)[1] # r or rho text
  num2 <- n#format(c(n, 0.123456789), trim=TRUE, digits = NULL, scientific = FALSE)[1] # n text
  num3 <- format(c(lowerci_r, 0.123456789), digits = digits)[1] # upper 95% CI text
  num4 <- format(c(upperci_r, 0.123456789), digits = digits)[1] # lower 95% CI text
  if (perc_rank) txt5 <- paste("rho = ", num1, sep = "") else txt5 <- paste("r = ", num1, sep = "") # r or rho
  if (showp) txt6 <- paste("p = ", pv, sep = "") else txt6 <- paste("n = ", num2, sep = "") # p or n
  
  txt7 <- paste(" 95% CI =", sep= "") # text introducing the 95% CI
  txt8 <- paste("[", num3, ", ", num4, "]", sep = "") # 95% CI itself
  # write text
  text(0.5, 0.75, txt5, cex=1.25/(nvar/9), font=2)
  text(0.5, 0.54, txt7, cex=.9/(nvar/9))
  text(0.5, 0.42, txt8, cex=.9/(nvar/9))
  text(0.5, 0.25, txt6, cex=1/(nvar/9))
}