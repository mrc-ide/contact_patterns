data <- read.csv("C:/Users/cw1716/Downloads/forestplotdata.csv", stringsAsFactors=FALSE)

## Labels defining subgroups are a little indented!
subgps <- c(4,5,8,9,12,13,16,17,20,21,24,25,28,29,32,33)
data$Variable[subgps] <- paste("  ",data$Variable[subgps]) 

## Combine the count and percent column
np <- ifelse(!is.na(data$Count), paste(data$Count," (",data$Percent,")",sep=""), NA)

## The rest of the columns in the table. 
tabletext <- cbind(c("Subgroup","\n",data$Variable), 
                   c("No. of Patients (%)","\n",np), 
                   c("4-Yr","\n",data$PCI.Group), 
                   c("4-Yr","\n",data$Medical.Therapy.Group), 
                   c("P Value","\n",data$P.Value))

forestplot(labeltext=tabletext, graph.pos=3, 
           mean=c(NA,NA,data$Point.Estimate), 
           lower=c(NA,NA,data$Low), upper=c(NA,NA,data$High),
           xlab="     <---PCI Better---    ---Medical Therapy Better--->",
           hrzl_lines=list("3" = gpar(lwd=1, col="black"), 
                           "7" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "15" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "23" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "31" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922")),
           txt_gp=fpTxtGp(label=gpar(cex=1.25),
                          ticks=gpar(cex=1.1),
                          xlab=gpar(cex = 1.2),
                          title=gpar(cex = 1.2)),
           col=fpColors(box="black", lines="black", zero = "gray50"),
           zero=1, cex=0.9, lineheight = "auto", boxsize=0.5, colgap=unit(6,"mm"),
           lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.4)
