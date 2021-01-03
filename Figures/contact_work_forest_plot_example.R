library(forestplot)

under_18 <- read.csv("C:/Users/cw1716/Downloads/under_18.csv", stringsAsFactors = FALSE)[, -1]
over_18 <- read.csv("C:/Users/cw1716/Downloads/over_18.csv", stringsAsFactors = FALSE)[, -1]

#-----------------PLOTS TO COMPARE AND A SUMMARY PLOT OF THE TWO MEAN VALUES AND SAR-----------------------
a <- c(" ", "Study", "Age Group", "0-15yrs", "15-49yrs", "49+yrs")
a <- append(a, list(expression(paste(gamma, " = 0.072"))))
b <- c(NA, "Gender", "Male", "Female")
b <- append(b, list(expression(paste(gamma, " = 0.012"))))

tabletext<-list(
  append(a,b),
  c( " ",  "Country",NA, c("UK", "UK", "UK"), NA, NA, NA, c("UK", "UK"), NA),
  c("Total", "contacts", NA, c(1000, 2000, 4000), NA, NA, NA, c(250, 5000), NA),
  c(NA, "Rate Ratio", 
    NA, paste0(format(round(c(0.5, 0.5, 0.5), digits = 2), nsmall=2),
               " (95% CI: ", format(round(c(0.25, 0.25, 0.25), digits = 2), nsmall=2),
               "-", format(round(c(0, 0, 0), digits=2), nsmall=2), ")"), 
    NA, NA, NA,
    paste0(format(round(c(0.5, 0.5), digits = 2), nsmall=2),
           " (95% CI: ", format(round(c(0.25, 0.25), digits = 2), nsmall=2),
           "-", format(round(c(0.75, 0.75), digits=2), nsmall=2), ")"), NA))

forest_data_to_plot <- 
  structure(list(
    mean  = c(NA, NA, NA, c(0.5, 0.5, 0.5),  NA, NA, NA, c(0.5, 0.5), NA), 
    lower = c(NA, NA, NA, c(0.25, 0.25, 0.25), NA, NA, NA, c(0.25, 0.25), NA),
    upper = c(NA, NA, NA, c(0.75, 0.75, 0.75), NA, NA, NA, c(0.75, 0.75), NA)),
    .Names = c("mean", "lower", "upper"), 
    row.names = c(NA, -12), # note 12 has to match up to length
    class = "data.frame")

forestplot(tabletext, 
           graph.pos = 3,
           forest_data_to_plot,
           hrzl_lines = list("3" = gpar(lwd=1)),
           txt_gp = fpTxtGp(label = gpar(cex=0.8),
                            ticks = gpar(cex=0.7),
                            xlab  = gpar(cex=0.8)),
           new_page = TRUE,
           clip=c(0,1), 
           align = c("l","c", "c", "c", "c", "c","r"),
           is.summary=c(T,T,T, rep(FALSE, 6), T,F,F, T, rep(FALSE, 9), T,F),
           col=fpColors(box="#003f5c",line="#003f5c", summary="#003f5c", hrz_lines = "#444444"), 
           colgap = unit(0,"mm"), 
           cex=0.4,
           lineheight = unit(0.5, "cm"),
           xlab=c("Secondary Attack Rate"),
           graphwidth = unit(100, "mm"),
           xticks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5),
           ci.vertices=TRUE#,
           # fn.ci_norm = function(size, ...) {
           #   fpDrawNormalCI(size = size * 0.5, ...)
           # }
)
