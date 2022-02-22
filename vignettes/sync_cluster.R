## ---- echo = FALSE, message = FALSE-------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#",
                      fig.width = 7, 
                      fig.height = 6)
# devtools::load_all(".") #remove this line

## ----echo=TRUE, warning=FALSE-------------------------------------------------
library(funtimes)
library(ggplot2)
library(gridExtra)
library(readxl)
library(reshape2)

## -----------------------------------------------------------------------------
data_types <- rep("numeric", 20)
d_org <- readxl::read_xlsx("Aus Sea Levels 17.xlsx", skip = 1, n_max = 7300, col_types = data_types)
# yearly average
d = data.frame(aggregate(d_org[, 4:20], list(d_org$Year), FUN = 'mean', na.rm = TRUE)[, -1], row.names = unique(d_org$Year))

## -----------------------------------------------------------------------------
ggplot(reshape2::melt(t(d))) + geom_line(aes(x = Var2, y = value, color = Var1), size = 1) +
  ylab('Sea level (m)') + xlab('Year') +
  theme_bw() +
  theme(axis.text = element_text(size = 13), axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), legend.text = element_text(size = 10), 
        legend.title = element_blank(), legend.key.size = unit(0.3, "cm"))

## -----------------------------------------------------------------------------
Clus_sync <- sync_cluster(d ~ t, Window = 3)
Clus_sync

## -----------------------------------------------------------------------------
for(i in 0:max(Clus_sync$cluster)) {
  assign(paste('py', i, sep = ''),
         ggplot(melt(t(d[, Clus_sync$cluster == i]))) +
           geom_line(aes(x = Var2,y = value,color = Var1),size = 1) +
           ylab('Sea level (m)') + xlab('Year') +
           theme_bw() + ggtitle(paste('Cluster',i)) +
           theme(axis.text = element_text(size = 13), axis.title.x = element_text(size = 15),
                 axis.title.y = element_text(size = 15), legend.text = element_text(size = 10),
                 legend.title = element_blank(), legend.key.size = unit(0.3, "cm")))
}
grid.arrange(py0, py1)

## -----------------------------------------------------------------------------
Clus_BICC <- BICC(as.matrix(d), p = 5, w = 4, s = 4)
Clus_BICC

## -----------------------------------------------------------------------------
citation("funtimes")

