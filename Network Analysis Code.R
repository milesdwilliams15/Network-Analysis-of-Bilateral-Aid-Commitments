
# -----------------------------------------------------
# Network Analysis of Bilateral Foreign Aid Commitments
# between 18 OECD Members
# -----------------------------------------------------

# Get the data
path <- file.path("C:","Users","Miles","Documents","R",
                  "Network Analysis of Foreign Aid","aid.csv")
aid <- read.csv(path)

# Get correlations
library(Hmisc)
res<-rcorr(as.matrix(aid))
install.packages("corrplot")
library(corrplot)
windows()
par(family="serif")
corrplot(res$r,type="upper",order="hclust",tl.col="black",tl.pos="d",
         tl.cex=.75,p.mat=res$P,sig.level=0.05,
         title="Correlations between OECD Member Bilateral\nAid Commitments",
         mar=c(2,2,3,2))
mtext("ThePoliticalScientist1.blogspot.com",side=1,adj=0,line=3,cex=.75)

# Put correlations in pairwise format
res.r <- res$r
res.r[lower.tri(res.r,diag=TRUE)]=NA # put NA
res.r<-as.data.frame(as.table(res.r)) # as a dataframe
res.r<-na.omit(res.r) # remove NA
res.r<-res.r[with(res.r, order(-Freq)), ] # order by correlation
res.r$Correlation <- res.r$Freq
# Plot the network of correlations
library(ggplot2)
library(igraph)
library(ggraph)
library(dplyr)
library(grid)
library(gridExtra)
set.seed(1234)
windows()
g <- res.r %>% filter(Freq >= .1914) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") + 
  geom_edge_link(aes(edge_alpha = Correlation, edge_width = Correlation), edge_colour = "darkorange") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void() + theme(text=element_text(family="serif")) +
  ggtitle("\nNetwork of Pairwise Correlations between\nOECD Members' Bilateral Aid Commitments\nto Non-Member States") +
  theme(plot.title=element_text(face="bold",size=14,hjust=.5)) +
  theme(legend.position = "bottom")
g <- arrangeGrob(g, bottom = textGrob("ThePoliticalScientist1.blogspot.com", x = .1, hjust = 0, vjust=0.1, gp = gpar(fontsize = 10,fontfamily="serif",
                                                                                                                     face="bold")))
grid.draw(g)

  