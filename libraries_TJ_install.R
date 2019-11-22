
install.packages(c("stringr","reshape2","Hmisc","MASS","plyr","RCurl"))
install.packages(c("caret","glmnet","KernSmooth","nnet","e1071"))
install.packages(c("doParallel"))
install.packages(c("ggplot2","grid","gridExtra","ggthemes"))
install.packages(c("GillespieSSA","deSolve"))
install.packages(c("rgl","plot3D","plot3Drgl"))
install.packages(c("data.table"))
install.packages(c("mvtnorm"))
install.packages(c("caTools"))
install.packages(c("corrplot"))
install.packages(c("openxlsx"))
install.packages("pryr")
install.packages("devtools")
install.packages("profvis")
install.packages("shiny")
install.packages("fpc","clue")
install.packages("GGally")
install.packages("DescTools")
install.packages("psych")
install.packages("GPArotation")
install.packages(c("rmarkdown","htmltools"))

install.packages("igraph")

install.packages(c("GPfit","mlegp"))

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")
install.packages(c("cluster",  "NbClust"))
install.packages(c("extrafont", "ggcorrplot", "jtools"))


## BioConductor ##
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("graphite", version = "3.9")
BiocManager::install(c("edge","qvalue","limma"), version = "3.9")
BiocManager::install(c("fgsea","DESeq2"), version = "3.9")

BiocManager::install(c("GEOquery","annotate","affy","AnnotationDbi","org.Hs.eg.db"), version = "3.9")
BiocManager::install(c("KEGG.db","reactome.db"), version = "3.9")
BiocManager::install(c("biomaRt"), version = "3.9")
BiocManager::install(c("fgsea"), version = "3.9")
