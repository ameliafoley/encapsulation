library(tidyverse)
library(ggtree)
library(here)

tree<- read.tree(here::here("data","phylotree.1.nwk"))
tree
ggtree(tree)              
