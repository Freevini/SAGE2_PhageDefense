
###-----------------------------------------------
##plotting
###-----------------------------------------------
library(ape)
library(ggtree)
library(tidyverse)
library(tidytree)
library(treeio)
library(TreeTools)

Bee_broad_tree_raw <- read.nexus("~/Desktop/Projects/2021_SAGE_II/Phylogeny/Stingless_BEE_microbiome_Rooted.tree_rerooted.nwk", tree.names = NULL, force.multi = FALSE)


gtree_01 <- ggtree(Bee_broad_tree_raw) +
  geom_tiplab(align = TRUE,size=3) + # tiplabls aligned
  geom_rootedge(rootedge = 0)  #NO ROOT  
gtree_01


###--------------------------------
#add Defense mechanisms indiviually
###--------------------------------

###--------------------------------
#add R-M
colorss="red"
###--------------------------------
table(pathway_identification_filter$mechanism_final)
pathway_identification_filter_sub  <- pathway_identification_filter %>% select(genome,mechanism_final,Presence_final) %>% rename("label"="genome") %>% filter(mechanism_final %in% c("R-M-IIG","R-M-III","R-M-II","R-M-I","R-M-IV","BREX","Disarm-I","Disarm-II")) %>% mutate(type = ifelse(Presence_final==1, "R-M", NA)) # %>% filter(Presence_final==1)

as_tibble(Bee_broad_tree_raw)$label %in% pathway_identification_filter_sub$label
pathway_identification_filter_sub$label %in% as_tibble(Bee_broad_tree_raw)$label

pathway_identification_filter_sub[which(!pathway_identification_filter_sub$label %in% as_tibble(Bee_broad_tree_raw)$label),"label"] %>% unique()


pathway_identification_filter_sub$mechanism_final  = factor(pathway_identification_filter_sub$mechanism_final , levels=c(
  "R-M-I", "R-M-II", "R-M-IIG", "R-M-III", "R-M-IV",
  "BREX","Disarm-I","Disarm-II", 
  "Druantia-I",  "Druantia-II","Druantia-III","Hachiman","Kiwa","Lamassu","Shedu","Thoeris","Zorya-I","Zorya-II","Wadjet-I","Septu",
  "CBASS","Abi",
  "GTPase","dNTPaminase","Retrons",
  
  
  
))

# p2 <- facet_plot(gtree_01, panel = 'R-M', data = pathway_identification_filter_sub, 
#                 geom = geom_tile,color = "grey",width=0.8,height=0.8,size=1.0,
#                 mapping = aes(x = mechanism_final,fill=Presence_final))+scale_x_discrete()+scale_fill_gradient2(na.value="white",low = "white", high = colorss, mid = "white",
#    midpoint = 0, limit = c(-1,1), space = "Lab",
#    name="max growth")+theme_classic()+theme(axis.text.y = element_text(size=7),
#       axis.text.x = element_text(angle = 45,size=10),axis.title=element_blank(), 
#     legend.title = element_blank(),legend.position = "none",strip.background = element_blank(),
#   strip.text.x = element_blank()) #+xlim_expand(c(0,500), 'Genome count')
# p2


p2 <- facet_plot(gtree_01, panel = 'R-M', data = pathway_identification_filter_sub, 
                 geom = geom_tile,color = "grey",width=0.8,height=0.8,size=0.2,
                 mapping = aes(x = mechanism_final,fill=type))+scale_fill_manual(values=colorss)+scale_x_discrete()+theme_classic()+theme(axis.text.y = element_text(size=7),
                                                                                                                                          axis.text.x = element_text(angle = 45,size=10,hjust=1),axis.title=element_blank(), 
                                                                                                                                          legend.title = element_blank(),legend.position = "none",strip.background = element_blank(),
                                                                                                                                          strip.text.x = element_blank()) #+xlim_expand(c(0,500), 'Genome count')
p2



###--------------------------------
#add unkown mechanism
colorss=c("blue","red")
###--------------------------------
table(pathway_identification_filter$mechanism_final)
pathway_identification_filter_sub  <- pathway_identification_filter %>% select(genome,mechanism_final,Presence_final) %>% rename("label"="genome") %>% filter(mechanism_final %in% c("Druantia-I",  "Druantia-II","Druantia-III","Hachiman","Kiwa","Lamassu","Shedu","Thoeris","Zorya-I","Zorya-II","Wadjet-I","Septu")) %>% mutate(type = ifelse(Presence_final==1, "Unknown", NA)) #%>% add_column(type="Unknown") %>% filter(Presence_final==1)

as_tibble(Bee_broad_tree_raw)$label %in% pathway_identification_filter_sub$label
pathway_identification_filter_sub$label %in% as_tibble(Bee_broad_tree_raw)$label

pathway_identification_filter_sub[which(!pathway_identification_filter_sub$label %in% as_tibble(Bee_broad_tree_raw)$label),"label"] %>% unique()


pathway_identification_filter_sub$mechanism_final  = factor(pathway_identification_filter_sub$mechanism_final , levels=c(
  "R-M-I", "R-M-II", "R-M-IIG", "R-M-III", "R-M-IV",
  "BREX","Disarm-I","Disarm-II", 
  "Druantia-I",  "Druantia-II","Druantia-III","Hachiman","Kiwa","Lamassu","Shedu","Thoeris","Zorya-I","Zorya-II","Wadjet-I","Septu",
  "CBASS","Abi",
  "GTPase","dNTPaminase","Retrons"
  
  
))

# p3 <- facet_plot(p2, panel = 'R-M', data = pathway_identification_filter_sub, 
#                 geom = geom_tile,color = "grey",width=0.8,height=0.8,size=1.0,
#                 mapping = aes(x = mechanism_final,fill=Presence_final))+scale_x_discrete()+scale_fill_gradient2(na.value="white",low = "white", high = colorss, mid = "white",
#    midpoint = 0, limit = c(-1,1), space = "Lab",
#    name="max growth")+theme_classic()+theme(axis.text.y = element_text(size=7),
#       axis.text.x = element_text(angle = 45,size=10),axis.title=element_blank(), 
#     legend.title = element_blank(),legend.position = "none",strip.background = element_blank(),
#   strip.text.x = element_blank()) #+xlim_expand(c(0,500), 'Genome count')
# p3


p3 <- facet_plot(p2, panel = 'unknown', data = pathway_identification_filter_sub, 
                 geom = geom_tile,color = "grey",width=0.8,height=0.8,size=0.2,
                 mapping = aes(x = mechanism_final,fill=type))+scale_fill_manual(values=colorss)+scale_x_discrete()+theme_classic()+theme(axis.text.y = element_text(size=7),
                                                                                                                                          axis.text.x = element_text(angle = 45,size=10,hjust=1),axis.title=element_blank(), 
                                                                                                                                          legend.title = element_blank(),legend.position = "none",strip.background = element_blank(),
                                                                                                                                          strip.text.x = element_blank()) #+xlim_expand(c(0,500), 'Genome count')
p3

###--------------------------------
#ABI
colorss=c("blue","red","green")
###--------------------------------
table(pathway_identification_filter$mechanism_final)
pathway_identification_filter_sub  <- pathway_identification_filter %>% select(genome,mechanism_final,Presence_final) %>% rename("label"="genome") %>% filter(mechanism_final %in% c("CBASS","Abi")) %>% mutate(type = ifelse(Presence_final==1, "Abi", NA)) #%>% add_column(type="Unknown") %>% filter(Presence_final==1)

as_tibble(Bee_broad_tree_raw)$label %in% pathway_identification_filter_sub$label
pathway_identification_filter_sub$label %in% as_tibble(Bee_broad_tree_raw)$label

pathway_identification_filter_sub[which(!pathway_identification_filter_sub$label %in% as_tibble(Bee_broad_tree_raw)$label),"label"] %>% unique()


pathway_identification_filter_sub$mechanism_final  = factor(pathway_identification_filter_sub$mechanism_final , levels=c(
  "R-M-I", "R-M-II", "R-M-IIG", "R-M-III", "R-M-IV",
  "BREX","Disarm-I","Disarm-II", 
  "Druantia-I",  "Druantia-II","Druantia-III","Hachiman","Kiwa","Lamassu","Shedu","Thoeris","Zorya-I","Zorya-II","Wadjet-I","Septu",
  "CBASS","Abi",
  "GTPase","dNTPaminase","Retrons"
  
  
))

p4 <- facet_plot(p3, panel = 'Abi', data = pathway_identification_filter_sub, 
                 geom = geom_tile,color = "grey",width=0.8,height=0.8,size=0.2,
                 mapping = aes(x = mechanism_final,fill=type))+scale_fill_manual(values=colorss)+scale_x_discrete()+theme_classic()+theme(axis.text.y = element_text(size=7),
                                                                                                                                          axis.text.x = element_text(angle = 45,size=10,hjust=1),axis.title=element_blank(), 
                                                                                                                                          legend.title = element_blank(),legend.position = "none",strip.background = element_blank(),
                                                                                                                                          strip.text.x = element_blank()) #+xlim_expand(c(0,500), 'Genome count')
p4

###--------------------------------
#Novel
colorss=c("blue","red","green","salmon")
###--------------------------------
table(pathway_identification_filter$mechanism_final)
pathway_identification_filter_sub  <- pathway_identification_filter %>% select(genome,mechanism_final,Presence_final) %>% rename("label"="genome") %>% filter(mechanism_final %in% c("GTPase","dNTPaminase","Retrons")) %>% mutate(type = ifelse(Presence_final==1, "novel", NA)) #%>% add_column(type="Unknown") %>% filter(Presence_final==1)

as_tibble(Bee_broad_tree_raw)$label %in% pathway_identification_filter_sub$label
pathway_identification_filter_sub$label %in% as_tibble(Bee_broad_tree_raw)$label

pathway_identification_filter_sub[which(!pathway_identification_filter_sub$label %in% as_tibble(Bee_broad_tree_raw)$label),"label"] %>% unique()


pathway_identification_filter_sub$mechanism_final  = factor(pathway_identification_filter_sub$mechanism_final , levels=c(
  "R-M-I", "R-M-II", "R-M-IIG", "R-M-III", "R-M-IV",
  "BREX","Disarm-I","Disarm-II", 
  "Druantia-I",  "Druantia-II","Druantia-III","Hachiman","Kiwa","Lamassu","Shedu","Thoeris","Zorya-I","Zorya-II","Wadjet-I","Septu",
  "CBASS","Abi",
  "GTPase","dNTPaminase","Retrons"
  
  
))

# library(ggplot2)
library(ggh4x)
p5 <- facet_plot(p4, panel = 'Novel', data = pathway_identification_filter_sub, 
                 geom = geom_tile,color = "grey",width=0.8,height=0.8,size=0.2,
                 mapping = aes(x = mechanism_final,fill=type))+scale_fill_manual(values=colorss)+scale_x_discrete()+theme_classic()+theme(axis.text.y = element_text(size=7),
                                                                                                                                          axis.text.x = element_text(angle = 45,size=10,hjust=1),axis.title=element_blank(), 
                                                                                                                                          legend.title = element_blank(),legend.position = "none",strip.background = element_blank(),
                                                                                                                                          strip.text.x = element_blank()) +force_panelsizes(cols = c(1,0.8,1.1,0.2,0.3))#+xlim_expand(c(0,500), 'Genome count')
p5

###--------------------------------
#CRISPR
colorss=c("blue","red","green","salmon","gold")
###--------------------------------
table(pathway_identification_filter$mechanism_final)
pathway_identification_filter_sub  <- pathway_identification_filter %>% select(genome,mechanism_final,Presence_final) %>% rename("label"="genome") %>% filter(mechanism_final %in% c("CRISPR I-B",     "CRISPR I-C",     "CRISPR I-E",    "CRISPR II-A",   "CRISPR III-A", "CRISPR Unknown")) %>% mutate(type = ifelse(Presence_final==1, "CRISPR", NA)) #%>% add_column(type="Unknown") %>% filter(Presence_final==1)

as_tibble(Bee_broad_tree_raw)$label %in% pathway_identification_filter_sub$label
pathway_identification_filter_sub$label %in% as_tibble(Bee_broad_tree_raw)$label

pathway_identification_filter_sub[which(!pathway_identification_filter_sub$label %in% as_tibble(Bee_broad_tree_raw)$label),"label"] %>% unique()


pathway_identification_filter_sub$mechanism_final  = factor(pathway_identification_filter_sub$mechanism_final , levels=c(
  "R-M-I", "R-M-II", "R-M-IIG", "R-M-III", "R-M-IV",
  "BREX","Disarm-I","Disarm-II", 
  "Druantia-I",  "Druantia-II","Druantia-III","Hachiman","Kiwa","Lamassu","Shedu","Thoeris","Zorya-I","Zorya-II","Wadjet-I","Septu",
  "CBASS","Abi",
  "GTPase","dNTPaminase","Retrons",
  "CRISPR I-B",     "CRISPR I-C",     "CRISPR I-E",    "CRISPR II-A",   "CRISPR III-A", "CRISPR Unknown"
  
  
))

# library(ggplot2)
library(ggh4x)
p6 <- facet_plot(p5, panel = 'CRISPR', data = pathway_identification_filter_sub, 
                 geom = geom_tile,color = "grey",width=0.8,height=0.8,size=0.2,
                 mapping = aes(x = mechanism_final,fill=type))+scale_fill_manual(values=colorss)+scale_x_discrete()+theme_classic()+theme(axis.text.y = element_text(size=7),
                                                                                                                                          axis.text.x = element_text(angle = 45,size=10,hjust=1),axis.title=element_blank(), 
                                                                                                                                          legend.title = element_blank(),legend.position = "none",strip.background = element_blank(),
                                                                                                                                          strip.text.x = element_blank()) +force_panelsizes(cols = c(2,0.8,1.1,0.2,0.3,0.6))
p6


png("~/Desktop/Projects/2021_SAGE_II/heatmap_with_phylogeny.png", width = 3500, height = 2000,res=300)


p6
dev.off()