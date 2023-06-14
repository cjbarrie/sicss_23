
### load packages, define functions, set working directory ######

library(tidyverse)
library(igraph)
#library(ForceAtlas2)
library(RColorBrewer)
library(lubridate)
library(zoo)
library(classInt) # classIntervals function - used in assigning colors

# function to extract main component from igraph object
main.component <- function(graph, ...) {
  cl <- clusters(graph, ...)
  induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
}

file.stem <- paste0("C:/Users/",
                    Sys.info()["user"],
                    "/OneDrive - University of Edinburgh/")

setwd(paste0(file.stem,"research/twitter"))

### download and restructure MP tweet data ##########

tweets <- readRDS(gzcon(url("https://github.com/cjbarrie/CTA-ED/blob/main/data/wordembed/twts_corpus_sample.rds?raw=true")))

retweets <- tweets %>%
  rename(retweet_username = reftweet_username) %>%
  mutate(year.month = format(date, "%Y-%m"),
         year.quarter = ordered(paste(year(date), quarter(date), sep = "-")),
         username = tolower(username),
         retweet_username = tolower(retweet_username)) %>%
  drop_na(retweet_username) %>%
  group_by(retweet_username, username,year.month) %>%
  summarise(n_retweets = n(), date = date) %>%
  relocate(year.month,retweet_username, username,n_retweets) %>%
  arrange(year.month)

mentions <- tweets %>%
  filter(nchar(ments)>0)

mentions.temp <- strsplit(mentions$ments, ",", fixed = T)

mentions.long <- data.frame(
  date = as.Date(unlist(mapply(rep,
                       as.list(mentions$date),
                       lapply(mentions.temp, length))),
                 format = "%Y-%m-%d",
                 origin = "1970-01-01"),
  username = unlist(mapply(rep,
                           as.list(mentions$username),
                           lapply(mentions.temp, length))),
  mentioned.account = trimws(unlist(mentions.temp)),
  stringsAsFactors = F
  ) %>%
  mutate(mentioned.account = gsub("@","",
                                mentioned.account,
                                fixed = T),
         year.month = format(date, "%Y-%m")) %>%
  filter(nchar(mentioned.account)>0) %>%
  group_by(username,mentioned.account, year.month) %>%
  summarize(n_mentions = n(), date = date)

### create dataset with party id of MPs ##########################
# this is pretty cludgy...

temp <- split(tweets[,c("username","party_value")],
                  tweets$username)
temp <- lapply(temp, function(x) lapply(x, unique))

# test for any party switching
# all "party_value" vectors should have length 1
table(unlist(lapply(temp, 
                    function(x) length(
                      unique(
                        x$party_value
                        )
                      )
                    )))

party.id <- data.frame(
  username = tolower(unlist(lapply(temp,'[[',"username"))),
  party = unlist(lapply(temp,'[[',"party_value"))
)

rm(temp)

# color-code parties using standard party colors 
# source for party colours:
# https://en.wikipedia.org/wiki/Wikipedia:Index_of_United_Kingdom_political_parties_meta_attributes

party.id <- party.id %>%
  mutate(party.color = recode(party,
                              "Conservative" = "#0087DC",
                              "Democratic Unionist Party" = "#D46A4C",
                              "Green Party" = "#528D6B",
                              "Labour" = "#E4003B",
                              "Labour (Co-op)" = "#E4003B",
                              "Liberal Democrat" = "#FDBB30",
                              "Plaid Cymru" = "#005B54",
                              "Scottish National Party" = "#FFFF00")
                              )


### one period analysis: March - June 2019 #############
# note: this is the period leading up to and immediately after Teresa May's resignation as prime minister

rt.samp <- retweets %>%
  filter(date >= as.Date("2019-03-01") & date <= as.Date("2019-06-30"))%>%
  group_by(retweet_username, username) %>%
    summarize(n_retweets = sum(n_retweets))

ig.samp <- graph_from_edgelist(
  as.matrix(rt.samp[,c("retweet_username","username")]),
  directed = T
)
ig.samp$weight <- rt.samp$n_retweets

pdf("results_MP_2023/samp1.pdf", width = 12)
plot(simplify(ig.samp), 
     vertex.label = NA, 
     vertex.size = 4,
     vertex.color = NA,
     edge.arrow.size = 0)
dev.off()

samp.attr <- data.frame(
  username = V(ig.samp)$name,
  node.seq = 1:length(V(ig.samp)$name),
  degree.in = degree(ig.samp, mode = "in"),
  degree.out = degree(ig.samp, mode = "out"),
  between.dir = betweenness(ig.samp, directed = T,normalized = T),
  between.undir = betweenness(ig.samp, directed = F, normalized = T)
  ) %>%
  left_join(party.id,
            by = "username")


# visualize the network with party labels - save a pdf for high resolution
pdf("results_MP_2023/samp1-partylab.pdf", width = 12)
plot(simplify(ig.samp), 
     vertex.label = NA, 
     vertex.size = 4,
     vertex.color = samp.attr$party.color,
     edge.arrow.size = 0)
dev.off()

### explore centrality scores ############################
# who are the most central nodes?
# what is the shape of the degree distribution?
# how does in degree vary by party (e.g. are members of any party more central?)
# how could we add other centrality measures, e.g. betweenness?
# how can we create a summary dataset using dplyr?

table(samp.attr$degree.in)

ggplot(samp.attr, aes(x = degree.in))+
  geom_density()+
  scale_x_continuous(trans = 'log1p')


table(samp.attr$degree.out)
ggplot(samp.attr, aes(x = degree.out))+
  geom_density()+
  scale_x_continuous(trans = 'log1p')



ggplot(samp.attr, aes(y = party, x = degree.out))+
  geom_boxplot()

ggplot(samp.attr, aes(y = party, x = between.dir))+
  geom_boxplot()

ggplot(samp.attr, aes(y = party, x = between.undir))+
  geom_boxplot()

pdf("results_MP_2023/samp1-degree.pdf")
plot(simplify(ig.samp), 
     vertex.label = NA, 
     vertex.size = sqrt(1 + samp.attr$degree.out),
     vertex.color = samp.attr$party.color,
     edge.arrow.size = 0)
dev.off()


plot(simplify(ig.samp), 
     vertex.label = NA, 
     vertex.size = sqrt(2 + samp.attr$between.undir*1000),
     vertex.color = samp.attr$party.color,
     edge.arrow.size = 0)


plot(samp.attr$degree.out, samp.attr$between.dir)
plot(samp.attr$degree.out, samp.attr$between.undir)


### detect subgroups/communities #########################
# we'll try four common community detection methods
# most community detection methods are designed for undirected networks
# we also simplify to remove self-ties (self-retweets)
# which method seems to perform best?

# Clauset, Aaron, M. E. J. Newman, and Cristopher Moore. 2004. “Finding Community Structure in Very Large Networks.” Physical Review E 70(6):066111. doi: 10.1103/PhysRevE.70.066111.
samp.greedy <- cluster_fast_greedy(as.undirected(simplify(ig.samp)))

# Blondel, Vincent D., Jean-Loup Guillaume, Renaud Lambiotte, and Etienne Lefebvre. 2008. “Fast Unfolding of Communities in Large Networks.” Journal of Statistical Mechanics: Theory and Experiment 2008(10):P10008. doi: 10.1088/1742-5468/2008/10/P10008.
samp.louvain <- cluster_louvain(as.undirected(simplify(ig.samp)))

# Traag, V. A., L. Waltman, and N. J. van Eck. 2019. “From Louvain to Leiden: Guaranteeing Well-Connected Communities.” Scientific Reports 9(1):5233. doi: 10.1038/s41598-019-41695-z.
samp.leiden <- cluster_leiden(as.undirected(simplify(ig.samp)),
                              objective_function = "modularity",
                              initial_membership = ifelse(!is.na(samp.attr$party),
                                               as.numeric(as.factor(samp.attr$party)),
                                               99),
                              resolution_parameter = .5)

# Raghavan, Usha Nandini, Réka Albert, and Soundar Kumara. 2007. “Near Linear Time Algorithm to Detect Community Structures in Large-Scale Networks.” Physical Review E 76(3):036106. doi: 10.1103/PhysRevE.76.036106.
samp.labelprop <- cluster_label_prop(as.undirected(simplify(ig.samp)),
                                     # set initial values to party, or -1 if unobserverved
                                     initial = ifelse(!is.na(samp.attr$party),
                                                      as.numeric(as.factor(samp.attr$party)),
                                                      -1),
                                     # fix labels for nodes with observed party
                                     fixed = !is.na(samp.attr$party))

samp.attr$member.greedy <- samp.greedy$membership
samp.attr$member.louvain <- samp.louvain$membership
samp.attr$member.leiden <- samp.louvain$membership
samp.attr$member.labelprop <- samp.labelprop$membership

table(samp.greedy$membership)
table(samp.louvain$membership)
table(samp.leiden$membership)
table(samp.labelprop$membership)

table(samp.attr$member.leiden, samp.attr$party, useNA = "ifany")
table(samp.attr$party, samp.attr$member.labelprop, useNA = "ifany")


modularity(samp.greedy)
modularity(samp.louvain)
modularity(ig.samp, samp.leiden$membership)
modularity(samp.labelprop)




pdf("results_MP_2023/samp1-greedylab.pdf", width = 12)
plot(simplify(ig.samp), 
     #layout = l,
     vertex.label = NA, 
     vertex.size = 4,
     vertex.color = samp.greedy$membership,
     edge.arrow.size = 0)
dev.off()

pdf("results_MP_2023/samp1-louvainlab.pdf", width = 12)
plot(simplify(ig.samp), 
     #layout = l,
     vertex.label = NA, 
     vertex.size = 4,
     vertex.color = samp.louvain$membership,
     edge.arrow.size = 0)
dev.off()

pdf("results_MP_2023/samp1-leidenlab.pdf", width = 12)
plot(simplify(ig.samp), 
     #layout = l,
     vertex.label = NA, 
     vertex.size = 4,
     vertex.color = samp.leiden$membership,
     edge.arrow.size = 0)
dev.off()

pdf("results_MP_2023/samp1-labelprop.pdf", width = 12)
plot(simplify(ig.samp), 
     #layout = l,
     vertex.label = NA, 
     vertex.size = 4,
     vertex.color = samp.labelprop$membership,
     edge.arrow.size = 0)
dev.off()



# within each subgroup, what % of nodes belong to each party?
# (among accounts that belong to MPs?)

Mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

samp.attr <- samp.attr %>%
  group_by(member.leiden) %>%
  mutate(n.labour = sum(party=="Labour" | party=="Labour (Co-op)", na.rm = T),
          leiden.pcnt.labour = 
           sum(party=="Labour" | party=="Labour (Co-op)", na.rm = T)/sum(!is.na(party)),
         leiden.pcnt.cons = 
           sum(party=="Conservative", na.rm = T)/sum(!is.na(party)),
         leiden.lean.cons = leiden.pcnt.cons- leiden.pcnt.labour) %>%
  # assign a party to each community defined by label propagation
  # based on the modal observed party in each group
  group_by(member.labelprop) %>%
  mutate(labelprop.party.mode = Mode(party),
         labelprop.party.color = recode(labelprop.party.mode,
                              "Conservative" = "#0087DC",
                              "Democratic Unionist Party" = "#D46A4C",
                              "Green Party" = "#528D6B",
                              "Labour" = "#E4003B",
                              "Labour (Co-op)" = "#E4003B",
                              "Liberal Democrat" = "#FDBB30",
                              "Plaid Cymru" = "#005B54",
                              "Scottish National Party" = "#FFFF00")
  )
  

# assign colors to subgroups

nclr <- 10
min <- -1 # theoretical minimum
max <- 1 # theoretical maximum
breaks <- (max - min) / nclr

plotclr <- brewer.pal(nclr, "RdBu")
plotvar <- samp.attr$leiden.lean.cons
class <- classIntervals(plotvar,
                        nclr,
                        style = "fixed",
                        fixedBreaks = seq(min, max, breaks))
colcode <- findColours(class, 
                       plotclr)

pdf("results_MP_2023/samp1-leiden-leancons.pdf", width = 12)
plot(simplify(ig.samp), 
     vertex.label = NA, 
     vertex.size = 4,
     vertex.color = colcode,
     edge.arrow.size = 0)
dev.off()

pdf("results_MP_2023/samp1-labelprop-party.pdf", width = 12)
plot(simplify(ig.samp), 
     vertex.label = NA, 
     vertex.size = 4,
     vertex.color = samp.attr$labelprop.party.color,
     edge.arrow.size = 0)
dev.off()



### analysis of structure of mostly conservative and labour clusters (bonus) ##########

# identify nodes belonging to mostly conservative/labour clusters
labour.nodes <- unique(samp.attr$node.seq[samp.attr$greedy.pcnt.labour >= .5])
cons.nodes <- unique(samp.attr$node.seq[samp.attr$greedy.pcnt.cons >= .5])

# extract subnetworks of these nodes
ig.samp.labour <- induced.subgraph(ig.samp, labour.nodes)
ig.samp.cons <- induced.subgraph(ig.samp, cons.nodes)

plot(simplify(ig.samp.labour),
     vertex.size = 3,
     vertex.label = NA,
     edge.arrow.size = 0)

plot(simplify(ig.samp.cons),
     vertex.size = 3,
     vertex.label = NA,
     edge.arrow.size = 0)

edge_density(ig.samp.labour)
edge_density(ig.samp.cons)

### mention networks: one period ###########################

mentions.samp <- mentions.long %>%
  filter(date >= as.Date("2019-03-01") & date <= as.Date("2019-06-30"))%>%
  group_by(username, mentioned.account) %>%
  summarize(n_mentions = sum(n_mentions))

mentions.ig <- graph_from_edgelist(
            as.matrix(
                mentions.samp[,c("username","mentioned.account")]
                )
            )

plot(simplify(mentions.ig), 
     vertex.label = NA, 
     vertex.size = 2,
     edge.arrow.size = 0)


mentions.samp.attr <- data.frame(
  username = V(mentions.ig)$name,
  node.seq = 1:length(V(mentions.ig)$name),
  degree.in = degree(mentions.ig, mode = "in")
  ) %>%
  left_join(party.id,
            by = "username")

pdf("results_MP_2023/mentions-samp-partylab.pdf", width = 12)
plot(simplify(mentions.ig), 
     vertex.label = NA, 
     vertex.size = 4,
     vertex.color = mentions.samp.attr$party.color,
     edge.arrow.size = 0)
dev.off()

# now try replicating analyses above with mentions


### ANALYSIS BY MONTH  #######

# step 1: create monthly edgelist (must be in matrix format with 2 columns)
el.rt.monthly <- lapply(split(retweets, retweets$year.month), 
                      function(x){
  as.matrix(x[,c("retweet_username","username")])
})

# step 2: edgelist to igraph object 
ig.rt.monthly <- lapply(el.rt.monthly,
                        graph_from_edgelist, 
                        directed = T)


# gather node-level information
# in this case we will just use a loop for convenience
# note that we pre-assign the list nodes.monthly
# as before it is important to keep track of the NODE ORDER


nodes.monthly <- list()
for(i in 1:length(ig.rt.monthly)){
  nodes.monthly[[i]] <-  data.frame(username = V(ig.rt.monthly[[i]])$name)
  nodes.monthly[[i]]$node.seq <- 1:nrow(nodes.monthly[[i]])
  nodes.monthly[[i]] <- merge(nodes.monthly[[i]],
                              party.id,
                              by = "username",
                              all.x = T)
  nodes.monthly[[i]]$party.color <- ifelse(is.na(nodes.monthly[[i]]$party.color),
                                           NA,
                                           nodes.monthly[[i]]$party.color)
  nodes.monthly[[i]] <- nodes.monthly[[i]][order(nodes.monthly[[i]]$node.seq),]
  V(ig.rt.monthly[[i]])$vertex.color <- nodes.monthly[[i]]$party.color
  
}

### visualize all the networks ########

dates <- unique(retweets$year.month)

for(i in 1:length(ig.rt.monthly)){
pdf(paste0("results_MP_2023/rt-monthly-", dates[i],".pdf"), width = 12)
  plot(simplify(ig.rt.monthly[[i]]), 
      vertex.label = NA, 
      vertex.size = 4,
      vertex.color = V(ig.rt.monthly[[i]])$vertex.color,
      edge.arrow.size = 0,
      main = dates[i])
  dev.off()
}  

### modularity analysis in monthly data ########

labelprop.fun <- function(graph, ...){
  cluster_label_prop(simplify(as.undirected(graph)))
}

### label propagation monthly

initial.list <- lapply(nodes.monthly, function(x){
  ifelse(!is.na(x$party),
         as.numeric(as.factor(x$party)),
         -1)
})

monthly.labelprop <- mapply(labelprop.fun,
                            graph = ig.rt.monthly,
                            initial = initial.list,
                            fixed = lapply(initial.list,
                                           function(x) !is.na(x)))


data.frame(
  date = as.Date(as.yearmon(names(lapply(monthly.labelprop, modularity)))),
  modularity = unlist(lapply(monthly.labelprop, modularity))
) %>%
  ggplot(., aes(y = modularity, x = date))+
    geom_line()



for(i in 1:length(ig.rt.monthly)){
  pdf(paste0("results_MP_2023/rt-monthly-labelprop-", dates[i],".pdf"), width = 12)
  plot(simplify(ig.rt.monthly[[i]]), 
       vertex.label = NA, 
       vertex.size = 5,
       vertex.color = monthly.labelprop[[i]]$membership,
       edge.arrow.size = 0,
       main = dates[i])
  dev.off()
} 

### Leiden algorithm monthly

leiden.fun <- function(graph,...){
  cluster_leiden(as.undirected(simplify(graph)),
                 objective_function = "modularity")
}

initial.list <- lapply(nodes.monthly, function(x){
  ifelse(!is.na(x$party),
         as.numeric(as.factor(x$party)),
         99)
})

monthly.leiden <- mapply(leiden.fun,
                         graph = ig.rt.monthly,
                         initial_membership = initial.list)


modul.monthly.leiden <- mapply(modularity,
                               x = ig.rt.monthly,
                               membership = lapply(monthly.leiden,
                                                   membership))

data.frame(
  date = as.Date(as.yearmon(names(lapply(monthly.labelprop, modularity)))),
  modul.labelprop = unlist(lapply(monthly.labelprop, modularity)),
  modul.leiden = unlist(modul.monthly.leiden)
  ) %>%
  pivot_longer(-date)%>%
  ggplot(., aes(y = value, x = date, color = name))+
  geom_line()+
  xlab("")+
  ylab("modularity")+
  scale_color_discrete(name = "Method", labels = c("Label propagation", 
                                                   "Leiden"))+
  theme_bw(base_size = 16)

ggsave("results_MP_2023/modularity-trend.pdf", width = 12, height = 10)


for(i in 1:length(ig.rt.monthly)){
  pdf(paste0("results_MP_2023/rt-monthly-leiden-", dates[i],".pdf"), width = 12)
  plot(simplify(ig.rt.monthly[[i]]), 
       vertex.label = NA, 
       vertex.size = 5,
       vertex.color = monthly.leiden[[i]]$membership,
       edge.arrow.size = 0,
       main = dates[i])
  dev.off()
} 

### centrality over time ######################

outdeg.monthly <- data.frame(
    index = names(unlist(lapply(ig.rt.monthly, degree, mode = "out"))),
    degree.out = unlist(lapply(ig.rt.monthly, degree, mode = "out"))
  ) %>%
  mutate(year.month = substring(index,1,7),
         username = substring(index,9,nchar(index))) %>%
  group_by(username) %>%
  mutate(ever.high.indeg = any(degree.out > 30)) %>%
  left_join(party.id, by = "username")

outdeg.monthly.party <- outdeg.monthly %>%
  drop_na(party) %>%
  group_by(party, year.month) %>%
  summarize(outdeg.party.mean = mean(degree.out),
            outdeg.party.median = median(degree.out),
            outdeg.party.sd = sd(degree.out))


ggplot(filter(outdeg.monthly, ever.high.indeg == T),
       aes(y = degree.out, x = year.month, group = username,
           color = username))+
  geom_line() + geom_smooth(se = F) +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1))+
  xlab("")

ggplot(outdeg.monthly.party, aes(y = outdeg.party.mean, 
                             x = year.month, 
                             group = party, color = party))+
  geom_line()+geom_smooth(se = F) +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1))+
  xlab("")

ggplot(outdeg.monthly.party, aes(y = outdeg.party.median, 
                                x = year.month, 
                                group = party, color = party))+
  geom_line()+geom_smooth(se = F) +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1))+
  xlab("")

ggplot(outdeg.monthly.party, aes(y = outdeg.party.sd, 
                                x = year.month, 
                                group = party, color = party))+
  geom_line()+geom_smooth(se = F) +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1))+
  xlab("")
                            
  



