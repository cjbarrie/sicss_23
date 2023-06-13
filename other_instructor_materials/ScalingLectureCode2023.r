# 0.1 Installing Packages ----

# install.packages("devtools", dependencies=TRUE)
library(devtools)
devtools::install_github("uniofessex/asmcjr")


# 0.2 Setting the Global Options #####

setwd("~/Library/CloudStorage/OneDrive-UniversityofEdinburgh/++Active/Teaching/Scaling/Scaling2023")

options(width=60,
        str=strOptions(strict.width="wrap"))

# 0.3 Loading Packages ######
pckgs <-
  c(
    "tidyverse",
    "asmcjr",
    "basicspace",
    "effects",
    "ellipse",
    "gridExtra",
    "MASS",
    "MCMCpack",
    "oc",
    "pscl",
    "runjags",
    "rgenoud",
    "smacof",
    "wnominate",
    "xtable",
    "ggeffects",
    "boot"
  )
lapply(pckgs, require, character.only = TRUE)


# 1. Aldrich-McKelvey Scaling ----

data(franceEES2009)
head(franceEES2009, n = 10)

#Running Aldrich-Mckelvey scaling on France EES

result.france <- aldmck(franceEES2009, respondent=1, 
                                polarity=2,missing=c(77,88,89), verbose=FALSE)

str(result.france)    
summary(result.france)

# plot density of ideal points
plot_resphist(result.france, xlab="Left-Right")


# plot stimuli locations in addition to ideal point density

plot_resphist(result.france, addStim=TRUE, xlab = "Left-Right") +
  theme(legend.position="bottom", aspect.ratio=1) +
  guides(shape = guide_legend(override.aes = list(size = 4),nrow=3)) +
  labs(shape="Party", colour="Party")

# Isolate positive weights - people who saw political space “correctly” (i.e., that the Extreme Left Party is to the left of Le Pen’s National Front Party)

plot_resphist(result.france, addStim=TRUE, weights="positive",
              xlab = "Left-Right")  +
  theme(legend.position="bottom", aspect.ratio=1) +
  guides(shape = guide_legend(override.aes = list(size = 4),
                              nrow=3)) +
  labs(shape="Party", colour="Party")

# Isolating negative weights - only 28 respondents confused this ordering and have negative weights.

plot_resphist(result.france, addStim=TRUE, weights="negative",
              xlab = "Left-Right")  +
  theme(legend.position="bottom", aspect.ratio=1) +
  guides(shape = guide_legend(override.aes = list(size = 4),
                              nrow=3)) +
  labs(shape="Party", colour="Party")

# The respondents with negative weights cluster near the center of the ideological continuum. This makes intuitive sense, since left-wing and right-wing ideologues should be less likely to confuse the left-right ordering of major political actors.

# Bootstraping std errors

# The logic behind the bootstrap is that we can take repeated, random draws from a single dataset, and treat these draws as random samples themselves. In this case, we can take a random sample (with replacement) of respondents, perform the A-M scaling procedure on that data, save those results, and repeat the process a specified number of times (often, 100 or 1,000)

boot.france  <- boot.aldmck(franceEES2009,
                            polarity=2, respondent=1, missing=c(77,88,89),
                            verbose=FALSE, boot.args = list(R=100))

plot(boot.france$sumstats) +
  ylab(NULL) +
  xlab("Left-Right")

# 2. Basic Space Scaling using 2000 Convention Delegate Study which interviewed delegates to the Republican and Democratic National Conventions----

data(CDS2000)
head(CDS2000[,5:8])

issues <- as.matrix(CDS2000[,5:14])
head(issues)

#Blackbox syntax of Republican-Democrat left-right scale
result.repdem <- blackbox(issues,
                          missing=99, dims=3, minscale=5, verbose=TRUE)

result.repdem$fits
result.repdem$stimuli


# Party: Democrats = 1; Republicans = 2
party <- car:::recode(CDS2000[,1],
                      "1='Democrat'; 2='Republican'; else=NA",
                      as.factor=TRUE)
# Make the plot
plot_blackbox(result.repdem, dims=c(2,3), groupVar=party,
              xlab= "First Dimension\n(Left-Right)", 
              ylab="Second Dimension") +
  theme(legend.position="bottom", aspect.ratio=1) +
  guides(shape=guide_legend(override.aes=list(size=4))) + 
  labs(colour="Party")

# Norm Vectors
plot_blackbox(result.repdem, dims=c(1,2), groupVar=party,
              issueVector=c(2,10), data=issues,
              nudgeX= c(0,.125), nudgeY=c(-.05,0)) +
  theme(legend.position="bottom", aspect.ratio=1) +
  labs(colour="Party") +
  guides(shape = guide_legend(override.aes = list(size = 4)))

# 3. Multidimensional Scaling (SMACOF - Scaling by Majorizing a Complicated Function) ----

# In 1968, Wish (1971) asked 18 students in his psychological measurement class to rate the perceived similarity between each pair of 12 nations using a 9-point scale ranging from “1=very different” to “9=very similar.” He then constructed a matrix of average similarity ratings between the twelve nations

data(nations)
print(nations)

d <- (9-nations)^2

smacof_metric_result <- smacofSym(delta=d, ndim=2, itmax = 1000,
                                  type = "interval", eps=0.000001)

conf <- smacof_metric_result$conf
print(smacof_metric_result$niter)
print(smacof_metric_result$stress)

smacof.dat <- data.frame(
  dim1 = conf[,1],
  dim2 = conf[,2],
  country=rownames(nations)
)

ggplot(smacof.dat, aes(x=dim1, y=dim2)) +
  geom_point() +
  geom_text(aes(label=country), nudge_y=-.05) +
  xlab("") +
  ylab("") +
  geom_abline(slope=c(-3/5,5/3), intercept=c(0,0), lty=2) +
  xlim(-1,1) +
  ylim(-1,1) +
  theme_bw() +
  theme(aspect.ratio=1)

ndim <- 5
result <- vector("list", ndim)
for (i in 1:ndim){
  result[[i]] <- smacofSym(delta=d, ndim=i, type = "interval")
}
stress <- sapply(result, function(x)x$stress)

stress.df <- data.frame(
  stress=stress,
  dimension = 1:length(stress))

ggplot(stress.df, aes(x=dimension, y=stress)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x="Dimension", y="Stress")


# 4. Optimal Classification (OC) ----

data("france4", package="asmcjr")

rc <- rollcall(data=france4[,6:ncol(france4)],
               yea=1,
               nay=6,
               missing=7,
               notInLegis=c(8,9),
               legis.names=paste(france4$NAME,france4$CASEID,sep=""),
               vote.names=colnames(france4[6:ncol(france4)]),
               legis.data=france4[,2:5],
               vote.data=NULL,
               desc="National Assembly of the French Fourth Republic")

result2 <- oc(rc, dims=2, minvotes=20, lop=0.025,
              polarity=c(2,2), verbose=FALSE)
summary(result2)


result1 <- oc(rc, dims=1, minvotes=20, lop=0.025,
       polarity=2, verbose=FALSE)
summary(result1)

fits <- cbind(result1$fits, result2$fits)
colnames(fits) <- c("1 Dim", "2 Dim")
rownames(fits) <- c("% Correct", "APRE")
fits

result <- result2

pb <- rc$legis.data$PAR
pb <- car::recode(pb, '1="Communitst"; 2="Socialists";
     5="Christian Dems"; 7 = "Poujadists"; else=NA',
                  as.factor=TRUE)

plot_oc_coords(result, pb, dropNV=TRUE, ptSize=3) +
  theme_bw() +
  theme(aspect.ratio=1, legend.position="bottom") +
  scale_shape_manual(values=c("C", "S", "D", "P"),
                     labels= c("Communist", "Socialist",
                               "Christian Dem", "Poujadists"),
                     name = "Party") +
  scale_color_manual(values=gray.colors(5),
                     labels= c("Communist", "Socialist",
                               "Christian Dem", "Poujadists"),
                     name = "Party")+
  labs(x="First Dimension", y="Second Dimension")


deg2rad <- function(x)x*pi/180
rad45 <- deg2rad(45)
A <- matrix(c(cos(rad45), -sin(rad45),
              sin(rad45), cos(rad45)),
            nrow=2, ncol=2, byrow=TRUE)



pb <- rc$legis.data$PAR
pb <- car::recode(pb, '1="Communitsts"; 2="Socialists";
     5="Christian Dems"; 7 = "Poujadists"; else=NA',
                  as.factor=TRUE)
plot_oc_coords(result, pb, dropNV=TRUE, ptSize=3, rotMat=A) +
  theme_bw() +
  coord_fixed() +
  scale_shape_manual(values=c("C", "S", "D", "P"),
                     labels= c("Communist", "Socialist",
                               "Christian Dem", "Poujadists"),
                     name = "Party") +
  scale_color_manual(values=gray.colors(5),
                     labels= c("Communist", "Socialist",
                               "Christian Dem", "Poujadists"),
                     name = "Party")+
  labs(x="First Dimension", y="Second Dimension") +
  theme(aspect.ratio=1, legend.position="bottom")


### Roll Call Level Analysis - Regulation of Labor Conflicts6 February 1957 (Roll Call ID V3090)

which(colnames(rc$votes)=="V3090")

vote <- rc$votes[, "V3090"]

plot_oc_rollcall(result, rc, shapeVar=pb, 807, dropNV=TRUE,
                 ptSize=3, onlyErrors=FALSE)  +
  theme_bw() +
  theme(aspect.ratio=1, legend.position="bottom") +
  xlim(-1.1,1.1) + ylim(-1.1,1.1) +
  scale_shape_manual(values=c("D", "C", "P", "S"),
                     labels=c("Christian Dems", "Communists",
                              "Poujadists", "Socialists"),
                     name = "Party") +
  xlab("First Dimension (Left-Right)") +
  ylab("Second Dimension (Pro / Anti-Regime") +
  guides(shape=guide_legend(nrow=2)) +
  annotate("text", label=paste0("Yea = ",
                                sum(vote %in% rc$codes$yea)), colour="gray33", x=-.9, y=1) +
  annotate("text", label=paste0("Nay = ",
                                sum(vote %in% rc$codes$nay)), colour="gray67", x=-.9, y=.9) +
  annotate("text", label="Predicted\nYea",
           colour="gray33", x=.1, y=.2 ) +
  annotate("text", label="Predicted\nNay",
           colour="gray67", x=-.6, y=-.5 )







