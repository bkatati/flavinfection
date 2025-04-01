######################
# Manuscript Title:
# Title: Does Maize Ear Husk Cover Inadequacy at Harvest Construe a Correlation with Aspergillus section Flavi Infection as Dry Spell Would?
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###############################################################################################################
# FIGURE 3A - Influence of Grain Condition on Aflatoxin Contamination on Tight Husk Ears, Season1 (2018/2019) #
###############################################################################################################
library(ComplexHeatmap)
library(gplots)
# Season1: 2018/2019 .
seas1 <- read.csv(url("https://github.com/bkatati/flavinfection/Soil&Maize_Flav_seas1.csv"))
# seas1 <- read.csv("C:/Users/bkatati/OneDrive_WUR_2023/ACADEMIC/Thesis/Publications/Github/flavinfection/Soil&Maize_Flav_seas1.csv")
# NB: if file path error occurs, download csv file "Soil&Maize_Flav_seas1.csv" from site "https://github.com/bkatati/flavinfection" 
# On your PC, create appropriate local drive path and change above file path to local drive.

head(seas1)
dim(seas1)
matrix <- as.matrix(seas1[,c(6:10)])
head(matrix)
class(seas1)
class(matrix)
colnames(matrix)
rownames(matrix) <- (seas1[,1]) # Exclude first column
head(matrix)
fontsize <- 0.65 # adjust fontsize
library(circlize)
col_fun = colorRamp2(c(0, 0.2,0.6,3), c("black", "green","green", "yellow"))
# UNCLUSTERED:
Heatmap(matrix, col = col_fun,
        cluster_columns=F,
        row_names_side = "left",
        row_dend_side = "left",
        row_names_gp=gpar(cex=fontsize),
        row_dend_width = unit(3, "cm"),
        cluster_rows = FALSE, name = "Heat
scale")

# Flavi visible throughout soil,
# but on maize, is most pronounced under weather variable S1, including aflatoxin

##################################################################################################
# FIGURE 3B - Influence of Husk Cover Condition on Flavi infection of Ears, Season2 (2020/2021) ##
##################################################################################################

# Season2: 2020/2021 .
seas2 <- read.csv(url("https://github.com/bkatati/flavinfection/Soil&Maize_Flav_seas2.csv"))
# NB: if file path error occurs, download csv file "Soil&Maize_Flav_seas.csv" from site "https://github.com/bkatati/flavinfection" 
# On your PC, create appropriate local drive path and change above file path to local drive.

head(seas2)
dim(seas2)
matrix <- as.matrix(seas2[,c(8:12)])
head(matrix)
class(seas2)
class(matrix)
colnames(matrix)
rownames(matrix) <- (seas2[,1])
head(matrix)
fontsize <- 0.65 # adjust fontsize
library(circlize)
col_fun = colorRamp2(c(0, 0.2,0.6,3), c("black", "green","green", "yellow"))
# UNCLUSTERED:
Heatmap(matrix, col = col_fun,
        cluster_columns=F,
        row_names_side = "left",
        row_dend_side = "left",
        row_names_gp=gpar(cex=fontsize),
        row_dend_width = unit(3, "cm"),
        cluster_rows = FALSE, name = "Heat
scale")

# Flavi visible throughout soil,
# few visible under variable S2; No aflatoxin detected.


# Significance of the Influence of Husk Cover Condition on Flavi infection of Ears
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
head(seas2)
dim(seas2)
wilcox.test(seas2$huskOK, seas2$huskPoor, paired=TRUE) 

# No effect: P = 0.05906

################################################################################
# Influence of agronomic factors and weather variables on maize Flavi infection#
################################################################################

# A) Weather variables

######################

# Check normality of distribution (based on season1 & 2) for soil and maize:

# Maize Flavi:
# ~~~~~~~~~~

diffM <- seas1$Maize_CFU - seas2$Maize_CFU
diffM
shapiro.test(diffM)
# Not normal distribution ( P < 0.001)

# Soil Flavi:
# ~~~~~~~~~~

# Check normality of distribution (based on season1 & 2):
diffS <- seas1$Soil_CFU -  seas2$Soil_CFU
diffS
shapiro.test(diffS)
# Not normal distribution (P < 0.001).

# Use non-parametric
#~~~~~~~~~~~~~~~~~~~

# Apply Wilcox Test:
# ------------------

# Combined season 1 and 2:
data <- read.csv2(url("https://github.com/bkatati/flavinfection/Soil&Maize_Flav.csv"))
# NB: if file path error occurs, download csv file "Soil&Maize_Flav.csv" from site "https://github.com/bkatati/flavinfection" 
# On your PC, create appropriate local drive path and change above file path to local drive.

head(data)

# Soil Flavi:
# ~~~~~~~~~~

x1 <- data$Soil_CFU
g <- factor(data$Weather_var)
pairwise.wilcox.test(x1, g, p.adjust.method = "bonf", paired = FALSE)

# Pairwise comparisons using Wilcoxon rank sum test with continuity correction 

# data:  x and g 

#        N1    N2    S1   
#   N2 0.208 -     -    
#   S1 1.000 0.014 -    
#   S2 1.000 0.015 1.000

# N2 had more soil flavi density (CFU/g) than S1 and S2.

################################################################
## T-test for Aspergillus section Flavi in (Tight Husk ears) ###
################################################################

# Maize Flavi
# ~~~~~~~~~~

x2 <- data$Maize_CFU
pairwise.wilcox.test(x2, g, p.adjust.method = "bonf", paired = FALSE)

#       N1      N2      S1     
#  N2 1.00000 -       -      
#  S1 5.7e-05 0.00013 -      
#  S2 0.24097 0.82874 0.00417

# S1 had more maize flavi density than rest of variables
# No correspondingly higher Flavi in for maize N2 Flavi as that of soil.

# B) Agronomic Factors

################################

# Apply Multi-Dimension Scaling:
# ------------------------------

# Obtained as average of lowest Aspergillus sample then included all samples with  0's, finding the average.
# NB: In this work, we set LOD for Flavi at 0,1520 CFU/g kernels.

mycobiome <- read.csv2(url("https://github.com/bkatati/flavinfection/MDS_Flav.csv"))
# NB: if file path error occurs, download csv file "MDS_Flav.csv" from site "https://github.com/bkatati/flavinfection" 
# On your PC, create appropriate local drive path and change above file path to local drive.

head(mycobiome)
dim(mycobiome)

mycobiome.matrix<-as.matrix(mycobiome[,7:16])
head(mycobiome.matrix)

# Transform matrix to square roots to minimize influence of highly represented variables:

mycobiome.mat<-sqrt(mycobiome.matrix)
head(mycobiome.mat)

require(vegan)
set.seed(79) #for reproducible results

mycobiomeMDS<-metaMDS(mycobiome.mat, distance="bray", k=2, trymax=35, autotransform=TRUE, rm.na= T) #k = number of dimensions
mycobiomeMDS

# Stress value < 0.2, acceptable:

library(ggplot2); packageVersion("ggplot2")
MDS1 <- mycobiomeMDS$points[,1]
MDS2 <- mycobiomeMDS$points[,2]
mycobiome.plot<-cbind(mycobiome, MDS1, MDS2)
head(mycobiome.plot)

require(dplyr) 
fit<-envfit(mycobiomeMDS, mycobiome.mat)
arrow<-data.frame(fit$vectors$arrows,R = fit$vectors$r, P = fit$vectors$pvals)
arrow$Agronomic_Factors <- rownames(arrow)

###############################################################################
##Table 3 - Agronomic factors linked to preharvest maize Flavi proliferation###
###############################################################################

# Collect only significant (p < 0.05) predictors, irrespective of contribution value (R > 0):
arrow.p<-filter(arrow, P < 0.05, R > 0)
arrow.p

# Note3: you may write arrow values (P-values and R-sq):
# write.csv2(arrow.p, file = ("your path/filename.csv"), row.names = F)

set.seed(10) # run each time for reproducible coordinates
p <- ggplot(data=mycobiome.plot, aes(MDS1,MDS2)) + theme_classic() +
  geom_point(data=mycobiome.plot, aes(MDS1, MDS2, color=Flavi_level), position=position_jitter(1)) +##separates overlapping points
  stat_ellipse(aes(fill=Flavi_level), alpha=0.25,type='t',size = 1, segments = 360, level = 0.67, geom="polygon", show.legend = NA) +##changes shading on ellipses
  theme_classic() +
  geom_segment(data=arrow.p, aes(x=0, y=0, xend=NMDS1, yend=NMDS2, lty = Agronomic_Factors), arrow=arrow(length=unit(.5, "cm")*arrow.p$R), colour = "blue", size = .5, alpha = 1) ##add arrows (scaled by R-squared value)

#

set.seed(2023) # for reproducible diagram

#################
## Figure 4 #####
#################

p + theme(legend.position = "right", text = element_text(size = 12), axis.title = element_text(size = 13),
          axis.text = element_text(size = 10)) +
  annotate("text", x=(-1.1), y=(1), label=paste('Stress =',round(mycobiomeMDS$stress,3))) +
  annotate("text", x=(-1), y=(0.35), label=paste('no burn')) +
  annotate("text", x=(-1), y=(0.08), label=paste('pest')) +
  annotate("text", x=(-1), y=(-0.05), label=paste('S1 (dryspell)')) +
  annotate("text", x=(0.2), y=(1.03), label=paste('S2 (low rain)')) +
  annotate("text", x=(-1), y=(0.55), label=paste('monocropping')) +
  annotate("text", x=(-1.1), y=(0.45), label=paste('early maturity')) +
  annotate("text", x=(.1), y=(-1), label=paste('medium maturity')) +
  labs(title = expression(paste("MDS of Agronomic Factors that may Influence section", italic(" Flavi"), " quantity in Maize"))) +
  theme(title =element_text(size=10))

############################################################
# Effect of seed condition on Aflatoxin - Season1 2018/2019#
############################################################

head(seas1)
dim(seas1)
wilcox.test(seas1$AF_clean, seas1$AF_poor, paired=TRUE) 

# There was difference in AF between poor and clean seed: p-value = 0.006633

# Geometric mean for actually contaminated seed (variable S1 of season one):
AFL <- seas1$Weather_var=="S1"

exp(mean(seas1$lnAF_poor[AFL]))
exp(mean(seas1$lnAF_clean[AFL]))

# Poor seed had significantly higher aflatoxin geometric mean (11.2 ug/kg) than clean seed (2.3 ug/kg).

########################################################################################
# Supplemental Table S3: Effect of Husk Condition on visible fungi incidence, season-2##
########################################################################################
Fungal <- read.csv("C:/Users/bkatati/OneDrive_WUR_2023/ACADEMIC/Thesis/Publications/Github/flavinfection/Husk_fungal.csv")
dim(Fungal)
head(Fungal)

Hgood <- Fungal$cover=="inadequate"
Hpoor <- Fungal$cover=="full"

shapiro.test(Fungal$Fungal_proportion[Hgood])
shapiro.test(Fungal$Fungal_proportion[Hpoor])

# Not normal distribution ( P = 0.002647; 0.001613)
# Apply non-parametric:

wilcox.test(Fungal$Fungal_proportion[Hgood], Fungal$Fungal_proportion[Hpoor], paired=T)

# Wilcoxon signed rank test with continuity correction

# data: 
# V = 160.5, p-value = 0.5228

# alternative hypothesis: true location shift is not equal to 0

# The qualitative difference in fungal incidences not significant 
# between inadequate husk cover and full husk.

#################################END#########################################
