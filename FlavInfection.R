######################
# Manuscript Title:
# Husk cover condition of preharvest maize is not a predictor for Aspergillus infection
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#########################################################################
# FIGURE 5 - Influence of husk cover condition on maize Flavi infection #
#########################################################################
library(ComplexHeatmap)
library(gplots)
# ASPERGILLUS INGRESS SEASON-1 & -2
data <- read.csv2("C:/Users/bkatati/OneDrive_WUR_2023/ACADEMIC/Thesis/Publications/Github/Flavinfection/post/Soil&Maize_Flav.csv")
# write.csv2(data, file="C:/Users/bkatati/OneDrive_WUR_2023/ACADEMIC/Thesis/Publications/Github/Flavinfection/Heatmap2_SoilMaize_Flav.csv")
head(data)
dim(data)
matrix <- as.matrix(data[,c(8:14)])
head(matrix)
class(data)
class(matrix)
colnames(matrix)
rownames(matrix) <- (data[,1])
head(matrix)
fontsize <- 0.45 # adjust fontsize
library(circlize)
col_fun = colorRamp2(c(-0.01,0, 0.2,0.6,3), c("white", "black", "green","green", "yellow"))
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
# but most pronounced under S1 on maize.

#### EFFECT OF HUSK COVER ON APSERGILLUS IN MAIZE:

Asp <- subset(data, Season == "two")
head(Asp)
dim(Asp)
wilcox.test(Asp$huskOK, Asp$huskPoor, paired=TRUE) 

# No effect: P = 0.05906

########################################################################################
# Table 2 Influence of agronomic factors and weather variables on maize Flavi infection#
########################################################################################

# Soil Flavi:
# ~~~~~~~~~~
# Check normality of distribution (based on season1 & 2):
seas1 <- data$Season=="one"
seas2 <- data$Season=="two"
diff1 <- data$Soil_CFU[seas1] -  data$Soil_CFU[seas2]
diff1
shapiro.test(diff1)
# Not normal distribution ( P < 0.05). Use non-parametric

#####

x1 <- data$Soil_CFU
g <- factor(data$Weather_var)
pairwise.wilcox.test(x1, g, p.adjust.method = "bonf", paired = FALSE)

# Pairwise comparisons using Wilcoxon rank sum test with continuity correction 

# data:  x and g 

#        N1    N2    S1   
#   N2 0.208 -     -    
#   S1 1.000 0.014 -    
#   S2 1.000 0.015 1.000

# N2 had more flavi density (CFU/g) than S1 and S2.

# Maize Flavi:
# ~~~~~~~~~~

# Check normality of distribution (based on season1 & 2):

diff2 <- data$Maize_CFU[seas1] -  data$Maize_CFU[seas2]
diff2
shapiro.test(diff2)
# Not normal distribution ( P < 0.05)

x2 <- data$Maize_CFU
pairwise.wilcox.test(x2, g, p.adjust.method = "bonf", paired = FALSE)

#       N1      N2      S1     
#  N2 1.00000 -       -      
#  S1 5.7e-05 0.00013 -      
#  S2 0.24097 0.82874 0.00417


##############################
# MULTI-DIMENSITONAL SCALING #
##############################

#NMDS - AGRONOMIC FACTORS ####
# Obtained CFU/g limit of detection to replace zeroes.
# Obtain this as average of lowest Flavi CFU/g sample then included all samples with  0's, and find the average.
# NB: In this work, we set LOD for Flavi at 0,1520 CFU/g kernels.

mycobiome <- read.csv2("C:/Users/bkatati/OneDrive_WUR_2023/ACADEMIC/Thesis/Publications/Github/Flavinfection/post/MDS_Flav.csv")
# NB: if file path error occurs, download csv file "MDS_df_FusB1.csv" from site "https://github.com/bkatati/nichemap" 
# On your PC, create appropriate file path on local drive.

head(mycobiome)
dim(mycobiome)

mycobiome.matrix<-as.matrix(mycobiome[,7:13])
head(mycobiome.matrix)

# Transform matrix to square roots to minimize influence of most abundant genera:

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

##Table 3 #######
#################

# Collect only significant (p < 0.05) predictors, irrespective of contribution value (R > 0):
arrow.p<-filter(arrow, P < 0.05, R > 0)
arrow.p

# Note3: you may write arrow values (P-values and R-sq):
# write.csv2(arrow.p, file = ("C:/Users/bkatati/OneDrive_WUR_2023/ACADEMIC/Thesis/Publications/Github/Flavinfection/predictors.csv"), row.names = F)

set.seed(10) # run each time for reproducible coordinates
p <- ggplot(data=mycobiome.plot, aes(MDS1,MDS2)) + theme_classic() +
  geom_point(data=mycobiome.plot, aes(MDS1, MDS2, color=Flavi_level), position=position_jitter(1)) +##separates overlapping points
  stat_ellipse(aes(fill=Flavi_level), alpha=0.25,type='t',size = 1, segments = 360, level = 0.9, geom="polygon", show.legend = NA) +##changes shading on ellipses
  theme_classic() +
  geom_segment(data=arrow.p, aes(x=0, y=0, xend=NMDS1, yend=NMDS2, lty = Agronomic_Factors), arrow=arrow(length=unit(.5, "cm")*arrow.p$R), colour = "blue", size = .5, alpha = 1) ##add arrows (scaled by R-squared value)

#

set.seed(2023) # for reproducible diagram

## Figure 6 #####
###eeeeeeeeeeee##

p + theme(legend.position = "right", text = element_text(size = 12), axis.title = element_text(size = 13),
          axis.text = element_text(size = 10)) +
  annotate("text", x=(-1.1), y=(1), label=paste('Stress =',round(mycobiomeMDS$stress,3))) +
  annotate("text", x=(-1), y=(-0.55), label=paste('no burn')) +
  annotate("text", x=(-1), y=(0.25), label=paste('pest')) +
  annotate("text", x=(-1), y=(-0.05), label=paste('S1 (dryspell)')) +
  annotate("text", x=(0.2), y=(1), label=paste('S2 (low rain)')) +
  annotate("text", x=(-1), y=(0.55), label=paste('monocropping')) +
  labs(title = expression(paste("MDS of Agronomic Factors that may Influence", italic(" Flavi"), " quantity in Maize"))) +
  theme(title =element_text(size=10))

#########################################
# Effect of seed condition on Aflatoxin #
#########################################

AF <- subset(data, Season == "one")
head(AF)
dim(AF)
wilcox.test(AF$AF_clean, AF$AF_poor, paired=TRUE) 

# There was difference in AF between poor and clean seed: p-value = 0.006633

# Geometric mean for actually contaminated seed (variable S1 of season one):
W <- AF$Weather_var=="S1"

exp(mean(AF$lnAF_poor[W]))
exp(mean(AF$lnAF_clean[W]))

# Poor seed had higher aflatoxin geometric mean (11.1 ug/kg) than clean seed (2.2 ug/kg).

############################ END ###############################################
