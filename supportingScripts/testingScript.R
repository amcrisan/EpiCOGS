library(seqinr)
library(ape)
library(phangorn)
library(ggdendro)
# 
# dat<-read.FASTA("../../Documents/snpfiles/julian_recoded_x.fasta")
# 
# datSample<-readRDS("fakeData/FakeData_obsandGenotype.RDS")
# 
# #grab the state of california
# datSample<-datSample %>%
#   filter(State=="CA") %>%
#   sample_n(277)
# 
# dat_phyDat <- phyDat(dat, type = "DNA", levels = NULL)

#----- Adding to "metadata" -----
snpFASTA<-sapply(dat_phyDat, function(x){paste0(c("a","c","g","t")[x],collapse="")})
datSample<-cbind(datSample,snpFASTA)

data <-data
dat_test<-as.DNAbin(as.character(data$snpFASTA))
dat_test<-phyDat(dat_test,type = "DNA", levels = NULL)


dna_dist <- dist.ml(dat_test, model="JC69")
dat_UPGMA <- upgma(dna_dist)
plot(dat_UPGMA, main="UPGMA")

hc_dat <- as.hclust.phylo(dat_UPGMA)

dhc <- as.dendrogram(hc_dat)
# Rectangular lines
ddata <- dendro_data(dhc, type = "rectangle")

ggplot(segment(ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  coord_flip() +
  theme_bw()+
  xlab("")+
  ylab("Distance")+
  scale_y_reverse(expand = c(0.2, 0))+
  theme(axis.text.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor.y= element_blank(),
        panel.border=element_blank(),
        axis.ticks.y = element_blank())


# --- fitting a probability tree ---
fit <- pml(dat_UPGMA, data=dat_phyDat)
fitJC <- optim.pml(fit, model = "GTR", rearrangement = "stochastic")
logLik(fitJC)
bs <- bootstrap.pml(fitJC, bs=100, optNni=TRUE, multicore=TRUE, control = pml.control(trace=0))
plotBS(midpoint(fitJC$tree), bs, p = 50, type="p")

# ---- OTHER RANDOM FITTING NOTES ---- 
# Jukes-Cantor (starting tree from NJ)  
fitJC <- pml(tree, Laurasiatherian)  
# optimize edge length parameter     
fitJC <- optim.pml(fitJC)
fitJC 


## Not run:    
# search for a better tree using NNI rearrangements     
fitJC <- optim.pml(fitJC, optNni=TRUE)
fitJC   
plot(fitJC$tree)

# JC + Gamma + I - model
fitJC_GI <- update(fitJC, k=4, inv=.2)
# optimize shape parameter + proportion of invariant sites     
fitJC_GI <- optim.pml(fitJC_GI, optGamma=TRUE, optInv=TRUE)
# GTR + Gamma + I - model
fitGTR <- optim.pml(fitJC_GI, rearrangement = "stochastic", 
                    optGamma=TRUE, optInv=TRUE, model="GTR") 

#convert to dendrogram
plot.phylo(fitJC)
plot(fitJC, "radial",cleaves=0.1)

save(fitJC)
#read jenn's tree
