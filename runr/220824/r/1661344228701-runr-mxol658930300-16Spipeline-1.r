#First we need to load the required packages
library(dada2) #https://benjjneb.github.io/dada2/dada-installation.html
packageVersion("dada2")
library(ShortRead) #https://bioconductor.org/packages/release/bioc/html/ShortRead.html
packageVersion("ShortRead")
library(Biostrings) #https://bioconductor.org/packages/release/bioc/html/Biostrings.html
packageVersion("Biostrings")
library(phyloseq) #https://www.bioconductor.org/packages/release/bioc/html/phyloseq.html
packageVersion("phyloseq")

#If you do not have these in your environment, you can install using the below commands
#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install(c("dada2", "ShortRead", "Biostrings", "phyloseq"))

#Have a look at what we have
path <- "."
list.files(path)

#Get the raw data
raw.path <- "RawReads"
list.files(raw.path)

#Put the raw read files in a list. One list for forward and one for reverse
fnFs <- sort(list.files(raw.path, pattern="_R1_001.fastq.gz", full.names = TRUE))
fnRs <- sort(list.files(raw.path, pattern="_R2_001.fastq.gz", full.names = TRUE))

#The presence of ambiguous bases (Ns) in the sequencing reads makes accurate mapping of short 
#primer sequences difficult. Next we are going to “pre-filter” the sequences just to remove 
#those with Ns, but perform no other filtering
fnFs.filtN <- file.path(path, "filtN", basename(fnFs))
fnRs.filtN <- file.path(path, "filtN", basename(fnRs))
filterAndTrim(fnFs, fnFs.filtN, fnRs, fnRs.filtN, maxN = 0)

#Adapters and Primers are sequencing artifacts. These may be seen as chimeras by the
#bioinformatic pipelines and will produce problems later on

#We first start with the adapters
#Adapter sequences
FWD <- "TCGTCGGCAGCGTCAGATGTGTATAAGAGACAG"
REV <- "GTCTCGTGGGCTCGGAGATGTGTATAAGAGACAG"

#Now we need a function to obtain all the orientations of the input sequence
allOrients <- function(primer) {
  require(Biostrings)
  dna <- DNAString(primer)
  orients <- c(Forward = dna, Complement = complement(dna), Reverse = reverse(dna), 
               RevComp = reverseComplement(dna))
  return(sapply(orients, toString))
}

#Use the function on our adapters
FWD.orients <- allOrients(FWD)
REV.orients <- allOrients(REV)

#Check the output of the function
FWD.orients

#No we will count the number of times the adapters appear in the forward and reverse read
#while considering all possible adapter orientations
#We again create a function to do this for us

primerHits <- function(primer, fn) {
  nhits <- vcountPattern(primer, sread(readFastq(fn)), fixed = FALSE)
  return(sum(nhits > 0))
}

#Now we run this function on all our samples
#This may take a while
for(i in 1:length(fnFs.filtN)){
  x <- rbind(FWD.ForwardReads = sapply(FWD.orients, primerHits, fn = fnFs.filtN[[i]]), 
      FWD.ReverseReads = sapply(FWD.orients, primerHits, fn = fnRs.filtN[[i]]), 
      REV.ForwardReads = sapply(REV.orients, primerHits, fn = fnFs.filtN[[i]]), 
      REV.ReverseReads = sapply(REV.orients, primerHits, fn = fnRs.filtN[[i]]))
  print(x)
}

#Now that we know where and in what orientation the adapters are, we can remove them
#For this we use cutadapt http://cutadapt.readthedocs.io/en/stable/index.html
cutadapt <- "/Users/pierneefr/anaconda3/envs/CutAdapt/bin/cutadapt" #Specify the location of cutadapt
system2(cutadapt, args = "--version") #This allows us to run shell commands from within R

#We will be creating a new directory to store the results
#These are still paired-end fastq files
path.cutAdapt <- file.path(path, "CutAdapters")
if(!dir.exists(path.cutAdapt)) dir.create(path.cutAdapt) #Create directory if it does not exist
fnFs.cutAdapt <- file.path(path.cutAdapt, basename(fnFs.filtN)) #Forward out
fnRs.cutAdapt <- file.path(path.cutAdapt, basename(fnRs.filtN)) #Reverse out
#Generate the reverse compliment of the adapters
FWD.RC <- dada2:::rc(FWD)
REV.RC <- dada2:::rc(REV)
#We now need to set the cutadapt parameters
R1.flags <- paste("-g", REV.RC) #Forward reads
R2.flags <- paste("-G", FWD.RC) #Reverse reads
#Then we run cutadapt on all the samples
#Remember R1 and R2 always go together
#They are married
#This may take some time and a lot of information will be sent to the console
#The most important is that we have results in the CutAdapters directory
for(i in seq_along(fnFs)) {
  system2(cutadapt, args = c(R1.flags, R2.flags, "-n", 2,
                             "-o", fnFs.cutAdapt[i], "-p", fnRs.cutAdapt[i],
                             fnFs.filtN[i], fnRs.filtN[i]))
}

#Now we check to see if this worked
#There should be no adapters
for(i in 1:length(fnFs.cutAdapt)){
  x <- rbind(FWD.ForwardReads = sapply(FWD.orients, primerHits, fn = fnFs.cutAdapt[[i]]), 
             FWD.ReverseReads = sapply(FWD.orients, primerHits, fn = fnRs.cutAdapt[[i]]), 
             REV.ForwardReads = sapply(REV.orients, primerHits, fn = fnFs.cutAdapt[[i]]), 
             REV.ReverseReads = sapply(REV.orients, primerHits, fn = fnRs.cutAdapt[[i]]))
  print(x)
}

#We use the same methodology for the primers
#Primer sequences
FWD <- "CCTACGGGNGGCWGCAG"
REV <- "GACTACHVGGGTATCTAATCC"

FWD.orients <- allOrients(FWD)
REV.orients <- allOrients(REV)

#Check if the primers are present and in what orientation
#We already have a function
#Use the results from the previous step which removed the adapters
for(i in 1:length(fnFs.cutAdapt)){
  x <- rbind(FWD.ForwardReads = sapply(FWD.orients, primerHits, fn = fnFs.cutAdapt[[i]]), 
             FWD.ReverseReads = sapply(FWD.orients, primerHits, fn = fnRs.cutAdapt[[i]]), 
             REV.ForwardReads = sapply(REV.orients, primerHits, fn = fnFs.cutAdapt[[i]]), 
             REV.ReverseReads = sapply(REV.orients, primerHits, fn = fnRs.cutAdapt[[i]]))
  print(x)
}

#Definitely need some cutting
path.cutPrime <- file.path(path, "CutPrimers") #Output directory
if(!dir.exists(path.cutPrime)) dir.create(path.cutPrime)
fnFs.cutPrime <- file.path(path.cutPrime, basename(fnFs.cutAdapt))
fnRs.cutPrime <- file.path(path.cutPrime, basename(fnRs.cutAdapt))
FWD.RC <- dada2:::rc(FWD)
REV.RC <- dada2:::rc(REV)
R1.flags <- paste("-g", FWD, "-g", REV.RC, "-g", FWD.RC, "-g", REV)
R2.flags <- paste("-G", REV, "-G", FWD.RC, "-G", REV.RC, "-G", FWD)
for(i in seq_along(fnFs.cutAdapt)) {
  system2(cutadapt, args = c(R1.flags, R2.flags, "-n", 2,
                             "-o", fnFs.cutPrime[i], "-p", fnRs.cutPrime[i],
                             "--max-n 0 --minimum-length 75",
                             fnFs.cutAdapt[i], fnRs.cutAdapt[i]))
}

#Check if the primers have been cut
for(i in 1:length(fnFs.cutPrime)){
  x <- rbind(FWD.ForwardReads = sapply(FWD.orients, primerHits, fn = fnFs.cutPrime[[i]]), 
             FWD.ReverseReads = sapply(FWD.orients, primerHits, fn = fnRs.cutPrime[[i]]), 
             REV.ForwardReads = sapply(REV.orients, primerHits, fn = fnFs.cutPrime[[i]]), 
             REV.ReverseReads = sapply(REV.orients, primerHits, fn = fnRs.cutPrime[[i]]))
  print(x)
}

#Sanity checks are important
#Are we sure the adapters are also removed?
#Did we maybe use the wrong file?
#We just check for the adapters again
#Adapters
FWD <- "TCGTCGGCAGCGTCAGATGTGTATAAGAGACAG"
REV <- "GTCTCGTGGGCTCGGAGATGTGTATAAGAGACAG"
FWD.orients <- allOrients(FWD)
REV.orients <- allOrients(REV)
#Check if the adapters have been cut
for(i in 1:length(fnFs.cutPrime)){
  x <- rbind(FWD.ForwardReads = sapply(FWD.orients, primerHits, fn = fnFs.cutPrime[[i]]), 
             FWD.ReverseReads = sapply(FWD.orients, primerHits, fn = fnRs.cutPrime[[i]]), 
             REV.ForwardReads = sapply(REV.orients, primerHits, fn = fnFs.cutPrime[[i]]), 
             REV.ReverseReads = sapply(REV.orients, primerHits, fn = fnRs.cutPrime[[i]]))
  print(x)
}

#All good!
#We have everything we need for the next step
#Some cleaning
rm(list=ls())

#We have cleaned the data nicely and now we will proceed to dada2
#OTU vs ASV?

#Generate some paths
path <- "."
path.cutPrime <- file.path(path, "CutPrimers")

#Generate lists for the forward and reverse reads
fnFs <- sort(list.files(path.cutPrime, pattern="_R1_001.fastq.gz", full.names = TRUE))
fnRs <- sort(list.files(path.cutPrime, pattern="_R2_001.fastq.gz", full.names = TRUE))

#Now we need to check the quality of the data
#This information will help us in determining the parameters to be used for quality filtering
#Forward reads
plotQualityProfile(fnFs[1:3])
#In gray-scale is a heat map of the frequency of each quality score at each base position
#The mean quality score at each position is shown by the green line, 
#and the quartiles of the quality score distribution by the orange lines.
#The forward reads are good quality. We generally advise trimming the last few nucleotides 
#to avoid less well-controlled errors that can arise there.

#Reverse reads
plotQualityProfile(fnRs[1:3])
#The reverse will always look worse than the forward

#Some filtering
#Create output directory and assign names to new results
sample.names <- sapply(strsplit(basename(fnFs), "_L001_"), `[`, 1)
filtFs <- file.path(path, "CutPrimersFilt", paste0(sample.names, "_F_filt.fastq.gz"))
filtRs <- file.path(path, "CutPrimersFilt", paste0(sample.names, "_R_filt.fastq.gz"))
names(filtFs) <- sample.names
names(filtRs) <- sample.names

#We’ll use standard filtering parameters: maxN=0 (DADA2 requires no Ns), truncQ=2, rm.phix=TRUE and maxEE=2
#The maxEE parameter sets the maximum number of “expected errors” allowed in a read, which is a better 
#filter than simply averaging quality scores.
#The reads must still overlap after truncation in order to merge them later!
#Where do you think we should truncate based on the quality profiles?
out <- filterAndTrim(fnFs, filtFs, fnRs, filtRs, truncLen=c(270,220),
                     maxN=0, maxEE=c(2,2), truncQ=2, rm.phix=TRUE,
                     compress=TRUE)

#How many reads did we lose
#Remember, they are married!
head(out)
#Check the quality of the filtered data
plotQualityProfile(filtFs[1:3])
plotQualityProfile(filtRs[1:3])

#Looking good
#Still have more than enough data
#The DADA2 algorithm makes use of a parametric error model (err) and every amplicon dataset has a 
#different set of error rates. The learnErrors method learns this error model from the data, 
#by alternating estimation of the error rates and inference of sample composition until they 
#converge on a jointly consistent solution. As in many machine-learning problems, the algorithm must 
#begin with an initial guess, for which the maximum possible error rates in this data are used 
#(the error rates if only the most abundant sequence is correct and all the rest are errors).
#This may take a while
errF <- learnErrors(filtFs)
errR <- learnErrors(filtRs)

#Some visualization would be nice
plotErrors(errF, nominalQ=TRUE)
#The error rates for each possible transition (A→C, A→G, …) are shown. 
#Points are the observed error rates for each consensus quality score. 
#The black line shows the estimated error rates after convergence of the machine-learning algorithm. 
#The red line shows the error rates expected under the nominal definition of the Q-score. 
#Here the estimated error rates (black line) are a good fit to the observed rates (points), 
#and the error rates drop with increased quality as expected

#The dada2 algorithm https://www.nature.com/articles/nmeth.3869
dadaFs <- dada(filtFs, err=errF)
dadaRs <- dada(filtRs, err=errR)
#What does the output mean?

#We now merge the forward and reverse reads together to obtain the full denoised sequences
#Merging is performed by aligning the denoised forward reads with the reverse-complement of the 
#corresponding denoised reverse reads, and then constructing the merged “contig” sequences
#What is a contig? What did we sequence?
#By default, merged sequences are only output if the forward and reverse reads overlap by at least 12 bases
#and are identical to each other in the overlap region
mergers <- mergePairs(dadaFs, filtFs, dadaRs, filtRs, verbose=TRUE)

#We can now construct an amplicon sequence variant table (ASV) table
#a higher-resolution version of the OTU table produced by traditional methods
seqtab <- makeSequenceTable(mergers)

#We need to remove chimeras! What is a chimera
#The core dada method corrects substitution and indel errors, but chimeras remain
#Fortunately, the accuracy of sequence variants after denoising makes identifying chimeric ASVs 
#simpler than when dealing with fuzzy OTUs
#Chimeric sequences are identified if they can be exactly reconstructed by combining a left-segment 
#and a right-segment from two more abundant “parent” sequences
seqtab.nochim <- removeBimeraDenovo(seqtab, method="consensus", verbose=TRUE)

#What percentage did we lose because of chimeras?
sum(seqtab.nochim)/sum(seqtab)

#We now need to do some Read Tracking
#How much and where did we lose data?
#Need a little functio for this and voila!
getN <- function(x) sum(getUniques(x))
track <- cbind(out, sapply(dadaFs, getN), sapply(dadaRs, getN), sapply(mergers, getN), rowSums(seqtab.nochim))
colnames(track) <- c("input", "filtered", "denoisedF", "denoisedR", "merged", "nonchim")
rownames(track) <- sample.names
head(track)
path.16Sresults <- file.path(path, "16Sresults")
if(!dir.exists(path.16Sresults)) dir.create(path.16Sresults) #Create directory if it does not exist
write.table(track, "16Sresults/ReadTracking.txt", quote = FALSE)
#We can do better than this!
#What about some visualization
#Hello ggplots
library(reshape2) #https://www.rdocumentation.org/packages/reshape2/versions/1.4.4
library(ggplot2) #https://ggplot2.tidyverse.org/
library(ggsci) #https://cran.r-project.org/web/packages/ggsci/vignettes/ggsci.html
track.long <- melt(as.matrix(track))
colnames(track.long) <- c("SampleID", "dada2Step", "Reads")
ggplot(track.long, aes(fill=dada2Step, y=Reads, x=SampleID)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_classic()+
  scale_fill_startrek()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_hline(yintercept = 100000)
#Very nice!
#What are we looking at?

#Great, so we have some non-chimera sequences
#What should these represent?
#What do we want to do with these?
#Assign Taxonomy and Save ASV
#I like Silva https://www.arb-silva.de/
#But there are various others available
taxa <- assignTaxonomy(seqtab.nochim, "silva_nr99_v138.1_train_set.fa.gz") #https://benjjneb.github.io/dada2/training.html
#What do we have?
taxa.print <- taxa
rownames(taxa.print) <- NULL
head(taxa.print)
#Looks like some bacteria. Great!

#This does not really mean anything to us without some metadata/sample information
#What should this information look like?
SampleSheet <- read.csv("SampleSheet1.csv", row.names=1)
#Great, we have a ASV table and some added information
#We now need to combine these into one object
#where the sample information is connected to the samples in the ASV table
#Hello phyloseq https://joey711.github.io/phyloseq/

SixTeenS.ps <- phyloseq(otu_table(seqtab.nochim, taxa_are_rows=FALSE), 
               sample_data(SampleSheet), 
               tax_table(taxa))
#Have a look at the phyloseq object

#We want to save this phyloseq object without losing all the work done!
#That way we can end the R session and resume without having to go through all the steps
#For this we use saveRDS https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/readRDS
#Functions to write a single R object to a file, and to restore it.
saveRDS(SixTeenS.ps, file = "SixTeenS.rds")
rm(list=ls())
dev.off()

#Read in the phyloseq object and check
SixTeenS.ps <- readRDS("SixTeenS.rds")
#Nice, all there

#This object is a bit difficult to inspect
#All the information is linked but I like to have a look at the data in one variable
#Long vs wide format
SixTeenS.long <- psmelt(SixTeenS.ps)
#What do we have here?
#What Kingdoms are represented?
unique(SixTeenS.long$Kingdom)
#And Phyla?
unique(SixTeenS.long$Phylum)
#At which level do we remove NA?
SixTeenS.dropNA <- subset_taxa(SixTeenS.ps, !is.na(Phylum))
#Did this work?
SixTeenS.dropNA.long <- psmelt(SixTeenS.dropNA)
unique(SixTeenS.dropNA.long$Phylum)
#Great, no NA at Phylum level

#Normalization
#This is going to be a hot topic! 
#https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1003531
#Why?
#When?

#How?
#I really like a DESeq2 method below. Not tested, just an example
#x <- as(otu_table(SixTeenS.dropNA), "matrix")
#x <- x+1
#deseqOTU <- normalize(x, method = "median_ratio")
#deseqOTU <- floor(deseqOTU)
#ps.deseq <- phyloseq(otu_table(as.matrix(deseqOTU), taxa_are_rows=FALSE), 
#SixTeenS.dropNA@sam_data, 
#SixTeenS.dropNA@tax_table)
library(ggplot2)
data("mpg")
#For this workshop we will use Relative Abundance
#What is Relative Abundance?
SixTeenS.dropNA.ra <- transform_sample_counts(SixTeenS.dropNA, function(OTU) OTU/sum(OTU))
plot_bar(SixTeenS.dropNA.ra)
#How would we convert this to a percentage?
SixTeenS.dropNA.ra <- transform_sample_counts(SixTeenS.dropNA, function(OTU) OTU/sum(OTU)*100)
plot_bar(SixTeenS.dropNA.ra, "SampleType", "Abundance", "Phylum")
#Why is the y-axis going to 300?
library(ggplot2)
data("mpg")
#We can always change this up
plot_bar(SixTeenS.dropNA.ra, "SampleType", "Abundance", "Phylum")
#Please remove those black borders
plot_bar(SixTeenS.dropNA.ra, "SampleType", "Abundance", "Phylum") + 
  geom_bar(stat="identity")

#Again, I like to work with the long format
SixTeenS.dropNA.ra.long <- psmelt(SixTeenS.dropNA.ra)
#We are able to subset samples and sample groups
x <- subset(SixTeenS.dropNA.ra.long, SixTeenS.dropNA.ra.long$Sample=="Khanyi_H1_16S_S24")
x <- subset(SixTeenS.dropNA.ra.long, SixTeenS.dropNA.ra.long$SampleType=="Nomusa Mkhize")
unique(x$SampleType)
unique(x$Genus)

#What is an ASV?
#What if we want to collapse all the taxonomic information to Genus level?
#We will use the pre Relative Abundance version
SixTeenS.dropNA.genus <- tax_glom(SixTeenS.dropNA, "Genus")
#Have a look at how the phyloseq object has changed
#This is again a phyloseq object and all the above steps can be repeated on the agglomorated version

#Some Alpha Diversity Indices
#What is your understanding of Alpha Diversity?
#Always a contentious topic https://www.frontiersin.org/articles/10.3389/fmicb.2019.02407/full

#Phyloseq can generate various Alpha Diversity indices
#For this we will use the SixTeenS.dropNA.genus as an example
#Which object do you think is the most appropriate?
SixTeenS.dropNA.genus.alpha <- estimate_richness(SixTeenS.dropNA.genus, measures = c("Observed", "Chao1", 
  "ACE", "Shannon", "Simpson"))
#What does this warning message mean?
#have a look at the result
SixTeenS.dropNA.genus.alpha

#Nice but what should we do now?
#We can add this data to our metadata
MetaAlpha <- cbind(SixTeenS.dropNA.genus@sam_data, SixTeenS.dropNA.genus.alpha)

#How would we test for differences between the sample types?
richness.test <- kruskal.test(MetaAlpha$Observed~MetaAlpha$SampleType)
summary(aov(MetaAlpha$Observed~MetaAlpha$SampleType))
shannon.test <- kruskal.test(MetaAlpha$Shannon~MetaAlpha$SampleType)
summary(aov(MetaAlpha$Shannon~MetaAlpha$SampleType))
simpson.test <- kruskal.test(MetaAlpha$Simpson~MetaAlpha$SampleType)
summary(aov(MetaAlpha$Simpson~MetaAlpha$SampleType))
chao1.test <- kruskal.test(MetaAlpha$Chao1~MetaAlpha$SampleType)
summary(aov(MetaAlpha$Chao1~MetaAlpha$SampleType))
ace1.test <- kruskal.test(MetaAlpha$ACE~MetaAlpha$SampleType)
summary(aov(MetaAlpha$ACE~MetaAlpha$SampleType))
pvalues <- matrix(NA, nrow=5, ncol=2)
colnames(pvalues) <- c("Indice", "pvalue")
pvalues[1,1] <- "richness"
pvalues[1,2] <- richness.test$p.value
pvalues[2,1] <- "shannon"
pvalues[2,2] <- shannon.test$p.value
pvalues[3,1] <- "simpson"
pvalues[3,2] <- simpson.test$p.value
pvalues[4,1] <- "chao1"
pvalues[4,2] <- chao1.test$p.value
pvalues[5,1] <- "ace1"
pvalues[5,2] <- ace1.test$p.value
padjust <- p.adjust(as.numeric(pvalues[,"pvalue"]), method="BH")
pvalues <- cbind(pvalues, padjust)
pvalues

#Now for some visualizations
plot_richness(SixTeenS.dropNA.genus, color = "SampleType", measures=c("Chao1", "Shannon"))
boxplot(MetaAlpha$Observed~MetaAlpha$SampleType)

#Ordination time https://joey711.github.io/phyloseq/plot_ordination-examples.html
plot_ordination(SixTeenS.dropNA.genus, ordinate(SixTeenS.dropNA.genus, method = "MDS"),
                color = "SampleType")

#What else would be of interest?
#Maybe looking at some Genera differences between sample types
#Merge the data for each sample type https://joey711.github.io/phyloseq/merge.html
#We will use the uncollapsed object

SixTeenS.dropNA.merged <- merge_samples(SixTeenS.dropNA, "SampleType")
#We can again agglomorate on Genus level
SixTeenS.dropNA.merged.genus <- tax_glom(SixTeenS.dropNA.merged, "Genus")
SixTeenS.dropNA.merged.genus.long <- psmelt(SixTeenS.dropNA.merged.genus)
head(SixTeenS.dropNA.merged.genus.long)
Hot <- subset(SixTeenS.dropNA.merged.genus.long, SixTeenS.dropNA.merged.genus.long$Sample=="Bazini Mkhize")
head(Hot)
tail(Hot)
#We have some Genera with no abundance
#Remove these
Hot <- subset(Hot, Hot$Abundance != 0)
#Same with Cold
Cold <- subset(SixTeenS.dropNA.merged.genus.long, SixTeenS.dropNA.merged.genus.long$Sample=="Zehlile Mkhize")
head(Cold)
tail(Cold)
#We have some Genera with no abundance
#Remove these
Cold <- subset(Cold, Cold$Abundance != 0)
#Makes a big difference
#Do we have Genera overlaps?
Hot.genus <- unique(Hot$Genus)
Cold.genus <- unique(Cold$Genus)
#I really like UpSet plots https://cran.r-project.org/web/packages/UpSetR/vignettes/basic.usage.html
library(UpSetR)
#Generate list input
listInput <- list(Hot=Hot.genus, Cold=Cold.genus)
#Create this angry plot
upset(fromList(listInput), order.by = "freq")
#What are we looking at?
#How do we get the unique and overlapping elements per sample type? 
#https://stat.ethz.ch/R-manual/R-devel/library/base/html/sets.html
intersect(Hot.genus, Cold.genus)
length(intersect(Hot.genus, Cold.genus))
#Looks right
#Unique Hot
setdiff(Hot.genus, Cold.genus)
length(setdiff(Hot.genus, Cold.genus))
#How would we get the unique Cold genera?

#What else would we like to do?
#...
