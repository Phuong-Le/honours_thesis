library(ggplot2)
library(magrittr)
library(ggpubr)
library(GWASTools)

mut_distribution = function(maf, pk, chrom, freq=F, lower=NA, upper=NA) {
  # hypersensitivity region
  open_start = pk$chromStart[pk$chrom==paste('chr',chrom, sep = '')]
  open_end = pk$chromEnd[pk$chrom==paste('chr',chrom, sep = '')]

  # centromeres
  data("centromeres.hg19")

  # plotting
  if (freq == F) {
    p = ggplot(data = maf, mapping = aes(x=Start_position)) +
      geom_vline(xintercept = centromeres.hg19$left.base[centromeres.hg19$chrom==chrom], linetype='dotted') +
      geom_density(bw='nrd', colour = '#3A5431') +
      annotate('rect', xmin=open_start, xmax = open_end, ymin=-5*10^(-10), ymax = -10^(-10), size=0.01, colour = '#3A5431') +
      xlab(paste('Base positions on chromosome ', chrom, sep='')) +
      ylab('mutation denstity') +
      theme_gray()
  } else {
    p = ggplot(data = maf, mapping = aes(x=Start_position)) +
      geom_vline(xintercept = centromeres.hg19$left.base[centromeres.hg19$chrom==chrom], linetype='dotted') +
      geom_density(aes(y=..count..),bw='nrd', colour = '#3A5431') +
      annotate('rect', xmin=open_start, xmax = open_end, ymin=-5*10^(-10), ymax = -10^(-10), size=0.01, colour = '#3A5431') +
      xlab(paste('Base positions on chromosome ', chrom, sep='')) +
      ylab('mutation denstity') +
      theme_gray()
  }

  if (is.na(lower) == F | is.na(upper) == F) {
    p = p + xlim(c(lower,upper))
  }

  return(p)
}


chrom = '12'
# maf
path_chrom = paste('/Users/phuongle/Documents/repos/PhuongData/processed/pcawg/maf_chrom',chrom,'.tsv.gz', sep = '')
muts = read.delim(path_chrom, sep='\t', header=T)

# skin melanoma
# encode
disease = 'Skin-Melanoma'
pk_skin = load_pk('/Users/phuongle/Documents/repos/PhuongData/raw/encode/MelanocyteDukeDNaseSeq.pk')
mut_skin = muts[muts$Project_Code == disease,]
p_skin = mut_distribution(mut_skin,pk_skin,chrom = chrom, freq = F) +
  # ggtitle('(a) Skin-Melanoma') +
  theme(axis.title=element_text(size=22),
        axis.text = element_text(size=20))
p_skin
# liver hcc
# encode
disease = 'Liver-HCC'
pk_liver = load_pk('/Users/phuongle/Documents/repos/PhuongData/raw/encode/HepatocytesDukeDNaseSeq.pk')
mut_liver = muts[muts$Project_Code == disease,]
p_liver = mut_distribution(mut_liver,pk_liver,chrom = chrom) +
  # ggtitle('(b) Liver-HCC') +
  theme(axis.title=element_text(size=22),
        axis.text = element_text(size=20))
p_liver

# medullo
disease = 'CNS-Medullo'
pk_medullo = load_pk('/Users/phuongle/Documents/repos/PhuongData/raw/encode/CerebellumDukeDNaseSeq.pk')
mut_medullo = muts[muts$Project_Code == disease,]
p_medullo=mut_distribution(mut_medullo,pk_medullo,chrom = chrom) +
  # ggtitle('(c) CNS-Medullo') +
  theme(axis.title=element_text(size=22),
        axis.text = element_text(size=20))
p_medullo

# piloastro
disease = 'CNS-PiloAstro'
pk_piloastro = load_pk('/Users/phuongle/Documents/repos/PhuongData/raw/encode/HAc-DS14765.peaks.fdr0.01.hg19.bed')
mut_piloastro = muts[muts$Project_Code == disease,]
p_piloastro = mut_distribution(mut_piloastro,pk_piloastro,chrom = chrom) +
  # ggtitle('(d) CNS-PiloAstro') +
  theme(axis.title=element_text(size=22),
        axis.text = element_text(size=20))
p_piloastro

# kidney
disease = 'Kidney-RCC'
pk = load_pk("/Users/phuongle/Documents/repos/PhuongData/raw/encode/RPTEC-DS14061.peaks.fdr0.01.hg19.bed")
mut = muts[muts$Project_Code == disease,]
pk.plot = mut_distribution(mut,pk,chrom = chrom) +
  # ggtitle(disease) +
  theme(axis.title=element_text(size=22),
        axis.text = element_text(size=20))
pk.plot
# mut_distribution(mut,pk,chrom = chrom,lower=0, upper = 2.5*10^7) + ggtitle(disease) +theme(plot.title = element_text(hjust = 0.5, size = 9))

# leukemia
disease = 'Lymph-CLL'
disease = 'Lymph-BNHL'
pk = load_pk("/Users/phuongle/Documents/repos/PhuongData/raw/encode/CD20_BcellDukeDNaseSeq.pk")
mut = muts[muts$Project_Code == disease,]
pk.plot = mut_distribution(mut,pk,chrom = chrom) +
  # ggtitle(disease) +
  theme(axis.title=element_text(size=22),
        axis.text = element_text(size=20))
pk.plot
# mut_distribution(mut,pk,chrom = chrom,lower=0, upper = 2.5*10^7) + ggtitle(disease) +theme(plot.title = element_text(hjust = 0.5, size = 9))

# panc-adeno
disease = 'Panc-AdenoCA'
pk = load_pk("/Users/phuongle/Documents/repos/PhuongData/raw/encode/HPDE6DukeDNaseSeq.pk")
mut = muts[muts$Project_Code == disease,]
pk.plot = mut_distribution(mut,pk,chrom = chrom) +
  # ggtitle(disease) +
  theme(axis.title=element_text(size=22),
        axis.text = element_text(size=20))
pk.plot

# panc-endocrine
disease = 'Panc-Endocrine'
pk = load_pk("/Users/phuongle/Documents/repos/PhuongData/raw/encode/Pancreatic_isletsDukeDNaseSeq.pk")
mut = muts[muts$Project_Code == disease,]
pk.plot = mut_distribution(mut,pk,chrom = chrom) +
  # ggtitle(disease) +
  theme(axis.title=element_text(size=22),
        axis.text = element_text(size=20))
pk.plot

# prost
disease = 'Prost-AdenoCA'
pk = load_pk("/Users/phuongle/Documents/repos/PhuongData/raw/encode/RWPEDukeDNaseSeq.pk")
mut = muts[muts$Project_Code == disease,]
pk.plot = mut_distribution(mut,pk,chrom = chrom) +
  # ggtitle(disease) +
  theme(axis.title=element_text(size=22),
        axis.text = element_text(size=20))
pk.plot

# breast
disease = 'Breast-AdenoCa'
pk = load_pk("/Users/phuongle/Documents/repos/PhuongData/raw/encode/HMECDukeDNaseSeq.pk")
mut = muts[muts$Project_Code == disease,]
pk.plot = mut_distribution(mut,pk,chrom = chrom) +
  # ggtitle(disease) +
  theme(axis.title=element_text(size=22),
        axis.text = element_text(size=20))
pk.plot

# bone
disease = 'Bone-Osteosarc'
pk = load_pk("/Users/phuongle/Documents/repos/PhuongData/raw/encode/OsteoblastDukeDNaseSeq.pk")
mut = muts[muts$Project_Code == disease,]
pk.plot = mut_distribution(mut,pk,chrom = chrom) +
  # ggtitle(disease) +
  theme(axis.title=element_text(size=22),
        axis.text = element_text(size=20))
pk.plot

