
#install.packages("mixOmics", repos="http://R-Forge.R-project.org")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
# The following initializes usage of Bioc devel
#BiocManager::install(version='devel')
BiocManager::install("mixOmics")

library(mixOmics)
library(ggplot2)

getwd()
setwd("E:/桌面/R script 2017/mixOmics偏最小二乘判别分析（PLS-DA）/")
##?????ļ?
#??ˮƽ???ȱ?
phylum <- read.delim('phylum_table.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
phylum <- data.frame(t(phylum))

#?????????ļ?
group <- read.delim('group.txt', sep = '\t', stringsAsFactors = FALSE)

##PLS-DA ????
#??????ˮƽ???ȱ���ֻչʾǰ 3 ????????
phylum <- phylum[group$names, ]
plsda_result <-plsda(phylum, group$group, ncomp = 3)

#??Ҫ?鿴????
plsda_result
#??
names(plsda_result)
plsda_result$explained_variance

#?鿴??????????��
plsda_result$explained_variance$X
#?鿴????????????
plsda_result$variates$X
#?鿴ϸ??????Ⱥ????????
plsda_result$loadings$X

#ʹ?? plotIndiv() ???? PLS-DA ????????
plotIndiv(plsda_result, ind.names = TRUE, style = 'ggplot2')
plotIndiv(plsda_result, ind.names = TRUE, style = '3d')

#??ȡ??????????��??ǰ��?ᣩ
plsda_result_eig <- {plsda_result$explained_variance$X}[1:2]

#??ȡ?????????꣨ǰ��?ᣩ
sample_site <- data.frame(plsda_result$variates)[1:2]

#Ϊ?????????????ӷ?????Ϣ
sample_site$names <- rownames(sample_site)
names(sample_site)[1:2] <- c('plsda1', 'plsda2')
sample_site <- merge(sample_site, group, by = 'names', all.x = TRUE)

#??ѡ???????????? PLS-DA ????????
write.table(sample_site, 'plsda_sample.txt', row.names = FALSE, sep = '\t', quote = FALSE)

#ʹ?? ggplot2 ?򵥻??? PLS-DA ????ͼ
plsda_plot <- ggplot(sample_site, aes(plsda1, plsda2, color = group, label = names)) +
geom_point(size = 1.5, alpha = 0.6) + 
stat_ellipse(show.legend = FALSE) +	#???? 95% ??????Բ
scale_color_manual(values = c('#1D7ACC', '#F67433', '#00815F')) +
theme(panel.grid = element_line(color = 'grey50'), panel.background = element_rect(color = 'black', fill = 'transparent')) + 
theme(legend.title = element_blank(), legend.key = element_rect(fill = 'transparent')) +
labs(x = paste('PLS-DA axis1 ( explained variance ', round(100 * plsda_result_eig[1], 2), '% )', sep = ''), y = paste('PLS-DA axis2 ( explained variance ', round(100 * plsda_result_eig[2], 2), '% )', sep = ''))

plsda_plot
#ggsave('plsda_plot.pdf', plsda_plot, width = 6, height = 5)
ggsave('plsda_plot.png', plsda_plot, width = 6, height = 5)


?plsda

plsda(X,
      Y,
      ncomp = 2,
      scale = TRUE,
      mode = c("regression", "canonical", "invariant", "classic"),
      tol = 1e-06,
      max.iter = 100,
      near.zero.var = FALSE,
      logratio="none",  # one of "none", "CLR"
      multilevel=NULL,
      all.outputs = TRUE)
#X:OTU
#Y:????
#ncomp:չʾǰ???ᡣĬ??2
#scale:??׼??
#mode:???????㷨
#max.iter: ????????

## First example
data(breast.tumors)
X <- breast.tumors$gene.exp
Y <- breast.tumors$sample$treatment

plsda.breast <- plsda(X, Y, ncomp = 2)
plotIndiv(plsda.breast, ind.names = TRUE, ellipse = TRUE, legend = TRUE)


## Second example
data(liver.toxicity)
X <- liver.toxicity$gene
Y <- liver.toxicity$treatment[, 4]

plsda.liver <- plsda(X, Y, ncomp = 2)
plotIndiv(plsda.liver, ind.names = Y, ellipse = TRUE, legend =TRUE)
