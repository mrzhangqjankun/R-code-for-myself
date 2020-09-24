##2019.8.28

##ampvis2 一个用于分析和可视化16S rRNA扩增子数据的R包 

##https://mp.weixin.qq.com/s/y_Za_6r3w-G8kDX5v-73YQ


# install.packages("remotes")
# remotes::install_github("MadsAlbertsen/ampvis2")
library("ampvis2")




myotutable <- read.csv("example_otutable.csv", check.names = FALSE)
mymetadata <- read.csv("example_metadata.csv", check.names = FALSE)

### 数据结构
library(ampvis2)
d <- amp_load(otutable = myotutable,
              metadata = mymetadata)
d
data("MiDAS")
MiDAS
MiDAS$refseq


## 数据过滤
MiDASsubset <- amp_subset_samples(MiDAS, Plant %in% c("Aalborg West", "Aalborg East"))
MiDASsubset <- amp_subset_samples(MiDAS, Plant %in% c("Aalborg West", "Aalborg East") & !SampleID %in% c("16SAMP-749"), minreads = 10000)
MiDAS_Chloroflexi_Actinobacteria <- amp_subset_taxa(MiDAS, tax_vector=c("p__Chloroflexi", "p__Actinobacteria"))



## 试试作者特别推荐的热图
# Load example data
data("AalborgWWTPs")

# 类似于phyloseq的封装方式
amp_heatmap(AalborgWWTPs, group_by = "Plant")

# Heatmap of 20 most abundant Genera (by mean) grouped by WWTP, split by Year,
# values not plotted for visibility, phylum name added and colorscale adjusted manually
amp_heatmap(AalborgWWTPs,
            group_by = "Plant",
            facet_by = "Year",
            plot_values = FALSE,
            tax_show = 20,
            tax_aggregate = "Genus",
            tax_add = "Phylum",
            color_vector = c("white", "red"),
            plot_colorscale = "sqrt",
            plot_legendbreaks = c(1, 5, 10)
)

# Heatmap with known functional information about the Genera shown to the right
# By default this information is retrieved directly from midasfieldguide.org
# but you can provide your own with the function_data argument as shown in the
# textmap
amp_heatmap(AalborgWWTPs,
            group_by = "Plant",
            tax_aggregate = "Genus",
            plot_functions = TRUE,
            functions = c("PAO", "GAO", "AOB", "NOB")
)

# A raw text version of the heatmap can be printed or saved as a data frame with textmap = TRUE.
# Notice the function_data is now retrieved from the MiF data frame
textmap <- amp_heatmap(AalborgWWTPs,
                       group_by = "Plant",
                       tax_aggregate = "Genus",
                       plot_functions = TRUE,
                       function_data = MiF,
                       functions = c("PAO", "GAO", "AOB", "NOB"),
                       textmap = TRUE
)
textmap




## 箱线图
amp_boxplot(MiDASsubset)


amp_boxplot(MiDASsubset,
            group_by = "Period",
            tax_show = 5,
            tax_add = "Phylum")


##作者重点推出的排序
amp_ordinate(MiDASsubset, 
             type = "pcoa",
             distmeasure = "bray",
             sample_color_by = "Plant",
             sample_colorframe = TRUE,
             sample_colorframe_label = "Plant") + theme(legend.position = "blank")

amp_ordinate(MiDASsubset, 
             type = "pcoa",
             distmeasure = "bray",
             sample_color_by = "Plant",
             sample_colorframe_label = "Plant", 
             sample_trajectory = "Date", 
             sample_trajectory_group = "Plant")

ordinationresult <- amp_ordinate(MiDASsubset, 
                                 type = "CCA",
                                 constrain = "Period",
                                 transform = "Hellinger",
                                 sample_color_by = "Period",
                                 sample_shape_by = "Plant",
                                 sample_colorframe = TRUE,
                                 sample_colorframe_label = "Period",
                                 detailed_output = TRUE)
ordinationresult$plot














