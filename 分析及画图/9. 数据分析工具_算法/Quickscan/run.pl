#!c:\perl\bin\perl -w 

# Ye written 1/26/2012:
# This program for running some statistic analysis in batch

use CGI::Carp qw(fatalsToBrowser);
use lib 'D:\wwwroot\agilent';
use NGUSER;
use LWP::Simple;
use Getopt::Long;

my ($owner, $data_file, $list_file, $str_method);
&GetOptions("owner=s"  => \$owner,                    #哈希胖箭头，键=>值
						"data=s"  => \$data_file, 
						"list=s" => \$list_file, 
						"method=s" => \$str_method);
						
my $work_dir='D:\wwwroot\agilent\combined_data';
chdir $work_dir;

(my $second = $data_file)=~s/\.txt//;              #s///查询并替换。 s/Tom/Fred/Tom被Fred替换掉； =~绑定操作符，右边的模式匹配左边的字符串
my $file_status = $second."_status.txt";
open(S,">$file_status") or die "File $file_status is wrong!";
print S"$owner: Initial checking\n";
close(S);

open(L, "<$list_file") or die "$list_file!";          #<读取
my (@samples, %groups, @trt);
while(my $line = <L>){
	chomp($line);
	$line=~tr/a-zA-Z_0-9,\.\-://cd;
	if($line){
		my($first, $rest)=split(/:/,$line);       #把字符串分割，并把结果写入数组，//中为正则表达式
		my @samp = split(/,/,$rest);
		push @samples, @samp;                     #push,数组后面添加值
		$groups{$first} = $rest;
		push @trt, $first;
	}
}
close(L);
my $str_samples = "\"".join("\",\"", @samples)."\"";   #join 把数组元素或者几个字符串通过分隔符连接成单个字符串
#die $str_samples;
my @grp;
foreach my $tr(@trt){
	my $member = $groups{$tr};
	my @mem = split(/,/,$member);
	my @a = ($tr) x (scalar @mem);
	push @grp, @a;
}
my $grp_list = "\"".join("\",\"", @grp)."\"";


my @methods = split(/,/,$str_method);

foreach my $method(@methods){
	if($method eq "summary_gene"){
		&run_gene_summary();
	}elsif($method eq "alpha"){
		&run_alpha();
	}elsif($method eq "beta"){
		&run_beta();
	}elsif($method eq "DCA"){
		&run_DCA();
	}elsif($method eq "dissimilarity"){
		&run_dissimilarity();
	}elsif($method eq "response_ratio"){
		&run_response_ratio();
	}
}

open(S,">>$file_status") or die "File $file_status is wrong!";      #>>追加
print S"$owner: Everything done\n";
close(S);

sub run_response_ratio(){
	open(S,">>$file_status") or die "File $file_status is wrong!";
	print S"$owner: Running response ratio test\n";
	close(S);
	
	#------adjust file columns-------
	my $file_adj = $second."_adj.txt";
	open(F,"<$data_file") or die "$data_file!!";
	open(O,">$file_adj") or die "$file_adj!!";
	my $no1=<F>;
	chomp($no1);
	my @first=split("\t",$no1);
	my ($row_name,@list1_col,@list2_col);
	my $i=0;
	my @list1=split(/,/,$groups{$trt[0]});
	my @list2=split(/,/,$groups{$trt[1]});
	foreach my $title(@first){
		foreach my $l(@list1){
			if($title=~/$l/){
				push @list1_col,$i;
			}
		}
		foreach my $l(@list2){
			if($title=~/$l/){
				push @list2_col,$i;
			}
		}
		$i++;
	}
	(my $title = $first[1])=~s/ /_/g;
	print O"$title\t",join("\t",@list1),"\t",join("\t",@list2),"\n";
	while(<F>){
		chomp();
		if(my @da=split("\t",$_)){
			my @aj_da;
			push @aj_da,$da[1];
			foreach my $j(@list1_col){
				push @aj_da,$da[$j]||"";
			}
			foreach my $j(@list2_col){
				push @aj_da,$da[$j]||"";
			}
			print O join("\t",@aj_da),"\n";
		}
	}
	close(F);
	close(O);
	
	#---run python program (Xiaoyang writing)---
	my $python_dir="D:\\Python26\\python";
	my $program_dir="D:\\wwwroot\\agilent\\ResponseRatio2.py";
	(my $file_in=$file_adj)=~s/\.txt//;
	my $para="filename=$file_in zero=no ci=95 column=1 treatment=2,".((scalar @list1)+1)." control=".((scalar @list1)+2).",".((scalar @list1)+(scalar @list2)+1);
	$para.=" bsig=yes";
	
	my $sen="$python_dir $program_dir $para";
	#delete all files in C:\TEMP folder
	#system("rd C:\\windows\\Temp \/s \/q");
	
	my $re=system("$sen");
	
}

sub run_dissimilarity(){
	open(S,">>$file_status") or die "File $file_status is wrong!";
	print S"$owner: Running dissimilarity test\n";
	close(S);
	
	my $dissimilarity_file = $second."_dissimilarity.csv";
	
	(my $dir = $work_dir)=~s/\\/\//g;
	my $sen=qq`
						library(vegan)
						setwd("$dir")
						fg.dat = read.table("$data_file", sep="\\t", header=T, row.names=1)
						fg.dat2 = fg.dat[,c($str_samples)]
						fg.dat2[is.na(fg.dat2)] = 0
						fg.dat2 = t(fg.dat2)
						fg.dat3 = decostand(fg.dat2,method="pa")
						grp = c($grp_list)
						grp.list = unique(grp)
						grp.num = length(grp.list)
						
						report = c()
						mrpp.bc = mrpp(fg.dat2, grp, distance = "bray")
						anosim.bc = anosim(fg.dat2, grp, distance = "bray")
						adonis.bc = adonis(fg.dat2 ~ as.factor(grp), method = "bray")
						mrpp.j = mrpp(fg.dat3, grp, distance = "jaccard")
						anosim.j = anosim(fg.dat3, grp, distance = "jaccard")
						adonis.j = adonis(fg.dat3 ~ as.factor(grp), method = "jaccard")
						report = rbind(report, c("Whole",mrpp.j\$delta, mrpp.j\$Pvalue, anosim.j\$statistic, anosim.j\$signif, adonis.j\$aov.tab[1,4], adonis.j\$aov.tab[1,6], mrpp.bc\$delta, mrpp.bc\$Pvalue, anosim.bc\$statistic, anosim.bc\$signif, adonis.bc\$aov.tab[1,4], adonis.bc\$aov.tab[1,6]))
						colnames(report) = c("Groups","MRPP.Jaccard.Delta", "MRPP.Jaccard.p", "anosim.Jaccard.R", "anosim.Jaccard.p", "adonis.Jaccard.F", "adonis.Jaccard.p","MRPP.Bray.Delta", "MRPP.Bray.p", "anosim.Bray.R", "anosim.Bray.p", "adonis.Bray.F", "adonis.Bray.p")
						
						if(grp.num>2){
							for(i in 1:(grp.num-1)){
								for(j in (i+1):grp.num){
									list1 = which(grp == grp.list[i])
									list2 = which(grp == grp.list[j])
									dat = fg.dat2[c(list1,list2),]
									#====================
									sum1 = colSums(dat) 
									valid.col = which(sum1 > 0)
									dat2 = dat[,valid.col]
									#=====================
									dat3 = decostand(dat2, "pa")
									grp1 = grp[c(list1,list2)]
									mrpp.bc = mrpp(dat2, grp1, distance = "bray")
									anosim.bc = anosim(dat2, grp1, distance = "bray")
									adonis.bc = adonis(dat2 ~ as.factor(grp1), method = "bray")
									mrpp.j = mrpp(dat3, grp1, distance = "jaccard")
									anosim.j = anosim(dat3, grp1, distance = "jaccard")
									adonis.j = adonis(dat3 ~ as.factor(grp1), method = "jaccard")
									report = rbind(report, c(paste(grp.list[i],"vs",grp.list[j]),mrpp.j\$delta, mrpp.j\$Pvalue, anosim.j\$statistic, anosim.j\$signif, adonis.j\$aov.tab[1,4], adonis.j\$aov.tab[1,6], mrpp.bc\$delta, mrpp.bc\$Pvalue, anosim.bc\$statistic, anosim.bc\$signif, adonis.bc\$aov.tab[1,4], adonis.bc\$aov.tab[1,6]))
								}
							}
						}
						write.csv(report, "$dissimilarity_file")
	`;
	my $aa = new NGUSER;
	my $re = $aa->run_R($sen);
	
}

sub run_DCA(){
	open(S,">>$file_status") or die "File $file_status is wrong!";
	print S"$owner: Running DCA\n";
	close(S);
	
	my $dca_file = $second."_dca.txt";
	my $dca_plot = $second."_dca.jpg";
	
	(my $dir = $work_dir)=~s/\\/\//g;
	my $sen=qq`
						library(vegan)
						setwd("$dir")
						fg.dat = read.table("$data_file", sep="\\t", header=T, row.names=1)
						fg.dat2 = fg.dat[,c($str_samples)]
						fg.dat2[is.na(fg.dat2)] = 0
						x.dca = decorana(t(fg.dat2))
						sink("$dca_file")
						x.re = summary(x.dca)
						print(x.re\$site.scores)
						sink()
						
						jpeg(file="$dca_plot")
						plot(x.dca, dis="site")
						grp = c($grp_list)
						grp.list = unique(grp)
						for(g in 1:length(grp.list)){
							list.name = which(grp == grp.list[g])
							points(x.re\$site.scores[list.name,1:2], col=g, pch=19)
						}
						dev.off()
	`;
	my $aa = new NGUSER;
	my $re = $aa->run_R($sen);
	
}

sub run_beta(){
	open(S,">>$file_status") or die "File $file_status is wrong!";
	print S"$owner: Calculating beta diversity for each two samples\n";
	close(S);
	
	my $beta_file = $second."_beta.txt";
	my $beta_plot = $second."_beta.jpg";
	
	(my $dir = $work_dir)=~s/\\/\//g;
	my (@grp_name,@col); 
	foreach my $tr(@trt){
		push @grp_name, ("$tr.within","$tr.outer");
		push @col, (4,5);
	}
	my $str_grp = "\"".join("\",\"",@grp_name)."\"";
	my $str_col = "\"".join("\",\"",@col)."\"";
	my $sen=qq`
			setwd("$dir")
			library("vegan")
			fg.dat = read.table("$data_file", sep="\\t", header=T, row.names=1)
			fg.dat2 = fg.dat[,c($str_samples)]
			fg.dat2[is.na(fg.dat2)] = 0
			
			jaccard.dist = vegdist(t(fg.dat2),method = "jaccard", binary=TRUE)
			bray.dist = vegdist(t(fg.dat2),method = "bray")
			jaccard.matrix = as.matrix(jaccard.dist)
			bray.matrix = as.matrix(bray.dist)
			
			report = list("Jaccard_distance"=jaccard.matrix, "Bray-Cutis_distance"=bray.matrix)
			sink("$beta_file")
			print(report)
			sink()
			
			grp = c($grp_list)
			grp.list = unique(grp)
			jaccard.rep = list()
			bray.rep = list()
			for(i in 1:length(grp.list)){
				num = which(grp == grp.list[i])
				within.group = jaccard.matrix[num, num]
				within.group.jaccard = c(within.group[upper.tri(within.group)])
				within.group = bray.matrix[num, num]
				within.group.bray = c(within.group[upper.tri(within.group)])
				
				between.group.jaccard = c(jaccard.matrix[num, -num])
				between.group.bray = c(bray.matrix[num, -num])
				
				jaccard.rep[[i*2-1]] = within.group.jaccard
				jaccard.rep[[i*2]] = between.group.jaccard
				bray.rep[[i*2-1]] = within.group.bray
				bray.rep[[i*2]] = between.group.bray
			}
			names(jaccard.rep) = c($str_grp)
			names(bray.rep) = c($str_grp)
			
			jpeg(file="$beta_plot",width=800)
			par(mfrow=c(1,2))
			boxplot(jaccard.rep, main = "Jaccard distances", col=c($str_col), horizontal=TRUE, las=1, mar=c(5,18,4,2))
			boxplot(bray.rep, main = "Bray-Cutis distances", col=c($str_col), horizontal=TRUE, las=1, mar=c(5,18,4,2))
			dev.off()
			`;
	my $aa = new NGUSER;
	my $re = $aa->run_R($sen);
	
}

sub run_alpha(){	
	open(S,">>$file_status") or die "File $file_status is wrong!";
	print S"$owner: Calculating alpha diversity for each sample\n";
	close(S);
	
	my $alpha_file = $second."_alpha.csv";
	my $alpha_plot = $second."_alpha.jpg";
	
	(my $dir = $work_dir)=~s/\\/\//g;
	my $sen=qq`
			setwd("$dir")
			library("vegan")
			fg.dat = read.table("$data_file", sep="\\t", header=T, row.names=1)
			fg.dat2 = fg.dat[,c($str_samples)]
			fg.dat2[is.na(fg.dat2)] = 0
			shannon = diversity(t(fg.dat2))
			inverse.simpson = diversity(t(fg.dat2), "inv")
			pielou.evenness = shannon/log(colSums(fg.dat2>0))
			simpson.evenness = inverse.simpson/colSums(fg.dat2>0)
			report = cbind(shannon, inverse.simpson, pielou.evenness, simpson.evenness)
			write.csv(report, "$alpha_file")
			
			jpeg(file="$alpha_plot")
			par(mfrow=c(2,2))
			grp = c($grp_list)
			for(i in 1:4){
				boxplot(report[,i] ~ grp, main = colnames(report)[i])
			}
			dev.off()
			`;
	my $aa = new NGUSER;
	my $re = $aa->run_R($sen);

}

sub run_gene_summary(){
	open(S,">>$file_status") or die "File $file_status is wrong!";
	print S"$owner: Summarizing the functional genes/categories\n";
	close(S);
	
	open(D, "<$data_file") or die "$data_file!";
	my $line = <D>;
	chomp($line);
	$line=~tr/a-zA-Z_0-9,\.\-:\t//cd;
	my @a = split(/\t/,$line);
	my $label = $a[1];
	my @samp = @a[2..$#a];
	my $str_samples = join(",",@samples);
	my $last=pop(@samp);
	if($last=~/(\w+)/){push @samp,$1;} #check the last item
	my (%t_cat,%i_gene,%e_cat,%sum_cat,%total_sum);
	while(<D>){
		chomp();
		my @line = split(/\t/,$_);
		my $gene= $line[1];
		for(my $i=2;$i<=($#samp+2);$i++){ 
			my $value=$line[$i];
			my $table_name = $samp[$i-2];
			if($value and $value=~/\d+/ and $str_samples=~/$table_name/){
				unless($t_cat{$table_name}{$gene}++){$t_cat{$table_name}{$gene} = 1;}
				unless($sum_cat{$table_name}{$gene}+=$value){$sum_cat{$table_name}{$gene}=$value;}
				$e_cat{$gene}=1;
				unless($i_gene{$table_name}++){$i_gene{$table_name}=1;}
				unless($total_sum{$table_name}+=$value){$total_sum{$table_name}=$value;}
			}
		}
	}
	close(D);
	
	my $file_num = $second."_number.txt";
	my $file_int = $second."_intensity.txt";
	
	open(N, ">$file_num") or die "$file_num is wrong!";
	open(I, ">$file_int") or die "$file_int is wrong!";
	print N"$label\t",join("\t", @samples),"\n";
	print I"$label\t",join("\t", @samples),"\n";
	foreach my $gene(sort keys %e_cat){
		print N"$gene";
		print I"$gene";
		foreach my $samp(@samples){
			if(exists($t_cat{$samp}{$gene})){
				print N"\t$t_cat{$samp}{$gene}";
				chomp($sum_cat{$samp}{$gene});
				print I"\t$sum_cat{$samp}{$gene}";
			}else{
				print N"\t0";
				print I"\t0";
			}
		}
		print N"\n";
		print I"\n";
	}
	print N"Total";
	print I"Total";
	foreach my $samp(@samples){
		chomp($samp);
		print N"\t$i_gene{$samp}";
		print I"\t$total_sum{$samp}";
	}
	print N"\n";
	print I"\n";
	close(I);
	close(N);
	
	# plot # genes and total intensity in R
	(my $dir = $work_dir)=~s/\\/\//g;
	my $plot_num = $second."_number.jpg";
	my $plot_sum = $second."_intensity.jpg";
	my $width=600;
	if($#samples>=16){$width=$#samples*40;}
	my @col;
	my $i = 1;
	foreach my $tr(@trt){
		my $member = $groups{$tr};
		my @mem = split(/,/,$member);
		my @a = ($i) x (scalar @mem);
		push @col, @a;
		$i++;
	}
	my $my_col = join(",",@col);
	my $sen=qq`
			setwd("$dir")
			num = read.table("$file_num", sep="\\t", header=T, row.names=1)
			total_num = num[nrow(num), ]
			jpeg(file="$plot_num",width=$width,height=300)
			bplot = barplot(c(t(total_num)), main="Numbers of detected genes", xlab="samples",names.arg=colnames(num),cex.names=0.6, col=c($my_col))
			mtext(3, at=bplot, text = c(t(total_num)), cex=0.6)
			dev.off()
			
			sum = read.table("$file_int", sep="\\t", header=T, row.names=1)
			total_sum = trunc(sum[nrow(sum), ])
			jpeg(file="$plot_sum",width=$width,height=300)
			bplot = barplot(c(t(total_sum)), main="Total intensity", xlab="samples",names.arg=colnames(sum),cex.names=0.6, col=c($my_col))
			mtext(3, at=bplot, text = c(t(total_sum)), cex=0.6)
			dev.off()`;
	my $aa = new NGUSER;
	my $re = $aa->run_R($sen);
}

