#!/usr/bin/perl
use strict;
#use Statistics::DependantTTest;
use List::Util qw(sum);

my $otufile  = "miTAG_profile_36235_new.txt";
&CalOBSimilarity($otufile,"bray");
##taxonomic
foreach my $distance_method ( "bray" ) {#bray, sorensen, jaccard
  foreach my $null_model ("ecosphere") {#ecosphere, ecosim,
    &CalTaxonomicSimilarity( $distance_method, "group", $null_model );
  }
}

#phylogenetic: shuffle tree
#&CalStochasticityPhylogenetic($otufile,$treefile,$null_model,$shuffle);

sub CalOBSimilarity(){
  my($otufile,$distance_method)=@_;
  my $distance_method1 = $distance_method;
  my $transfer         = "binary" if $distance_method eq "sorensen";
  $distance_method1 = "bray"   if $distance_method eq "sorensen";
    my $sen = qq`
	library(vegan)
	library(picante)
	transfer="$transfer"
	distance.method="$distance_method1"
	data<-read.table(file="$otufile",sep="\\t",header=T,row.names=1)
	data[is.na(data)]=0
	##transfer data if defined
	if(transfer == "binary"){
	  data = decostand(data, method="pa")  # only work for presence/absence data
	  PRM = decostand(PRM, method="pa")  # only work for presence/absence data
	}else if(transfer == "integer"){
	  data = round(data, digits=0)
	  PRM = round(PRM, digits=0)
	}
	##calculate observed beta dissimilarity and similarity
	beta.dist = vegdist(t(data),method = distance.method)
    similarity.ob = 1 - beta.dist
	##write similarity values to files
	similarity.ob<-as.matrix(similarity.ob)
	write.table(similarity.ob,file="data/$distance_method\_ob_tax.txt",quote=F,col.names=T,row.names=T,sep="\\t")
  `;
    open( R, ">r.tmp" ) or die "Can't open temp file to write";
    print R "$sen";
    close(R);
    system("R --vanilla --slave <r.tmp >r.out 2>r.tmp2");
}

sub CalTaxonomicSimilarity() {
  my ($distance_method, $gamma_method, $null_model ) = @_;
  my %stochasticity;
  my $distance_method1 = $distance_method;
  my $transfer         = "binary" if $distance_method eq "sorensen";
  $distance_method1 = "bray"   if $distance_method eq "sorensen";
  for ( my $i = 1; $i <= 1000; $i++ ) {
	my $prmfile="data/$null_model\_$i.txt";
	print "$prmfile\n";
    my $sen = qq`
	library(vegan)
	library(picante)
	gamma.method="$gamma_method"
	null.model="$null_model"
	transfer="$transfer"
	distance.method="$distance_method1"
	data<-read.table(file="$otufile",sep="\\t",header=T,row.names=1)
	data[is.na(data)]=0
	PRM<-read.table(file="$prmfile",sep="\\t",header=T,row.names=1)
	PRM[is.na(PRM)]=0
	##transfer data if defined
	if(transfer == "binary"){
	  data = decostand(data, method="pa")  # only work for presence/absence data
	  PRM = decostand(PRM, method="pa")  # only work for presence/absence data
	}else if(transfer == "integer"){
	  data = round(data, digits=0)
	  PRM = round(PRM, digits=0)
	}
	##delete empty rows
	if(gamma.method == "group"){
      rsum = rowSums(data)
      temp = which(rsum==0)
      if(length(temp)!=0){
	    data = data[-temp,]
	  }
    }
    ##calculate permutated beta dissimilarity and similarity
	dist_pm = vegdist(t(PRM),method = distance.method)
    similarity.pm = 1- dist_pm
	##write similarity values to files
	similarity.pm<-as.matrix(similarity.pm)
	write.table(similarity.pm,file="data/$null_model\_$i\_$distance_method\_pm_tax.txt",quote=F,col.names=T,row.names=T,sep="\\t")
  `;
    open( R, ">r.tmp" ) or die "Can't open temp file to write";
    print R "$sen";
    close(R);
    system("R --vanilla --slave <r.tmp >r.out 2>r.tmp2");
  }
}

