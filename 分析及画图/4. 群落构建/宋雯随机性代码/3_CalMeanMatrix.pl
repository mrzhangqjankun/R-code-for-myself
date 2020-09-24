#!/usr/bin/perl
use strict;

my @pmfiles=glob("data/ecosphere_*_bray_pm_tax.txt");
my $obfile="data/bray_ob_tax.txt";
my $outfile="ecosphere_bray_pm_tax.txt";

my %data;
foreach my $pmfile(@pmfiles){
  open(FILE,"$pmfile")||die"#1\n";
  my $line=<FILE>;
  chomp $line;
  my @heads=split("\t",$line);
  while(<FILE>){
	chomp;
	my @items=split("\t",$_);
	for(my $i=1;$i<=$#items;$i++){
	  $data{$items[0]}{$heads[$i-1]}+=$items[$i];
	  #print "$data{$items[0]}{'UYUM11'}" if $heads[$i-1] eq "UYUM11";
	}
  }
  close FILE;
}

open(OB,"$obfile")||die"#2\n";
my $line=<OB>;
chomp $line;
my @heads=split("\t",$line);
my @samples=@heads[1..$#heads];
close OB;

open(OUT,">$outfile")||die"#3\n";
print OUT "\t",join("\t",@samples),"\n";
foreach my $sample1(@samples){
  print OUT "$sample1";
  foreach my $sample2(@samples){
	print OUT "\t",$data{$sample1}{$sample2}/scalar(@pmfiles);
  }
  print OUT "\n";
}
close OUT;
