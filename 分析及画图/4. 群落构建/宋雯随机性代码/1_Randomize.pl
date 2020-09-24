#!/usr/bin/perl
use strict;

my $null_model = "ecosphere";
for ( my $n = 1; $n <= 1000; $n++ ) {
  print "$null_model\t$n\n";
  my @files = glob("funtion_region_t_ABC.txt");
  my %data;
  my @samples;
  foreach my $file (@files) {
    my $sen = qq`library(vegan)
			 library(picante)
			 data<-read.table(file="$file",sep="\\t",header=T,row.names=1);
			 data[is.na(data)]=0
			 null.model="$null_model"
			 PRM = matrix(0, ncol= ncol(data), nrow = nrow(data))
	         colnames(PRM)<-colnames(data)
	         rownames(PRM)<-rownames(data)
			 gamma = nrow(data)
			 alpha = colSums(data>0)
			 occur = apply(data, MARGIN=1, FUN=sum)
             if(null.model == "ecosphere"){
               for(j in 1:ncol(data)){
                  aa = data[data[,j]>0,j]
                  PRM[sample(1:gamma, alpha[j], replace=FALSE, prob=occur), j] = aa
               }
             }else if(null.model == "ecosim"){
                PRM = randomizeMatrix(data, null.model="independentswap",iterations=1000000)
             }else if(null.model == "frequency"){
                PRM = randomizeMatrix(data, null.model="frequency")
             }
			 write.table(PRM,file="prm.txt",quote=F,col.names=T,row.names=T,sep="\\t")
			 `;
    open( R, ">r.tmp" ) or die "Can't open temp file to write";
    print R "$sen";
    close(R);
    system("R --vanilla --slave <r.tmp >r.out 2>r.tmp2");
    open( PRM, "prm.txt" ) || die "#1\n";
    my $line = <PRM>;
    chomp $line;
    my @heads = split( "\t", $line );
    push( @samples, @heads );

    while (<PRM>) {
      chomp;
      my @items = split( "\t", $_ );
      for ( my $i = 1; $i <= $#items; $i++ ) {
        $data{ $items[0] }{ $heads[ $i - 1 ] } = $items[$i];
      }
    }
    close PRM;
  }

  open(OUT,">$null_model/$null_model\_$n.txt")||die"#2\n";
  print OUT "OTU\t", join( "\t", @samples ), "\n";
  foreach my $otu ( keys %data ) {
    print OUT "$otu";
    foreach my $sample (@samples) {
      print OUT "\t$data{$otu}{$sample}" if $data{$otu}{$sample};
      print OUT "\t0" if !$data{$otu}{$sample};
    }
    print OUT "\n";
  }
  close OUT;
}
