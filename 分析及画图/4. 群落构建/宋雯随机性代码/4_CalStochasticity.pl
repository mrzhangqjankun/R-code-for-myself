#!/usr/bin/env perl
use strict;
#use Statistics::DependantTTest;
use List::Util qw(sum);

my $distance_method = "bray";

my %ocean;
open(OCEAN,"sample_ocean.txt")||die"#1\n";
while(<OCEAN>){
  chomp;
  my @items=split("\t",$_);
  $ocean{$items[0]}=$items[1];
}
close OCEAN;

my ( %hash_ob, %hash_pm );
open( OB, "data/$distance_method\_ob_tax.txt" ) || die "#can not open similarity_ob.txt\n";
my $line = <OB>;
chomp $line;
my @heads = split( /\s/, $line );
my $line_num = 1;
while (<OB>) {
  chomp;
  my @items = split( /\s/, $_ );
  for ( my $i = $line_num + 1; $i <= $#heads; $i++ ) {
    $hash_ob{ $items[0] }{ $heads[$i] } = $items[$i];
  }
  $line_num++;
}
close OB;

open( PM, "data/ecosphere_$distance_method\_pm_tax.txt" ) || die "#can not open similarity_pm.txt\n";
$line = <PM>;
chomp $line;
@heads = split( /\s/, $line );
$line_num = 1;
while (<PM>) {
  chomp;
  my @items = split( /\s/, $_ );
  for ( my $i = $line_num + 1; $i <= $#heads; $i++ ) {
    $hash_pm{ $items[0] }{ $heads[$i] } = $items[$i];
  }
  $line_num++;
}
close PM;

my ( @array_ob, @array_pm );
my (%array_ob,%array_pm);
foreach my $key1 ( keys %hash_ob ) {
  foreach my $key2 ( keys %{ $hash_ob{$key1} } ) {
    push( @array_ob, $hash_ob{$key1}{$key2} );
    push( @array_pm, $hash_pm{$key1}{$key2} );
	##volumn
	$key1=~/TARA_\d+_(.*?)\_/;
	my $volumn1=$1;
	$key2=~/TARA_\d+_(.*?)\_/;
	my $volumn2=$1;
    push( @{$array_ob{$volumn1}}, $hash_ob{$key1}{$key2} ) if $volumn1 eq $volumn2;
    push( @{$array_pm{$volumn1}}, $hash_pm{$key1}{$key2} ) if $volumn1 eq $volumn2;
	##ocean
	my $ocean1=$ocean{$key1};
	my $ocean2=$ocean{$key2};
	#print "$key1\t$ocean1\t$key2\t$ocean2\n";
    push( @{$array_ob{$ocean1}}, $hash_ob{$key1}{$key2} ) if $ocean1 eq $ocean2;
    push( @{$array_pm{$ocean1}}, $hash_pm{$key1}{$key2} ) if $ocean1 eq $ocean2;
  }
}
print "$#array_ob\t$#array_pm\n";
#my $p_value = &paired_ttest( \@array_ob, \@array_pm );
my $ob_mean = sum(@array_ob) / scalar(@array_ob);
my $ob_std  = &stdev( \@array_ob );
my $pm_mean = sum(@array_pm) / scalar(@array_pm);
my $pm_std  = &stdev( \@array_pm );
print "ob:$ob_mean+-$ob_std\n";
print "pm:$pm_mean+-$pm_std\n";
my @es;

for ( my $i = 0; $i <= $#array_ob; $i++ ) {
  push( @es, log( $array_ob[$i] ) - log( $array_pm[$i] ) )
    if $array_ob[$i] != 0 and $array_pm[$i] != 0;
}
my $es_mean = sum(@es) / scalar(@es);
my $es_std  = &stdev( \@es );
my @ses;
for ( my $i = 0; $i <= $#array_ob; $i++ ) {
  push( @ses, ( $array_ob[$i] - $array_pm[$i] ) / $pm_std );
}
my $ses_mean = sum(@ses) / scalar(@ses);
my $ses_std  = &stdev( \@ses );

my ($st_ratio_a, $st_ratio_b, $mst_ratio_a, $mst_ratio_b)=&CalStochasticity(\@array_ob,\@array_pm);
my @st_ratio_a=@$st_ratio_a;
my @st_ratio_b=@$st_ratio_b;
my @mst_ratio_a=@$mst_ratio_a;
my @mst_ratio_b=@$mst_ratio_b;

my (%st_ratio,%st_ratio,%mst_ratio,%mst_ratio);
foreach my $group(keys %array_ob){
  my @array_ob=@{$array_ob{$group}};
  my @array_pm=@{$array_pm{$group}};
  my ($st_ratio_a, $st_ratio_b, $mst_ratio_a, $mst_ratio_b)=&CalStochasticity(\@array_ob,\@array_pm);
  @{$st_ratio{$group}}=(@$st_ratio_a,@$st_ratio_b);
  @{$mst_ratio{$group}}=(@$mst_ratio_a,@$mst_ratio_b);
}

my $st_ratio_mean =
  ( sum(@st_ratio_a) + sum(@st_ratio_b) ) / ( scalar(@st_ratio_a) + scalar(@st_ratio_b) );
my @st_ratio = ( @st_ratio_a, @st_ratio_b );
my $st_ratio_std = &stdev( \@st_ratio );
my $mst_ratio_mean =
  ( sum(@mst_ratio_a) + sum(@mst_ratio_b) ) / ( scalar(@mst_ratio_a) + scalar(@mst_ratio_b) );
my @mst_ratio = ( @mst_ratio_a, @mst_ratio_b );
my $mst_ratio_std = &stdev( \@mst_ratio );
my %stochasticity;
#$stochasticity{"pvalue"}         = $p_value;
$stochasticity{"ob_mean"}        = $ob_mean;
$stochasticity{"ob_std"}         = $ob_std;
$stochasticity{"pm_mean"}        = $pm_mean;
$stochasticity{"pm_std"}         = $pm_std;
$stochasticity{"es_mean"}        = $es_mean;
$stochasticity{"es_std"}         = $es_std;
$stochasticity{"ses_mean"}       = $ses_mean;
$stochasticity{"ses_std"}        = $ses_std;
$stochasticity{"st_ratio_mean"}  = $st_ratio_mean;
$stochasticity{"st_ratio_std"}   = $st_ratio_std;
$stochasticity{"mst_ratio_mean"} = $mst_ratio_mean;
$stochasticity{"mst_ratio_std"}  = $mst_ratio_std;
open( OUT, ">>stochasticity_tax_test.txt" ) || die "#can not open stochasticity_tax.txt\n";
print OUT "###################\n";
print OUT "distance method:$distance_method\n";

#print OUT "null model:$null_model\n";
#print OUT "P_value:$p_value\n";
print OUT "ob_mean:$ob_mean\n";
print OUT "ob_std:$ob_std\n";
print OUT "pm_mean:$pm_mean\n";
print OUT "pm_std:$pm_std\n";
print OUT "es_mean:$es_mean\n";
print OUT "es_std:$es_std\n";
print OUT "ses_mean:$ses_mean\n";
print OUT "ses_std:$ses_std\n";
print OUT "st_ratio_mean:$st_ratio_mean\n";
print OUT "st_ratio_std:$st_ratio_std\n";
print OUT "mst_ratio_mean:$mst_ratio_mean\n";
print OUT "mst_ratio_std:$mst_ratio_std\n";
print OUT "distance method:$distance_method\n";
print OUT "###################\n";
close OUT;
############
open( OUT, ">>stochasticity_tax_values_test.txt" ) || die "#can not open stochasticity_tax.txt\n";
print OUT "$distance_method\_st_ratio\t",  join( "\t", @st_ratio ), "\n";
print OUT "$distance_method\_mst_ratio\t", join( "\t", @mst_ratio ), "\n";
close OUT;
open( OUT, ">>stochasticity_tax_values_group_st_test.txt" ) || die "#can not open stochasticity_tax.txt\n";
#foreach my $group(keys %st_ratio){
foreach my $group("SRF","MES","DCM","NPO","SPO","NAO","SAO","MS","RS","IO","SO"){
  print OUT "$group $distance_method\_st_ratio\t", join("\t",@{$st_ratio{$group}}),"\n";
}
close OUT;
open( OUT, ">>stochasticity_tax_values_group_mst_test.txt" ) || die "#can not open stochasticity_tax.txt\n";
#foreach my $group(keys %mst_ratio){
foreach my $group("SRF","MES","DCM","NPO","SPO","NAO","SAO","MS","RS","IO","SO"){
  print OUT "$group $distance_method\_mst_ratio\t", join("\t",@{$mst_ratio{$group}}),"\n";
}
close OUT;

sub stdev() {
  my $ar       = shift;
  my $elements = scalar @$ar;
  my $mean     = sum(@$ar) / $elements;
  my $sumsq    = 0;
  foreach (@$ar) {
    $sumsq += ( ( $_ - $mean )**2 );
  }
  return sqrt( $sumsq / ( $elements - 1 ) );
}

sub paired_ttest() {
  my ( $array1, $array2 ) = @_;
  my @array1 = @$array1;
  my @array2 = @$array2;
  my $t_test = new Statistics::DependantTTest;
  $t_test->load_data( 'array1', @array1 );
  $t_test->load_data( 'array2', @array2 );
  my ( $t_value, $deg_freedom ) = $t_test->perform_t_test( 'array1', 'array2' );
  my ($p_value) = Statistics::Distributions::tprob( $deg_freedom, $t_value );
  return $p_value;
}

sub CalStochasticity() {
  my ( $array_ob, $array_pm ) = @_;
  my @array_ob = @$array_ob;
  my @array_pm = @$array_pm;
  my ( @st_ratio_a,     @st_ratio_b );
  my ( @mst_ratio_a, @mst_ratio_b );
  for ( my $i = 0; $i <= $#array_ob; $i++ ) {
    if ( $array_ob[$i] >= $array_pm[$i] ) {
      if ( $array_ob[$i] != 0 ) {
        push( @st_ratio_a, $array_pm[$i] / $array_ob[$i] );
        push( @mst_ratio_a,
          ( $array_pm[$i] * ( 1 - $array_ob[$i] ) ) / ( $array_ob[$i] * ( 1 - $array_pm[$i] ) )
        );
      }
    }
    else {
      if ( $array_ob[$i] != 0 and $array_pm[$i] != 0 ) {
        push( @st_ratio_b, ( 1 - $array_pm[$i] ) / ( 1 - $array_ob[$i] ) );
        push( @mst_ratio_b,
          ( $array_ob[$i] * ( 1 - $array_pm[$i] ) ) / ( $array_pm[$i] * ( 1 - $array_ob[$i] ) )
        );
      }
    }
  }
  return(\@st_ratio_a, \@st_ratio_b, \@mst_ratio_a, \@mst_ratio_b);
}
