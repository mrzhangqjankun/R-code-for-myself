package NGUSER;

# Copyright Ye Deng from IEG OU
# Revised Feb-2013 for GeoChip5

$VERSION = 0.02; 


use CGI qw(:standard);
#use CGI::Session;
use DBI;
use Statistics::Basic::Correlation;


sub new() {
	my $self=bless {},shift;
	return $self;
}

sub del_old_files(){
	my ($object,$sec,$rootdir)=@_;
	my $file_dir=$rootdir."\\combined_data";
	opendir(DIR,$file_dir);
	my @dirs=readdir(DIR);
	foreach my $file(@dirs){
		if($file=~/^(\d+)/){
			my $old_sec=$1;
			if(($sec-$old_sec)>100000){
				my $file_dir=$rootdir."\\combined_data\\$file";
				unlink $file_dir;
			}
		}
	}
	closedir(DIR);	
}

sub run_R(){
	my ($object,$sen)=@_;
	my $second=time();
	my $temp=$second."_";
	open(File,">$temp") or die "Can't open temp file to write";
	print File"$sen";
	close(File);
	my $temp2=$second."_out";
	my $R_dir='C:\R\R-2-11-0\bin\Rterm.exe';		
	my $com_sen="$R_dir --vanilla --slave <$temp >$temp2 2>temp_rep";
	#print "<p>$com_sen</p>";
	my $try=system($com_sen);
	#print "$try";
	if(defined($try) and $try==0){
		open(File,"<$temp2");
		while(my $line=<File>){
			$ret.=$line;
		}
		if(!$ret or $ret eq ""){
			$ret="Not enough finite observations!!";
		}
		close(File);
	}else{
		open(F,"<temp_rep");
		while(<F>){
			(my $a = $_)=~s/\n/<br>/mg;
			print $a;
		}
		close(F);
		exit;
	}
	#unlink($temp);
	#unlink($temp2);
	return $ret;
}

sub trans_date() {
	my ($object,$se)=@_;
	my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst)=localtime($se);
	$year+=1900;
	$mon++;
	my $date="$mon-$mday-$year $hour:$min";
	return $date;
}

sub trans_setting(){
	my ($object,$setting,$option)=@_;
	my ($clean_method,$order,$normalize_method);
	my ($i_clean,$clean_para,$i_normalize,$normal_para);
	$i_normalize=0;
	if($setting=~/clean_method=(\d);(.*)order=(.*?);normalize_method=(\d);(.*)/){
		($i_clean,$clean_para,$order,$i_normalize,$normal_para)=($1,$2,$3,$4,$5);
	}elsif($setting=~/clean_method=(\d);(.*)multiple normalization steps;(.*)/){
		#clean_method=3;dele_flag=1,3;dele_snr=2;multiple normalization steps;1st_nor_method:using the universal standards on Cy3;2nd_nor_method:normalize by the greatest mean value of Cy5;2nd_order:after cleaning bad spots;3rd_nor_method:normalize by the average Cy3 across samples;3rd_order:before cleaning bad spots;
		($i_clean,$clean_para)=($1,$2);
		$normalize_method="multiple steps";
		if(defined($option) and $option eq "detail"){
			$normalize_method="multiple steps:<br>$3";
		}
	}
	if($i_clean==1){
			$clean_para=~/dele_flag=(.*?);/;
			$flag1=$1;
			$clean_method="deleted flag $flag1";
	}elsif($i_clean==2){
			$clean_para=~/dele_snr=(.*?);/;
			$flag1=$1;
			$clean_method="deleted SNR<$flag1";
	}elsif($i_clean==3){
			$clean_para=~/dele_flag=(.*?);dele_snr=(.*?);/;
			$flag1=$1;
			$flag2=$2;
			$clean_method="deleted flag $flag1 and delete SNR<$flag2";
	}elsif($i_clean==4){
			$clean_para=~/dele_sbr=(.*?);/;
			$flag1=$1;
			$clean_method="deleted SBR<$flag1";
	}elsif($i_clean==5){
			$clean_para=~/dele_flag=(.*?);dele_sbr=(.*?);/;
			$flag1=$1;
			$flag2=$2;
			$clean_method="deleted flag $flag1 and delete SBR<$flag2";
	}elsif($i_clean==6){
			$clean_para=~/dele_sig=(.*?);dele_snr=(.*?);/;
			my $snr=$2;
			my $sig=$1;
			$clean_method="deleted signal intensity<$sig and delete SNR<$snr";
	}else{
			$clean_method="Unknown";
	}
		if($i_normalize==1){
			$normalize_method="normalized by all spots mean signals in same slide";
		}elsif($i_normalize==2){
			$normalize_method="normalized by all spots sum signals in same slide";
		}elsif($i_normalize==3){
			$normal_para=~/ctr_gene=(.*)/;
			$normalize_method="normalized by mean signals of $1 in same slide";
		}elsif($i_normalize==4){
			$normal_para=~/snr_thr=(.*)/;
			$normalize_method="normalized by all spots with SNR>=$1 in same slide";
		}elsif($i_normalize==5){
			$normalize_method="normalized by greatest mean value among replicates";
		}elsif($i_normalize==6){
			$normalize_method="normalized by all spots median signals in same slide";
		}elsif($i_normalize==7){
			$normalize_method="normalized by greatest sum among ctr slides";
		}elsif($i_normalize==8){
			$normalize_method="normalized by greatest sum among ctr slides in shared spots";
		}elsif($i_normalize==9){
			$normalize_method="normalized gene by gene across all reps of samples";
		}elsif(!normalize_method){
			$normalize_method="Unknown";
		}
		if(!$order){$order="Unknown";}
	return ($clean_method,$order,$normalize_method);
}

sub cal_para_cv(){
	my ($object,$dbhptr,$table_name,$times,$sigma,$i_ratio,$i_spots)=@_;
	my $times_sigma="times$times"."sigma$sigma";
	$times_sigma=~s/\./_/g;
	my $ratio_sigma="ratio$i_ratio"."sigma$sigma";
	$ratio_sigma=~s/\./_/g;
	(my $i_sigma=$sigma)=~s/\./_/;
	if($times eq "" and $sigma eq ""){
		$statement="select gene_ID,count(nomalized_data),avg(nomalized_data),stddev_samp(nomalized_data),ratio$i_ratio from $table_name group by gene_ID having ratio$i_ratio is null and count(nomalized_data)>=$i_spots";
	}elsif(($i_spots==0 and $i_ratio==0) or ($i_spots==1 and $i_ratio==0)){
		$statement="select gene_ID,count(nomalized_data),avg(nomalized_data),stddev_samp(nomalized_data),$times_sigma from $table_name group by gene_ID,sigma$i_sigma having sigma$i_sigma='' and $times_sigma='0' and count(nomalized_data)>=2";
	}elsif($i_spots>=2 and $i_ratio>0){
		$statement="select gene_ID,count(nomalized_data),avg(nomalized_data),stddev_samp(nomalized_data),$times_sigma,$ratio_sigma from $table_name group by gene_ID,sigma$i_sigma having sigma$i_sigma='' and $times_sigma='0' and $ratio_sigma is null and count(nomalized_data)>=$i_spots";
	}else{
#		 $Response->Write("Parameters error!!");
		die "$times,$sigma,$i_ratio,$i_spots";
	}
#	my $statement="select gene_ID,count(signal_mean),avg(signal_mean),stddev_samp(signal_mean),avg(nomalized_data),stddev_samp(nomalized_data) from $comb_table group by gene_ID having gene_ID in ($list_bgenes) and COUNT(signal_mean)>=2";
    #$Response->Write("<p>$statement</p>");
	my $ssth = $$dbhptr->prepare($statement)  or die "Can't prepare $statement: $$dbhptr->errstr\n";
	my $rv = $ssth->execute() or die "$statement$!";
	my (@normal_CV);
	while(my ($gene,$i_probe,$normal_avg,$normal_std)=$ssth->fetchrow_array){
		push @normal_CV,($normal_std/$normal_avg*100);
	}
	my $nor_cv_mean=sprintf "%.4f",&mean(@normal_CV);
	my $nor_cv_std=sprintf "%.4f",&std_dev(\@normal_CV);
	return ($nor_cv_mean,$nor_cv_std);
}

sub cal_Rsquare(){
	my ($object,$table1,$table2,$elements,$type,$table_name,$dbhptr)=@_;
	my $row;
	if ($type eq "probe"){
		$row="probe_ID";
	}else{
		$row="gene_ID";
	}
	my $mix_table="\'$table1\',\'$table2\'";
	my $list=join(",",@{$elements});
	my $R_sqt;
	if(scalar @{$elements}>0){
		my $statement="SELECT $row,table_name,avg(nomalized_data) from $table_name group by $row,table_name having $row in ($list) and table_name in ($mix_table) order by $row,table_name";
		#print "$statement\n";
		$ssth = $$dbhptr->prepare($statement)  or die "Can't prepare $statement: $dbh->errstr\n";
		$rv = $ssth->execute() or die "can't execute the query: $statement";
		my (@table1_values,@table2_values,$ori);
		my $i=0;
		while(my @line=$ssth->fetchrow_array){
			if ($i%2==0){
				push @table1_values,$line[2];
				$ori=$line[0];
			}else{
				if ($line[0] ne $ori){
					push @table2_values,0;
					die "False in calculating the R square";
				}else{
					push @table2_values,$line[2];
				}
			}
			$i++;
		}
		my $co = new Statistics::Basic::Correlation( \@table1_values, \@table2_values);
		$R_sqt=$co->query;
	}else{
	$R_sqt=0;
	}
	return $R_sqt;
}


sub connectdb() {
	my $host      = "localhost";
	my $dbname    = "Agilent";
	my $dbuser    = "nguser";
	my $dbpass    = "nguser123!";
	
	my $dbhptr;
	$dbhptr = DBI->connect("DBI:mysql:$dbname:$host",$dbuser,$dbpass);
	return $dbhptr;
}

sub disconnectdb() {
	my ($object,$dbhptr)=@_;
	$$dbhptr->disconnect;
	return 1;
}

