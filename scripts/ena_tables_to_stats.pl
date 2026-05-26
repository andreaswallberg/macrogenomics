#!/usr/bin/perl -w

use strict;
use warnings;
use 5.010;
use Data::Dumper;

my $tag = shift @ARGV;
my $table_file = shift @ARGV;
my $sample_file = shift @ARGV;

my $group_table = {};

my $species_table = {};

foreach my $species_list_file ( @ARGV ) {

	open ( my $in , "<" , $species_list_file ) or die "$!";
	
	while (<$in>) {

		chomp;
		
		my ( $group , $n_species , $kingdom , $phylun , $class , $order , $species_list ) = split ( /\t/ , $_ );
		
		foreach my $species ( split ( /\|/ , $species_list ) ) {
				
				$species =~ s/\(.+\)\s//;
			
				$species_table->{ $species }->{ 'group' } = $group;
			
				$group_table->{ $group }->{ 'species' }->{ $species } = 1;
		
		}

	}

}

open ( my $in , "<" , $table_file ) or die "$!";

my $header = <$in>;
chomp $header;

my $data_table = {};

while (<$in>) {

	chomp;
	
	my @data = split ( /\t/ , $_ );
	
	my ( $taxon_id , $scientific_names , $n_amplicon , $n_amplicon_geo , $n_wgs , $n_wgs_geo , $n_trans , $n_trans_geo , $n_rad , $n_rad_geo , $n_wxs , $n_wxs_geo ) = @data;

	my $keep = 0;
	
	my $species;
	
	foreach my $tmp_species_name ( split ( /\|/ , $scientific_names ) ) {
	
		if ( defined $species_table->{ $tmp_species_name } ) {
	
			$keep = 1;
			
			$species = $tmp_species_name;
			
			last;
		
		}
	
	}
	
	next unless $keep;

	my $group = $species_table->{ $species }->{ 'group' };
	
	# AMPLICON
	
	if ( $n_amplicon >= 1 ) {
	
		$data_table->{ $group }->{ "n_amplicon" } += $n_amplicon;
	
		$data_table->{ $group }->{ "n_amplicon_gt_1" }++;
	
		$species_table->{ $species }->{ 'seen' } = 1;

	}
	
	if ( $n_amplicon >= 10 ) {
	
		$data_table->{ $group }->{ "n_amplicon_gt_10" }++;
	
	}
	
	if ( $n_amplicon >= 50 ) {
	
		$data_table->{ $group }->{ "n_amplicon_gt_50" }++;
	
	}
	
	if ( $n_amplicon_geo >= 1 ) {
	
		$data_table->{ $group }->{ "n_amplicon_geo" } += $n_amplicon_geo;
	
		$data_table->{ $group }->{ "n_amplicon_geo_gt_1" }++;
	
	}
	
	if ( $n_amplicon_geo >= 10 ) {
	
		$data_table->{ $group }->{ "n_amplicon_geo_gt_10" }++;
	
	}
	
	if ( $n_amplicon_geo >= 50 ) {
	
		$data_table->{ $group }->{ "n_amplicon_geo_gt_50" }++;
	
	}
	
	# WGS
	
	if ( $n_wgs >= 1 ) {
	
		$data_table->{ $group }->{ "n_wgs" } += $n_wgs;
	
		$data_table->{ $group }->{ "n_wgs_gt_1" }++;
	
		$species_table->{ $species }->{ 'seen' } = 1;
		
	}
	
	if ( $n_wgs >= 10 ) {
	
		$data_table->{ $group }->{ "n_wgs_gt_10" }++;
	
	}
	
	if ( $n_wgs >= 50 ) {
	
		$data_table->{ $group }->{ "n_wgs_gt_50" }++;
	
	}
	
	if ( $n_wgs_geo >= 1 ) {
	
		$data_table->{ $group }->{ "n_wgs_geo" } += $n_wgs_geo;
	
		$data_table->{ $group }->{ "n_wgs_geo_gt_1" }++;
	
	}
	
	if ( $n_wgs_geo >= 10 ) {
	
		$data_table->{ $group }->{ "n_wgs_geo_gt_10" }++;
	
	}
	
	if ( $n_wgs_geo >= 50 ) {
	
		$data_table->{ $group }->{ "n_wgs_geo_gt_50" }++;
	
	}
	
	# TRANS

	if ( $n_trans >= 1 ) {
	
		$data_table->{ $group }->{ "n_trans" } += $n_trans;
	
		$data_table->{ $group }->{ "n_trans_gt_1" }++;
	
		$species_table->{ $species }->{ 'seen' } = 1;
	
	}
	
	if ( $n_trans >= 10 ) {
	
		$data_table->{ $group }->{ "n_trans_gt_10" }++;
	
	}
	
	if ( $n_trans >= 50 ) {
	
		$data_table->{ $group }->{ "n_trans_gt_50" }++;
	
	}
	
	if ( $n_trans_geo >= 1 ) {
	
		$data_table->{ $group }->{ "n_trans_geo" } += $n_trans_geo;
	
		$data_table->{ $group }->{ "n_trans_geo_gt_1" }++;
	
	}
	
	if ( $n_trans_geo >= 10 ) {
	
		$data_table->{ $group }->{ "n_trans_geo_gt_10" }++;
	
	}
	
	if ( $n_trans_geo >= 50 ) {
	
		$data_table->{ $group }->{ "n_trans_geo_gt_50" }++;
	
	}
	
	# RAD
	
	if ( $n_rad >= 1 ) {
	
		$data_table->{ $group }->{ "n_rad" } += $n_rad;
	
		$data_table->{ $group }->{ "n_rad_gt_1" }++;
	
		$species_table->{ $species }->{ 'seen' } = 1;
	
	}
	
	if ( $n_rad >= 10 ) {
	
		$data_table->{ $group }->{ "n_rad_gt_10" }++;
	
	}
	
	if ( $n_rad >= 50 ) {
	
		$data_table->{ $group }->{ "n_rad_gt_50" }++;
	
	}
	
	if ( $n_rad_geo >= 1 ) {
	
		$data_table->{ $group }->{ "n_rad_geo" } += $n_rad_geo;
	
		$data_table->{ $group }->{ "n_rad_geo_gt_1" }++;
	
	}
	
	if ( $n_rad_geo >= 10 ) {
	
		$data_table->{ $group }->{ "n_rad_geo_gt_10" }++;
	
	}
	
	if ( $n_rad_geo >= 50 ) {
	
		$data_table->{ $group }->{ "n_rad_geo_gt_50" }++;
	
	}
	
	# WXS
	
	if ( $n_wxs >= 1 ) {
	
		$data_table->{ $group }->{ "n_wxs" } += $n_wxs;
	
		$data_table->{ $group }->{ "n_wxs_gt_1" }++;
	
		$species_table->{ $species }->{ 'seen' } = 1;
	
	}
	
	if ( $n_wxs >= 10 ) {
	
		$data_table->{ $group }->{ "n_wxs_gt_10" }++;
	
	}
	
	if ( $n_wxs >= 50 ) {
	
		$data_table->{ $group }->{ "n_wxs_gt_50" }++;
	
	}
	
	if ( $n_wxs_geo >= 1 ) {
	
		$data_table->{ $group }->{ "n_wxs_geo" } += $n_wxs_geo;
	
		$data_table->{ $group }->{ "n_wxs_geo_gt_1" }++;
	
	}
	
	if ( $n_wxs_geo >= 10 ) {
	
		$data_table->{ $group }->{ "n_wxs_geo_gt_10" }++;
	
	}
	
	if ( $n_wxs_geo >= 50 ) {
	
		$data_table->{ $group }->{ "n_wxs_geo_gt_50" }++;
	
	}
	
}

my @fields = (
	"n_amplicon_gt_1" , "n_amplicon_geo_gt_1" , "n_amplicon_gt_10" , "n_amplicon_geo_gt_10" , "n_amplicon_gt_50" , "n_amplicon_geo_gt_50" ,
	"n_wgs_gt_1" , "n_wgs_geo_gt_1" , "n_wgs_gt_10" , "n_wgs_geo_gt_10" , "n_wgs_gt_50" , "n_wgs_geo_gt_50" ,
	"n_trans_gt_1" , "n_trans_geo_gt_1" , "n_trans_gt_10" , "n_trans_geo_gt_10" , "n_trans_gt_50" , "n_trans_geo_gt_50" ,
	"n_rad_gt_1" , "n_rad_geo_gt_1" , "n_rad_gt_10" , "n_rad_geo_gt_10" , "n_rad_gt_50" , "n_rad_geo_gt_50" ,
	"n_wxs_gt_1" , "n_wxs_geo_gt_1" , "n_wxs_gt_10" , "n_wxs_geo_gt_10" , "n_wxs_gt_50" , "n_wxs_geo_gt_50" ,
);

open ( my $out , ">" , $table_file . ".${tag}.stats.tsv" ) or die "$!";

say $out "GROUP\tGROUP_SIZE\tGROUP_SEEN\tPROP_SEEN\t" , join ( "\t" , @fields );

my @groups = sort keys %{ $data_table };

foreach my $group ( @groups ) {
	
	say $group;
	
	say Dumper ( $data_table->{ $group } );
	
	my @species = sort keys %{ $group_table->{ $group }->{ 'species' } };
	
	my $group_size = @species;
	
	my $group_size_seen = scalar grep { defined $species_table->{ $_ }->{ 'seen' } } @species;
	
	my $prop_seen = ( $group_size_seen / $group_size ) * 100;
	
	print $out $group , "\t" , $group_size , "\t" , $group_size_seen , "\t" , $prop_seen;
	
	foreach my $field ( @fields ) {
	
		$data_table->{ $group }->{ $field } = 0 unless defined $data_table->{ $group }->{ $field };
	
		print $out "\t" , $data_table->{ $group }->{ $field };
	
	}
	
	say $out "";

}

@fields = (
	"n_amplicon" , "n_amplicon_geo" ,
	"n_wgs" , "n_wgs_geo" ,
	"n_trans" , "n_trans_geo" ,
	"n_rad" , "n_rad_geo" ,
	"n_wxs" , "n_wxs_geo",
);

open ( $out , ">" , $table_file . ".${tag}.stats.libraries.tsv" ) or die "$!";

say $out "GROUP\tGROUP_SIZE\tGROUP_SEEN\tPROP_SEEN\t" , join ( "\t" , @fields );

foreach my $group ( @groups ) {
	
	say $group;
	
	my @species = sort keys %{ $group_table->{ $group }->{ 'species' } };
	
	my $group_size = @species;
	
	my $group_size_seen = scalar grep { defined $species_table->{ $_ }->{ 'seen' } } @species;
	
	my $prop_seen = ( $group_size_seen / $group_size ) * 100;
	
	print $out $group , "\t" , $group_size , "\t" , $group_size_seen , "\t" , $prop_seen;
	
	foreach my $field ( @fields ) {
	
		$data_table->{ $group }->{ $field } = 0 unless defined $data_table->{ $group }->{ $field };
	
		print $out "\t" , $data_table->{ $group }->{ $field };
	
	}
	
	say $out "";

}

open ( $in , "<" , $sample_file ) or die "$!";

open ( my $sample_out , ">" , $sample_file . ".groups.tsv" ) or die "$!";

$header = <$in>;
chomp $header;

say $sample_out "$header\tGROUP\tLIBRARY_TYPE\tGEO";

say STDERR $header;

my $date_table = {};

while (<$in>) {

	chomp;
	
	my @data = split ( /\t/ , $_ );
	
	# SAMPLE  DATASET_TYPE    STUDY_ACCESSION SAMPLE_TITLE    PROJECT_NAME    TAXON_ID        SCIENTIFIC_NAME CREATED_DATE COLLECTION_DATE    STRATEGY        SOURCE  INSTRUMENT      COUNTRY LAT     LON     LOCATION        DESCRIPTION     SAMPLE_DESCRIPTION
	
	my ( $sample , $type , $study_accession , $title , $project_name , $taxon_id , $scientific_names , $created_date , $collection_date , $strategy , $source , $instrument , $country , $lat , $lon , $location ) = @data;
	
	my $keep = 0;
	
	my $species;
	
	foreach my $tmp_species_name ( split ( /\|/ , $scientific_names ) ) {
	
		if ( defined $species_table->{ $tmp_species_name } ) {
	
			$keep = 1;
			
			$species = $tmp_species_name;
			
			last;
		
		}
	
	}
	
	next unless $keep;
	
	my $group = $species_table->{ $species }->{ 'group' };
	
	my $geo = "no_geo";
	
	my $library_type;
	
	if (
	
		defined $lat and $lat =~ m/\d/
		
	) {
		
		$geo = "has_geo";

	}
	
	if (
	
		defined $lon and $lon  =~ m/\d/
		
	) {
		
		$geo = "has_geo";

	}
	
	if ( defined $location ) {
	
		if (
	
			$location eq "NA" or
			$location =~ m/^not\s/i or
			$location =~ m/^missing\s*/i
		
		) {

		}
		
		elsif ( $location =~ m/\w/ ) {
		
			$geo = "has_geo";
			
		}

	}
	
	if ( defined $country ) {
		
		if (
		
			$country eq "NA" or
			$country =~ m/^not\s/i or
			$country =~ m/^missing\s*/i
			
		) {
		
		}
		
		elsif ( $country =~ m/\w/ ) {
		
			$geo = "has_geo";

		}
		
	}
	
	if ( $geo eq "has_geo" ) {
	
		say STDERR "$_";
	
	}

	if ( $strategy =~ m/(wgs|wga)/i ) {

		if ( $source eq "GENOMIC" ) {
		
			$library_type = "wgs";
	
		}
		
	}
		
	elsif ( $strategy =~ m/wxs/i ) {
	
		if ( $source eq "GENOMIC" ) {
	
			$library_type = "wxs";
		}
	
	}
	
	elsif ( $strategy =~ m/RNA-seq/i ) {
		
		if ( $source eq "TRANSCRIPTOMIC" ) {
	
			$library_type = "trans";
	
		}
	
	}
	
	elsif ( $strategy =~ m/(amplicon)/i ) {
		
		$library_type = "amplicon";

		
	}
	
	elsif ( $strategy =~ m/RAD\W+Seq/i ) {
	
		if ( $source eq "GENOMIC" ) {
	
			$library_type = "rad";
		}
	
	}
	
	if ( defined $library_type ) {
	
		say $sample_out "$_\t$library_type\t$geo";
	
		my $tmp_date = $created_date;
		
		$tmp_date =~ s/\W.*//;
		
		if ( $tmp_date =~ m/^\d+$/ ) {
		
			$date_table->{ $tmp_date }->{ $library_type }->{ $group }++;
	
		}
	
	}
	
}

open ( my $time_out , ">" , $sample_file . ".timeline.tsv" ) or die "$!";

print $time_out "YEAR";

foreach my $library_type ( "amplicon" , "wgs" , "trans" , "rad" , "wxs" ) {

	foreach my $group ( @groups ) {
	
		print $time_out "\t${library_type}_${group}_N";
	
	}

}

say $time_out "";

my @years = sort { $a <=> $b } keys %{ $date_table };

foreach my $tmp_date ( @years ) {

	print $time_out $tmp_date;

	foreach my $library_type ( "amplicon" , "wgs" , "trans" , "rad" , "wxs" ) {

		foreach my $group ( @groups ) {
		
			$date_table->{ $tmp_date }->{ $library_type }->{ $group } = 0
			unless defined
			$date_table->{ $tmp_date }->{ $library_type }->{ $group };
			
			print $time_out "\t" ,
			$date_table->{ $tmp_date }->{ $library_type }->{ $group }
		
		}
	
	}
	
	say $time_out "";

}
