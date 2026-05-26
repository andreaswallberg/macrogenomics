#!/usr/bin/perl -w

use strict;
use warnings;
use 5.010;
use Data::Dumper;

my @samples;
my @taxa;

my $name_table = {};

my $study_table = {};
my $sample_table = {};
my $taxon_table = {};

my $base_name = shift @ARGV;

my $names_file = shift @ARGV;
my $study_file = shift @ARGV;
my $sample_file = shift @ARGV;
my $run_file = shift @ARGV;

open ( my $names_in , "<" , $names_file ) or die "$!";

say "Reading species names ...";

while (<$names_in>) {

	chomp;
	
	$name_table->{ $_ } = 1;

}

open ( my $study_in , "<" , $study_file ) or die "$!";

say "Reading studies ...";

my $header = <$study_in>;

while (<$study_in>) {

	chomp;
	
	my @data = split ( /\t/ , $_ );
	
	my ( $study_accession , $description , $first_public , $keywords , $project_name , $study_name , $study_title , $study_description ) =
	@data;
	
	foreach my $info ( @data ) {
	
		if ( $info =~ m/aDNA/ or $info =~ m/ancient\W+DNA/i or $info =~ m/sedaDNA/ or $info =~ m/sedimentary\sancient\W+DNA/i or $info =~ m/paleogenomics/i or $info =~ m/museum\ssample/i or $info =~ m/museum\sspecimen/i ) {
		
			$study_table->{ $study_accession }->{ 'is_aDNA' } = 1;
		
		}
		
		if ( $info =~ m/marine/ or $info =~ m/seawater/i ) {
		
			$study_table->{ $study_accession }->{ 'is_marine' } = 1;
		
		}
	
	}
		
	$study_table->{ $study_accession }->{ 'description' } = $description;
	$study_table->{ $study_accession }->{ 'first_public' } = $first_public;
	$study_table->{ $study_accession }->{ 'keywords' } = $keywords;
	$study_table->{ $study_accession }->{ 'project_name' } = $project_name;
	$study_table->{ $study_accession }->{ 'study_name' } = $study_name;
	$study_table->{ $study_accession }->{ 'study_title' } = $study_title;
	$study_table->{ $study_accession }->{ 'study_description' } = $study_description;
	
}

open ( my $sample_in , "<" , $sample_file ) or die "$!";

say "Reading samples ...";

$header = <$sample_in>;

while (<$sample_in>) {

	chomp;
	
	my @data = split ( /\t/ , $_ );
	
	my ( $sample , $taxon_id , $name , $tax_lineage , $title , $project_name , $description , $sample_description ) =
	@data[ 0 , 3 , 4 , 5 , 6 , 7 , 8 , 9 ];

	$title = "" unless defined $title;
	$project_name = "" unless defined $project_name;
	$description = "" unless defined $description;
	$sample_description = "" unless defined $sample_description;
	
	foreach my $info ( $title , $description , $sample_description ) {
	
		if ( $info =~ m/aDNA/ or $info =~ m/ancient\W+DNA/i or $info =~ m/sedaDNA/ or $info =~ m/paleogenomics/i or $info =~ m/museum\ssample/i or $info =~ m/museum\sspecimen/i ) {
		
			$sample_table->{ $sample }->{ 'is_aDNA' } = 1;
		
		}
		
		if ( $info =~ m/marine/ or $info =~ m/seawater/i ) {
		
			$sample_table->{ $sample }->{ 'is_marine' } = 1;
		
		}
	
	}
	
	if ( defined $name ) {
		
		$sample_table->{ $sample }->{ 'taxon' } = $taxon_id;
		$sample_table->{ $sample }->{ 'title' } = $title;
		$sample_table->{ $sample }->{ 'project_name' } = $project_name;
		$sample_table->{ $sample }->{ 'description' } = $description;
		$sample_table->{ $sample }->{ 'sample_description' } = $sample_description;
		
		unless ( defined $taxon_table->{ $taxon_id } ) {
		
			push @taxa , $taxon_id;
		
		}
		
		foreach my $tmp_name ( split ( /\;/ , $name ) ) {
		
			$taxon_table->{ $taxon_id }->{ "name" }->{ $tmp_name } = 1;

		}
		
		$taxon_table->{ $taxon_id }->{ "sample" }->{ $sample } = 1;
		
		unless ( defined $taxon_table->{ $taxon_id }->{ "tax_lineage" } ) {
		
			$taxon_table->{ $taxon_id }->{ "tax_lineage" } = $tax_lineage;
	
		}
	
	}
	
}

open ( my $run_in , "<" , $run_file ) or die "$!";

say "Reading runs ...";

$header = <$run_in>;

while (<$run_in>) {

	chomp;
	
	my @data = split ( /\t/ , $_ );
	
	my ( $tmp_samples , $study_accession , $taxon_id , $name , $strategy , $source , $instrument ) =
	@data[ 1 , 3 , 4 , 5 , 6 , 7 , 8 ];
	
	foreach my $sample ( split ( /\;/ , $tmp_samples ) ) {
	
		if ( defined $study_table->{ $study_accession } ) {
	
			if ( defined $study_table->{ $study_accession }->{ 'is_aDNA' } ) {
			
				$sample_table->{ $sample }->{ 'is_aDNA' } = 1;
			
			}
			
			if ( defined $study_table->{ $study_accession }->{ 'is_marine' } ) {
			
				$sample_table->{ $sample }->{ 'is_marine' } = 1;
			
			}
	
		}
	
		if ( defined $sample_table->{ $sample } and defined $taxon_table->{ $taxon_id } ) {
		
			unless ( defined $sample_table->{ $sample }->{ 'strategy' } ) {
			
				push @samples, $sample;
				
			}
	
			$sample_table->{ $sample }->{ 'study' } = $study_accession;
			$sample_table->{ $sample }->{ 'strategy' } = $strategy;
			$sample_table->{ $sample }->{ 'source' } = $source;
			$sample_table->{ $sample }->{ 'instrument' } = $instrument;
			
			my ( $country , $collection_date , $lat , $lon , $location , $first_created ) =
			@data[ 9 , 10 , 11 , 12  , 13 , 15 ];
			
			$sample_table->{ $sample }->{ 'first_created' } = $first_created if defined $first_created;
			$sample_table->{ $sample }->{ 'country' } = $country if defined $country;
			$sample_table->{ $sample }->{ 'collection_date' } = $collection_date if defined $collection_date;
			$sample_table->{ $sample }->{ 'lat' } = $lat if defined $lat;
			$sample_table->{ $sample }->{ 'lon' } = $lon if defined $lon;
			$sample_table->{ $sample }->{ 'location' } = $location if defined $location;
	
		}
	
	}
	
}

check_metagenomic();
check_species();

sub check_metagenomic {

	my $type = "metagenomic";
	
	say "Checking $type ...";
	
	open ( my $c_out , ">" , ${base_name} . ".taxa.${type}.contemporary.tsv" ) or die "$!";
	open ( my $a_out , ">" , ${base_name} . ".taxa.${type}.ancient.tsv" ) or die "$!";
	
	say $c_out "TAXON_ID\tSCIENTIFIC_NAME\tN_AMPLICON\tN_AMPLICON_W_GEO\tN_WGS\tN_WGS_W_GEO\tN_TRANSCRIPTOMIC\tN_TRANSCRIPTOMIC_W_GEO\tN_RAD\tN_RAD_W_GEO\tN_WXS\tN_WXS_W_GEO\tLINEAGE";
	say $a_out "TAXON_ID\tSCIENTIFIC_NAME\tN_AMPLICON\tN_AMPLICON_W_GEO\tN_WGS\tN_WGS_W_GEO\tN_TRANSCRIPTOMIC\tN_TRANSCRIPTOMIC_W_GEO\tN_RAD\tN_RAD_W_GEO\tN_WXS\tN_WXS_W_GEO\tLINEAGE";
	
	my @all_keep_samples_contemporary;
	my @all_keep_samples_ancient;
	
	foreach my $taxon_id ( @taxa ) {

		my @meta_contemporary;
		my @meta_ancient;
	
		my @names;
		
		if ( defined $taxon_table->{ $taxon_id }->{ "name" } ) {
		
			push @names , ( sort keys %{ $taxon_table->{ $taxon_id }->{ "name" } } );
		
		}

		my $lineage = "";
		
		$lineage = $taxon_table->{ $taxon_id }->{ "tax_lineage" } if defined $taxon_table->{ $taxon_id }->{ "tax_lineage" };
		
		my @samples = sort keys %{ $taxon_table->{ $taxon_id }->{ "sample" } };
		
		foreach my $sample ( @samples ) {
		
			my $is_meta = 0;
		
			if (
			
				defined $sample_table->{ $sample }->{ "source" } and
				(
					$sample_table->{ $sample }->{ "source" } =~ m/metagenom/i or
					$sample_table->{ $sample }->{ "source" } =~ m/metatranscriptom/i
				)
				
			) {
			
				$is_meta = 1;
			
			}
			
			elsif (
			
				defined $sample_table->{ $sample }->{ 'project_name' } and
				(
					$sample_table->{ $sample }->{ 'project_name' } =~ m/metagenom/i or
					$sample_table->{ $sample }->{ 'project_name' } =~ m/metatranscriptom/i
				)
				
			) {
			
				$is_meta = 1;
			
			}
			
			elsif (
			
				defined $sample_table->{ $sample }->{ 'title' } and
				(
					$sample_table->{ $sample }->{ 'title' } =~ m/metagenom/i or
					$sample_table->{ $sample }->{ 'title' } =~ m/metatranscriptom/i
				)
				
			) {
			
				$is_meta = 1;
			
			}
			
			foreach my $name ( @names ) {
			
				if ( $name =~ m/metagenom/i or $name =~ m/metatranscriptom/i ) {
				
					$is_meta = 1;
				
				}
			
			}
			
			if ( $is_meta ) {

				$sample_table->{ $sample }->{ 'is_meta' } = 1;
				
				if (
				
					$sample_table->{ $sample }->{ 'title' } =~ m/ancient\W+DNA/i or
					$sample_table->{ $sample }->{ 'title' } =~ m/aDNA/
					
				) {
					
					$sample_table->{ $sample }->{ 'is_aDNA' } = 1;
				
				}
				
				foreach my $name ( @names ) {
				
					if ( $name =~ m/ancient\W+DNA/i or $name =~ m/aDNA/ ) {
						
						$sample_table->{ $sample }->{ 'is_aDNA' } = 1;
					
					}
				
				}
				
				if ( $sample_table->{ $sample }->{ 'is_marine' } ) {
				
					# Final checks for reasonable marine metagenomic libraries
					
					my $keep = 0;
					
					foreach my $name ( @names ) {
					
						if ( defined $name_table->{ $name } ) {
				
							$keep = 1;
							
						}
						
						foreach my $keyword (
						
							"marine sediment" ,
							"marine meta" ,
							"marine plankton" ,
							"seawater" ,
							"coral" ,
							"sponge" ,
							"alga" ,
							"oyster" ,
							"fish" ,
							"inverterate" ,
							"hydrothermal" ,
							"cold seep" ,
							"mollusc" ,
							"gill" ,
							"sea " ,
							"seagrass" ,
							"hydrozoan" ,
							"beach" ,
							"sand" ,
							"mangrove" ,
							"shrimp" ,
							"jellyfish" ,
							"ballast" ,
							"biofouling" ,
							"volcano" ,
							"ice" ,
							"estuary" ,
						
						) {
						
							if ( $name =~ m/${keyword}/i ) {
						
								$keep = 1;
						
							}
						
						}
						
						foreach my $keyword (
						
							"human" ,
							"Homo " ,
							"Mus " ,
							"indoor " ,
						
						) {
						
							if ( $name =~ m/${keyword}/i ) {
						
								$keep = 0;
						
							}
						
						}
				
					}

					if ( $keep ) {
				
						if ( $sample_table->{ $sample }->{ 'is_aDNA' } ) {
						
							push @meta_ancient , $sample;
						
						}
						
						else {
						
							push @meta_contemporary , $sample;
						
						}
					
					}
				
				}
			
			}
		
		}
		
		my ( $count_table , $taxon_keep_samples_ref ) = count_samples( \@meta_contemporary );

		my @taxon_keep_samples = @{ $taxon_keep_samples_ref };
			
		if ( @taxon_keep_samples ) {
			
			push @all_keep_samples_contemporary , @taxon_keep_samples;
			
			say $c_out "$taxon_id" ,
				"\t" , join ( "|" , @names ) ,
				"\t" , $count_table->{ "n_amplicon" } ,
				"\t" , $count_table->{ "n_amplicon_geo" } ,
				"\t" , $count_table->{ "n_wgs" } ,
				"\t" , $count_table->{ "n_wgs_geo" } ,
				"\t" , $count_table->{ "n_transcriptomic" } ,
				"\t" , $count_table->{ "n_transcriptomic_geo" } ,
				"\t" , $count_table->{ "n_rad" } ,
				"\t" , $count_table->{ "n_rad_geo" } ,
				"\t" , $count_table->{ "n_wxs" } ,
				"\t" , $count_table->{ "n_wxs_geo" } ,
				"\t" , $lineage;
		
		}
		
		( $count_table , $taxon_keep_samples_ref ) = count_samples( \@meta_ancient );

		@taxon_keep_samples = @{ $taxon_keep_samples_ref };
			
		if ( @taxon_keep_samples ) {
			
			push @all_keep_samples_ancient , @taxon_keep_samples;
			
			say $a_out "$taxon_id" ,
				"\t" , join ( "|" , @names ) ,
				"\t" , $count_table->{ "n_amplicon" } ,
				"\t" , $count_table->{ "n_amplicon_geo" } ,
				"\t" , $count_table->{ "n_wgs" } ,
				"\t" , $count_table->{ "n_wgs_geo" } ,
				"\t" , $count_table->{ "n_transcriptomic" } ,
				"\t" , $count_table->{ "n_transcriptomic_geo" } ,
				"\t" , $count_table->{ "n_rad" } ,
				"\t" , $count_table->{ "n_rad_geo" } ,
				"\t" , $count_table->{ "n_wxs" } ,
				"\t" , $count_table->{ "n_wxs_geo" } ,
				"\t" , $lineage;
		
		}
		
	}
	
	print_samples( ${type} . ".contemporary" , \@all_keep_samples_contemporary );
	print_samples( ${type} . ".ancient" , \@all_keep_samples_ancient );
	
}

sub check_species {

	my $type = "species";

	say "Checking $type ...";
	
	open ( my $c_out , ">" , ${base_name} . ".taxa.${type}.contemporary.tsv" ) or die "$!";
	open ( my $a_out , ">" , ${base_name} . ".taxa.${type}.ancient.tsv" ) or die "$!";
	
	say $c_out "TAXON_ID\tSCIENTIFIC_NAME\tN_AMPLICON\tN_AMPLICON_W_GEO\tN_WGS\tN_WGS_W_GEO\tN_TRANSCRIPTOMIC\tN_TRANSCRIPTOMIC_W_GEO\tN_RAD\tN_RAD_W_GEO\tN_WXS\tN_WXS_W_GEO\tLINEAGE";
	say $a_out "TAXON_ID\tSCIENTIFIC_NAME\tN_AMPLICON\tN_AMPLICON_W_GEO\tN_WGS\tN_WGS_W_GEO\tN_TRANSCRIPTOMIC\tN_TRANSCRIPTOMIC_W_GEO\tN_RAD\tN_RAD_W_GEO\tN_WXS\tN_WXS_W_GEO\tLINEAGE";
	
	my @all_keep_samples_contemporary;
	my @all_keep_samples_ancient;
	
	foreach my $taxon_id ( @taxa ) {

		my @species_contemporary;
		my @species_ancient;

		my @names;
		
		if ( defined $taxon_table->{ $taxon_id }->{ "name" } ) {
		
			push @names , ( grep { defined $name_table->{ $_ } } sort keys %{ $taxon_table->{ $taxon_id }->{ "name" } } );
		
		}
		
		next unless @names;

		my $lineage = "";
		
		$lineage = $taxon_table->{ $taxon_id }->{ "tax_lineage" } if defined $taxon_table->{ $taxon_id }->{ "tax_lineage" };
		
		my @samples = sort keys %{ $taxon_table->{ $taxon_id }->{ "sample" } };
		
		my @keep_samples;
		
		foreach my $sample ( @samples ) {
		
			$sample_table->{ $sample }->{ 'is_marine' } = 1;
		
			$sample_table->{ $sample }->{ "is_species" } = 1;
				
			if (
			
				$sample_table->{ $sample }->{ 'title' } =~ m/ancient\W+DNA/i or
				$sample_table->{ $sample }->{ 'title' } =~ m/aDNA/
				
			) {
				
				$sample_table->{ $sample }->{ 'is_aDNA' } = 1;
			
			}
			
			foreach my $name ( @names ) {
			
				if ( $name =~ m/ancient\W+DNA/i or $name =~ m/aDNA/ ) {
					
					$sample_table->{ $sample }->{ 'is_aDNA' } = 1;
				
				}
			
			}
			
			if ( not defined $sample_table->{ $sample }->{ "is_meta" } ) {

				if ( $sample_table->{ $sample }->{ 'is_aDNA' } ) {
				
					push @species_ancient , $sample;
				
				}
				
				else {
				
					push @species_contemporary , $sample;
				
				}
			
			}
			
		}
		
		my ( $count_table , $taxon_keep_samples_ref ) = count_samples( \@species_contemporary );

		my @taxon_keep_samples = @{ $taxon_keep_samples_ref };
			
		if ( @taxon_keep_samples ) {
			
			push @all_keep_samples_contemporary , @taxon_keep_samples;
			
			say $c_out "$taxon_id" ,
				"\t" , join ( "|" , @names ) ,
				"\t" , $count_table->{ "n_amplicon" } ,
				"\t" , $count_table->{ "n_amplicon_geo" } ,
				"\t" , $count_table->{ "n_wgs" } ,
				"\t" , $count_table->{ "n_wgs_geo" } ,
				"\t" , $count_table->{ "n_transcriptomic" } ,
				"\t" , $count_table->{ "n_transcriptomic_geo" } ,
				"\t" , $count_table->{ "n_rad" } ,
				"\t" , $count_table->{ "n_rad_geo" } ,
				"\t" , $count_table->{ "n_wxs" } ,
				"\t" , $count_table->{ "n_wxs_geo" } ,
				"\t" , $lineage;
		
		}
		
		( $count_table , $taxon_keep_samples_ref ) = count_samples( \@species_ancient );

		@taxon_keep_samples = @{ $taxon_keep_samples_ref };
			
		if ( @taxon_keep_samples ) {
			
			push @all_keep_samples_ancient , @taxon_keep_samples;
			
			say $a_out "$taxon_id" ,
				"\t" , join ( "|" , @names ) ,
				"\t" , $count_table->{ "n_amplicon" } ,
				"\t" , $count_table->{ "n_amplicon_geo" } ,
				"\t" , $count_table->{ "n_wgs" } ,
				"\t" , $count_table->{ "n_wgs_geo" } ,
				"\t" , $count_table->{ "n_transcriptomic" } ,
				"\t" , $count_table->{ "n_transcriptomic_geo" } ,
				"\t" , $count_table->{ "n_rad" } ,
				"\t" , $count_table->{ "n_rad_geo" } ,
				"\t" , $count_table->{ "n_wxs" } ,
				"\t" , $count_table->{ "n_wxs_geo" } ,
				"\t" , $lineage;
		
		}
		
	}
	
	print_samples( ${type} . ".contemporary" , \@all_keep_samples_contemporary );
	print_samples( ${type} . ".ancient" , \@all_keep_samples_ancient );

}

sub count_samples {

	my ( $keep_samples_ref ) = @_;

	my @keep_samples = @{ $keep_samples_ref };
	
	my @taxon_keep_samples;
	
	my $count_table = {};
	
	$count_table->{ "n_wgs" } = 0;
	$count_table->{ "n_wgs_geo" } = 0;
	
	$count_table->{ "n_transcriptomic" } = 0;
	$count_table->{ "n_transcriptomic_geo" } = 0;

	$count_table->{ "n_wxs" } = 0;
	$count_table->{ "n_wxs_geo" } = 0;
	
	$count_table->{ "n_amplicon" } = 0;
	$count_table->{ "n_amplicon_geo" } = 0;
	
	$count_table->{ "n_rad" } = 0;
	$count_table->{ "n_rad_geo" } = 0;
	
	foreach my $sample ( @keep_samples ) {
	
		if ( defined $sample_table->{ $sample }->{ 'strategy' } ) {
		
			my $strategy = $sample_table->{ $sample }->{ "strategy" };
			my $source = $sample_table->{ $sample }->{ "source" };
			
			my $geo = 0;
					
			if (
			
				defined $sample_table->{ $sample }->{ 'lat' } and
				$sample_table->{ $sample }->{ 'lat' } =~ m/\d/
				
			) {
				
				$geo = 1;
	
			}
			
			if (
			
				defined $sample_table->{ $sample }->{ 'lon' } and
				$sample_table->{ $sample }->{ 'lon' } =~ m/\d/
				
			) {
				
				$geo = 1;
	
			}
			
			if ( $geo == 0 and defined $sample_table->{ $sample }->{ 'location' } ) {
			
				if (
			
					$sample_table->{ $sample }->{ 'location' } eq "NA" or
					$sample_table->{ $sample }->{ 'location' } =~ m/^not\s/i or
					$sample_table->{ $sample }->{ 'location' } =~ m/^missing\s*/i
				
				) {
			
					$geo = 0;
				
				}
				
				elsif ( $sample_table->{ $sample }->{ 'location' } =~ m/\w/ ) {
				
					$geo = 1;
					
				}
	
			}
			
			if ( $geo == 0 and defined $sample_table->{ $sample }->{ 'country' } ) {
				
				if (
				
					$sample_table->{ $sample }->{ 'country' } eq "NA" or
					$sample_table->{ $sample }->{ 'country' } =~ m/^not\s/i or
					$sample_table->{ $sample }->{ 'country' } =~ m/^missing\s*/i
					
				) {
				
					$geo = 0;
				
				}
				
				elsif ( $sample_table->{ $sample }->{ 'country' } =~ m/\w/ ) {
				
					$geo = 1;
	
				}
				
			}
			
			if ( $strategy =~ m/(wgs|wga)/i ) {

				if ( $source eq "GENOMIC" ) {
				
					$count_table->{ "n_wgs" }++;
					
					$count_table->{ "n_wgs_geo" }++ if $geo;
					
					push @taxon_keep_samples , $sample;
			
				}
				
			}
				
			elsif ( $strategy =~ m/wxs/i ) {
			
				if ( $source eq "GENOMIC" ) {
			
					$count_table->{ "n_wxs" }++;
				
					$count_table->{ "n_wxs_geo" }++ if $geo;
					
					push @taxon_keep_samples , $sample;
			
				}
			
			}
			
			elsif ( $strategy =~ m/RNA-seq/i ) {
				
				if ( $source eq "TRANSCRIPTOMIC" ) {
			
					$count_table->{ "n_transcriptomic" }++;
				
					$count_table->{ "n_transcriptomic_geo" }++ if $geo;
					
					push @taxon_keep_samples , $sample;
			
				}
			
			}
			
			elsif ( $strategy =~ m/AMPLICON/i ) {
			
					$count_table->{ "n_amplicon" }++;
				
					$count_table->{ "n_amplicon_geo" }++ if $geo;
					
					push @taxon_keep_samples , $sample;
			
			}
			
			elsif ( $strategy =~ m/RAD\W+Seq/i ) {
			
					$count_table->{ "n_rad" }++;
				
					$count_table->{ "n_rad_geo" }++ if $geo;
					
					push @taxon_keep_samples , $sample;
			
			}
		
		}
	
	}
	
	return ( $count_table , \@taxon_keep_samples );

}

sub print_samples () {

	my ( $type , $samples_ref ) = @_;

	open ( my $out , ">" , ${base_name} . ".samples.${type}.tsv" ) or die "$!";

	say $out "SAMPLE\tDATASET_TYPE\tSTUDY_ACCESSION\tSAMPLE_TITLE\tPROJECT_NAME\tTAXON_ID\tSCIENTIFIC_NAME\tCREATED_DATE\tCOLLECTION_DATE\tSTRATEGY\tSOURCE\tINSTRUMENT\tCOUNTRY\tLAT\tLON\tLOCATION\tDESCRIPTION\tSAMPLE_DESCRIPTION";

	foreach my $sample ( @{ $samples_ref } ) {

		next unless defined $sample_table->{ $sample }->{ "taxon" };
		next unless defined $sample_table->{ $sample }->{ "strategy" };
		next unless defined $sample_table->{ $sample }->{ "source" };
		next unless defined $sample_table->{ $sample }->{ "instrument" };
		
		my $country = "";
		my $lat = "";
		my $lon = "";
		my $location = "";
		my $created_date = "";
		my $collection_date = "";
		
		$country = $sample_table->{ $sample }->{ 'country' } if defined $sample_table->{ $sample }->{ 'country' };
		$lat = $sample_table->{ $sample }->{ 'lat' } if defined $sample_table->{ $sample }->{ 'lat' };
		$lon = $sample_table->{ $sample }->{ 'lon' } if defined $sample_table->{ $sample }->{ 'lon' };
		$location = $sample_table->{ $sample }->{ 'location' } if defined $sample_table->{ $sample }->{ 'location' };

		$created_date = $sample_table->{ $sample }->{ 'first_created' } if defined $sample_table->{ $sample }->{ 'first_created' };
		$collection_date = $sample_table->{ $sample }->{ 'collection_date' } if defined $sample_table->{ $sample }->{ 'collection_date' };
		
		my $taxon_id = $sample_table->{ $sample }->{ "taxon" };
		
		my @names;
		
		if ( defined $taxon_table->{ $taxon_id }->{ "name" } ) {
		
			push @names , ( sort keys %{ $taxon_table->{ $taxon_id }->{ "name" } } );
		
		}
		
		say $out "$sample" ,
			"\t" , $type ,
			"\t" , $sample_table->{ $sample }->{ "study" } ,
			"\t" , $sample_table->{ $sample }->{ "title" } ,
			"\t" , $sample_table->{ $sample }->{ "project_name" } ,
			"\t" , $taxon_id ,
			"\t" , join ( "|" , @names ) ,
			"\t" , $created_date ,
			"\t" , $collection_date ,
			"\t" , $sample_table->{ $sample }->{ "strategy" } ,
			"\t" , $sample_table->{ $sample }->{ "source" } ,
			"\t" , $sample_table->{ $sample }->{ "instrument" } , 
			"\t" , $country ,
			"\t" , $lat ,
			"\t" , $lon ,
			"\t" , $location ,
			"\t" , $sample_table->{ $sample }->{ "description" } ,
			"\t" , $sample_table->{ $sample }->{ "sample_description" };

	}

}
