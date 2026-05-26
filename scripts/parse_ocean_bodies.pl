#!/usr/bin/perl -w

use strict;
use warnings;
use 5.010;

open ( my $in , "<" , $ARGV[ 0 ] ) or die "$!";

my $header = <$in>;
chomp $header;

my $ocean_table = {};

while (<$in>) {

	chomp;
	
	my @data = split ( /\t/ , $_ );
	
	my ( $library , $species , $region , $ocean_body , $hemisphere ) = @data[ 0 , 6 , 21 , 27 , 28 ];

	next if $region eq "NA";
	next if $ocean_body eq "NA";
	next if $hemisphere eq "NA";
	
	# say "$library\t$species\t$region\t$ocean_body\t$hemisphere";
	
	if ( $ocean_body eq "Mediterranean" ) {
	
		$ocean_body = "Atlantic";
	
	}
	
	if ( $hemisphere eq "Equator" ) {
	
		$hemisphere = "North";
	
	}
	
	$ocean_table->{ $hemisphere . " " . $ocean_body }->{ "species" }->{ $species } = 1;
	$ocean_table->{ $hemisphere . " " . $ocean_body }->{ "libraries" }++;
	
}

say "OCEAN\tSPECIES\tLIBRARIES";

foreach my $ocean ( sort keys %{ $ocean_table } ) {

	say "$ocean\t" , scalar ( keys %{ $ocean_table->{ $ocean }->{ "species" } } ) , "\t" , $ocean_table->{ $ocean }->{ "libraries" };

}
