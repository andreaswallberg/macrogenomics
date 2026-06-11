# Marine macrogenomics
Commands and scripts to download and process taxonomic and genetic metadata.

## Downloading NGS records from ENA

Data used to compile NGS statistics were downloaded from the European Nucleotide Archive (ENA) using the public ENA Browser API. "sample", "read_run" and "study" records were downloaded as three separate and TSV files, respectively. Commands:

### To download runs (12 GB TSV file):

```
curl --retry 5      --retry-delay 10      --keepalive-time 60      -X POST "https://www.ebi.ac.uk/ena/portal/api/search"      -d "result=read_run"      -d "fields=run_accession,sample_accession,experiment_accession,study_accession,tax_id,scientific_name,library_strategy,library_source,instrument_platform,country,collection_date,lat,lon,location,marine_region,first_created,project_name,study_title,sample_description"      -d "limit=0"      -d "offset=0"      -d "format=tsv" > ena_runs.description.tsv
```

### To download samples (18 GB TSV file):

```
curl --retry 5      --retry-delay 10      --keepalive-time 60      -X POST "https://www.ebi.ac.uk/ena/portal/api/search"      -d "result=sample"      -d "fields=submission_accession,sample_accession,secondary_sample_accession,tax_id,scientific_name,tax_lineage,sample_title,project_name,description,sample_description"      -d "limit=0"      -d "offset=0"      -d "format=tsv" > ena_samples.description.tsv
```

### To download studies (1 GB TSV file):

```
curl --retry 5      --retry-delay 10      --keepalive-time 60      -X POST "https://www.ebi.ac.uk/ena/portal/api/search"      -d "result=study"      -d "fields=study_accession,description,first_public,keywords,project_name,study_name,study_title,study_description"      -d "limit=0"      -d "offset=0"      -d "format=tsv" > ena_study.description.tsv
```

## Making data tables

The NGS data was cross-referenced with taxonomic species lists from the World Register of Marine Species (WoRMS) and AlgaeBase, in order to keep only putative marine records. Due to terms of usage and agreements necessary to access these comprehensive taxonomic datasets, re-distribution of full taxonomic lists to third parties is restricted. The species lists were prepared with the scripts `filter_marine.pl` (keep only marine species), `get_taxonomy.pl` (add higher taxonomic classification to species list) and `get_unique_species.pl` (remove any redundant records).

```
	./ena_to_tables.pl \
		ena_table \
		species_names.tsv \
		ena_study.description.tsv \
		ena_samples.description.tsv \
		ena_runs.description.tsv
```

## Deriving statistics from the data tables

Statistics for both ancient and contemporary (modern/current-day) samples were derived from the samples, producing summary output at the species and group levels.

```
	for TYPE in ancient contemporary; do

		for REGION in world; do

			./ena_tables_to_stats.pl \
				$REGION \
				ena_table.taxa.species.${TYPE}.tsv \
				ena_table.samples.species.${TYPE}.tsv \
				classification_to_orders.tsv
		
		done
	
	done
```

## Geolocating samples

Parsing of geographic information of records to derive their geographic location, if possible.

```
Rscript annotate_ocean_region.optimized.R ena_table.samples.species.contemporary.tsv.groups.tsv ena_table.samples.species.contemporary.tsv.groups.tsv.annotated_ocean_bodies.tsv
```

## Derive per-ocean statistics

Producing statistics for the NGS records according to major ocean bodies.

```
parse_ocean_bodies.pl ena_table.samples.species.contemporary.tsv.groups.tsv.annotated_ocean_bodies.tsv > ena_table.samples.species.contemporary.tsv.groups.tsv.annotated_ocean_bodies.tsv.summary.tsv
```
