# Mandatory Schooling Prep Pipeline

There is no dedicated prep script for mandatory schooling yet.

The current pipeline is manual:

1. Maintain `DATA_INPUT/compulsory_schooling_laws.csv` in Dropbox.
2. Run `analysis/analysis_compulsory_schooling.R`, which reads the manual CSV directly and assigns treatment by state law decade.

This is separate from `prep/elite_schools`: mandatory schooling is a broad state-level compulsory attendance law treatment, while elite schools are selective secondary-school openings.
