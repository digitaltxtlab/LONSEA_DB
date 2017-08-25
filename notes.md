# LONSEA database #
[site here](http://www.lonsea.de/)

At the moment, we have 12468 people, 3872 places, 1161 organisations and 21 topics in our database.

### data issues ###
- interpretation of values (e.g., gender)
- missing data/imputation
- normalization of *nationality* (rule: select country; select first)  
- repetition of *pname* based on *canonical_fname* (reflect organization history of individual)
- Three person (15 rows) have identical born years, begin year and end year. In career.path.dev.adam.R they are omitted in the analysis.

fname
canonical_fname: hierarchical
oname: section where they work (horizontal)

comment recode:
- canonical_fname recode af ordinal features


*reproduce entire data set LoN on fname / canonical_fname*
  - split nationality
  - data missing in the year, length(year) < 4
staff turnover

split nation

theory:
1: descriptive data for all features
2: response: organization history
  - people with and without dynamic path (recode)
3: more or less international (number of nationality each year)
  - variance nationality at different levels (recode hierarchy)
    - first - second - third division
4: staff turnover
  - recruitment
  - consolidation (short term staff --> increase of turnover)
5: number of people pr year (growth of the league)
   - control for nationality
   - descriptive stats for years
6: generational effect on age (age in cohorts)

FILTERS
LoN
General Assembly

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
meeting February 1, 2017
distribution of members pr. year
 - do canonical_fname for division for annual distribution of members
 - viz matters
