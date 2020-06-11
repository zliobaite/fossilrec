# NOW data preprocessing for recomemnder systems

[The description is unfinished, to be continued soon]

A repository providing instructions or preprocessing fossil data from NOW database. Will be renamed to dataNOW.

File <code>NOW\_20200611\_public.csv</code> contains a database dump from June 11, 2020. It is a flat table, each row is a species occurence at a site (locality). 

For example, suppose we have sites A, B; possible species are x, y, z. Suppose site A containc species (x,z), site B contains species (y,z), the the table will have the following rows:

	Ax
	Az
	By
	Bz

For experimenting with recommender system one needs to convert this to the occurence matrix, such:

		x	y	z
	A	1	0	1
	B	0	1	0
	
I'm not giving any code for conversion, you can easily do it in the most convenient way for you. I will explain next how to handle species and localities. Handling localities is rather straightforward, but handling species is not.

If you download the flat table, it should have around 85 columns. The database is being developed as well as data, so depending on when you are reading this, there may be more columns, but the principle, I'm sure stays. It is a good idea to download the newest data from the database, since data is being edited, updated and extended continuously.


The table has 85 columns. The columns describing sites (localities) that are relevant for recommender systems task: 

* LIDNUM, NAME - site ID and name (names should be unique, but better to use ID to be safe)
* MAX\_AGE, MIN\_AGE - timestamps of the site, upper and lower age limits in millions of years
* LATSTR, LONGSTR, LAT, LONG - geographic coordinates of sites in string and numeric formats (relevant if one wants to take spatial autocorrelation into account, for instance)

You may want to filter by continent or country.

Other columns describing sites, not directly relevant for the recommender task:

* BFA\_MAX, BFA\_MAX\_ABS, BFA\_MIN, BFA\_MIN\_ABS, FRAC\_MAX, FRAC\_MIN - basis for age and fractions of age, they indicate where timestamp information comes from
* CHRON, AGE_COMM - chronostratigraphic age and age comment, they can give extra information about age as free text entries
* COUNTRY, STATE, COUNTY - where the site is located
* APNUMSPM - how many specimens were found on the site approximately
* GENERAL - whether it is a general locality (yes or no), general locality indicates sites composed of several localities or when the area of collection is only known approximately
* LOC_SYNONYMS - other names by which the site may be known
* MEAN\_HYPSODONTY, ESTIMATE\_PRECIP, ESTIMATE\_TEMP, ESTIMATE\_NPP, PERS\_WOODY\_COVER, PERS\_POLLEN\_AP, PERS\_POLLEN\_NAP, PERS\_POLLEN\_OTHER - interpreted characteristics of the site, e.g. estimates of mean annual precipitation

The remaining coliumns describe species. In the Linnean taxonomic system a species is uniquely described by GENUS+SPECIES. For example, a wolf is called *Canis lupus*.

[to be continued soon]






