import delimited "C:\Users\olizardo\Google Drive\NetHealth Research\Data\arcsfall2015.csv", clear /*change to relevant local folder*/
cd "C:\Users\olizardo\Google Drive\NetHealth Research\PROJECTS\assortativity - psych scales\network ensembles"
discard

//dk1 preserves degree 
//dk2 preserves degree and degree distribution

forval i = 1  / 500 {
	dkseries, i(i) j(j) iter(10) dk(1) /*iter is how many swaps */
	cd "C:\Users\olizardo\Google Drive\NetHealth Research\PROJECTS\assortativity - psych scales\network ensembles\Fall 2015\dk1" /*change to relevant local folder*/
	export delimited arcsfall2015_dk1_shuff`i'.csv, replace 
	}


import delimited "C:\Users\olizardo\Google Drive\NetHealth Research\Data\arcsspring2016.csv", clear /*change to relevant local folder*/
cd "C:\Users\olizardo\Google Drive\NetHealth Research\PROJECTS\assortativity - psych scales\network ensembles"
discard
forval i = 1  / 500 {
	dkseries, i(i) j(j) iter(10) dk(1)
	cd "C:\Users\olizardo\Google Drive\NetHealth Research\PROJECTS\assortativity - psych scales\network ensembles\Spring 2016\dk1" /*change to relevant local folder*/
	export delimited arcsspring2016_dk1_shuff`i'.csv, replace 
	}
	
import delimited "C:\Users\olizardo\Google Drive\NetHealth Research\Data\arcsfall2015.csv", clear /*change to relevant local folder*/
cd "C:\Users\olizardo\Google Drive\NetHealth Research\PROJECTS\assortativity - psych scales\network ensembles"
discard
forval i = 1  / 500 {
	dkseries, i(i) j(j) iter(10) dk(2)
	cd "C:\Users\olizardo\Google Drive\NetHealth Research\PROJECTS\assortativity - psych scales\network ensembles\Fall 2015\dk2" /*change to relevant local folder*/
	export delimited arcsfall2015_dk2_shuff`i'.csv, replace 
	}


import delimited "C:\Users\olizardo\Google Drive\NetHealth Research\Data\arcsspring2016.csv", clear /*change to relevant local folder*/
cd "C:\Users\olizardo\Google Drive\NetHealth Research\PROJECTS\assortativity - psych scales\network ensembles"
discard
forval i = 1  / 500 {
	dkseries, i(i) j(j) iter(10) dk(2)
	cd "C:\Users\olizardo\Google Drive\NetHealth Research\PROJECTS\assortativity - psych scales\network ensembles\Spring 2016\dk2" /*change to relevant local folder*/
	export delimited arcsspring2016_dk2_shuff`i'.csv, replace 
	}

/*checking
import delimited "C:\Users\olizardo\Google Drive\NetHealth Research\Data\arcsfall2015.csv", clear /*change to relevant local folder*/
gen counter = 1
qui bysort i : egen outdeg = sum(counter)
list i j outdeg if i < 3

import delimited "C:\Users\olizardo\Google Drive\NetHealth Research\PROJECTS\assortativity - psych scales\network ensembles\Fall 2015\arcsfall2015_shuff228.csv", clear /*change to relevant local folder*/
gen counter = 1
qui bysort i : egen outdeg = sum(counter)
list i j outdeg if i < 3

