program define shuffle
syntax,i(varname) j(varname) iter(integer) 
	qui tab `i'
	local n = r(N) /*record number of edges*/
	mata : shuffle("`i'", "`j'", `iter', `n') /*pass "ego" variable name, "alter" variable name, n. of iterations, and edge list size to mata program*/
end

version 14.2
mata: mata clear
mata:
	void shuffle(string scalar i, string scalar j, real scalar k, real scalar n) {
		st_view(v2,.,j) /*create a "view" into the "alter" column of the edgelist data so that you can modify it directly from mata*/
		z = 1 /*initialize counter*/
		while (z <= k) { /*go for k number of iterations*/
			e1=1+trunc(n*runiform(1,1)) /*select a random edge (data set row; a random number between 1 and N)*/
			e2=1+trunc(n*runiform(1,1)) /*select another random edge*/
			l = v2[e1, 1] /*record the end vertex for first edge*/
			n = v2[e2, 1] /*record the end vertex for second edge*/
			v2[e1, 1] = n /*make end vertex of first edge the end vertex of the second edge*/
			v2[e2, 1] = l /*make end vertex of second edge the end vertex of the first edge*/
			z++ /*increase counter*/
			} /*end edge swap routine*/
	} /*end program*/
end

			
