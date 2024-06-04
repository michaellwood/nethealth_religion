program define dkseries
syntax, i(varname) j(varname) iter(integer) dk(integer)
	cap drop ki
	cap drop l
	cap drop counter
	qui sum i
	local N = r(N) /*record number of edges*/
	local iter = `iter' * `N'
	qui levelsof i,l(ilevs)
	local nlevs : word count `ilevs'
	gen l = .
	local m 1
	foreach l of local ilevs {
		qui replace l = `l' in `m'
		local ++m
		}
	gen counter = 1
	bysort i : egen ki = sum(counter) /*get degrees*/
	preserve
		collapse ki,by(i)
		sum ki
	restore
	drop counter
	mata : shuffle("`i'", "`j'", `iter', `N', `dk', `nlevs') /*pass "ego" variable name, "alter" variable name, n. of iterations, edge list size to mata program, and swap type to the mata*/
end

version 15
mata: mata clear
mata:
	void shuffle(string scalar i, string scalar j, real scalar w, real scalar N, real scalar dk, real scalar nlevs) {
		st_view(v1,.,i) /*create a "view" into the "ego" column of the edgelist data so that you can see it and/or modify it directly from mata*/
		st_view(v2,.,j) /*create a "view" into the "alter" column of the edgelist data so that you can see it and/or modify it directly from mata*/
		v = st_data((1, nlevs),"l") 
		if (dk == 0) { /*begin dk swap 0 routine*/
			z = 1 /*initialize counter*/
			while (z <= w) { /*go for w number of iterations*/
				skip = 0
				e1 = 1+trunc(N*runiform(1,1)) /*select a random edge (data set row; a random number between 1 and N)*/
				k = v1[e1, 1] /*record origin vertex of edge*/
				l = v2[e1, 1] /*record destination vertex of edge*/
				r1 = 1+trunc(nlevs*runiform(1,1)) 
				r2 = 1+trunc(nlevs*runiform(1,1)) 
				m = v[r1, 1]
				n = v[r2, 1]
				if (k == m | k == n | l == m | l == n | m == n) { /*check that neither of randomly selected vertices are same as those in edge to be swapped*/
					skip = 1
					}
				if (skip == 0) {
					kk = 0 /*initializing degree counter for node k*/
					kl = 0 /*initializing degree counter for node l*/
					for (e=1;e<=N;e++) { 
						if (v1[e, 1] == k) {
							kk++
							}
						if (v1[e, 1] == l) {
							kl++
							}
						if (v1[e, 1] == m & v2[e, 1] == n) {
							skip = 1 /*skip main conditional if mn edge exists*/
							}
						}
					if (kl == 1 | kk == 1) {
						skip = 1 /*skip if deletion of kl edge will make either k or l an isolate*/
						}
					}
			  if (skip == 0) {
				v1[e1, 1] = m /*create new swapped edge km and delete randomly selected edge*/
				v2[e1, 1] = n /*create new swapped edge km and delete randomly selected edge*/
				z
				z++ /*increase counter*/
				}
			} /*end while loop*/
		} /*end dk swap 0 routine*/
		if (dk == 1) { /*begin dk swap 1 routine*/
			z = 1 /*initialize counter*/
			while (z <= w) { /*go for w number of iterations*/
				skip = 0
				e1 = 1
				e2 = 1
				while (e1 == e2) {
					e1=1+trunc(N*runiform(1,1)) /*select a random edge (data set row; a random number between 1 and N)*/
					e2=1+trunc(N*runiform(1,1)) /*select a random edge (data set row; a random number between 1 and N)*/
					k = v1[e1, 1] /*record origin vertex for first edge*/
					l = v2[e1, 1] /*record destination vertex for first edge*/
					m = v1[e2, 1] /*record origin vertex for second edge*/
					n = v2[e2, 1] /*record destination vertex for second edge*/
					}
				if (k == m | k == n | l == m | l == n) {  /*check that neither of randomly selected vertices are same as those in edge to be swapped*/
					skip = 1
					}
				if (skip == 0) {
					for (e=1;e<=N;e++) { /*checking to see that the kn and ml edges do not exist*/
						if ((v1[e, 1] == k & v2[e, 1] == n) | (v1[e, 1] == m & v2[e, 1] == l)) {
							skip = 1 /*skip next loop if they exist*/
							}/*end no pre-existing potential swaps conditional*/
						}
					}
				if (skip == 0) { /*create new swapped edges kn and lm*/
				  v2[e1, 1] = n 
				  v2[e2, 1] = l 
				  z
				  z++ /*increase counter only on non-skipped loops*/
				  } 
				} /*end main while loop*/
		  } /*end dk swap 1 routine*/
		if (dk == 2) { /*begin dk swap 2 routine*/
			z = 1 /*initialize counter*/
			while (z <= w) { /*go for w number of iterations*/
				skip = 0
				e1 = 1
				e2 = 1
				while (e1 == e2) {
					e1=1+trunc(N*runiform(1,1)) /*select a random edge (data set row; a random number between 1 and N)*/
					e2=1+trunc(N*runiform(1,1)) /*select a random edge (data set row; a random number between 1 and N)*/
					k = v1[e1, 1] /*record origin vertex for first edge*/
					l = v2[e1, 1] /*record destination vertex for first edge*/
					m = v1[e2, 1] /*record origin vertex for second edge*/
					n = v2[e2, 1] /*record destination vertex for second edge*/
					}
			if (k == m | k == n | l == m | l == n) {  /*check that neither of randomly selected vertices are same as those in edge to be swapped*/
				skip = 1
				}
			if (skip == 0) { /*get degrees of each of the origin vertices*/
				kk = 0 /*initializing degree counter for node k*/
				km = 0 /*initializing degree counter for node l*/
				for (e=1;e<=N;e++) { /*loop through edgelist*/
					if (v1[e, 1] == k) { /*count of k's degree*/
						kk++
						} /*end if*/
					if (v1[e, 1] == m) { /*count of m's degree*/
						km++
						} /*end if*/
					if ((v1[e, 1] == k & v2[e, 1] == n) | (v1[e, 1] == m & v2[e, 1] == l)) { /*make sure that new reshuffled links don't already exist*/
						skip = 1 
						} /*end if*/
					} /*end edgelist loop*/
				} /*end skip if*/
				if (kk != km) { /*check that origin vertices in each edge have same degree*/
					skip = 1
					} /*end if*/
				if (skip == 0) { /*create new swapped edges kn and lm*/
				  v2[e1, 1] = n 
				  v2[e2, 1] = l 
				  z
				  z++ /*increase counter only on non-skipped loops*/
				  } /*end if*/
				} /*end main while loop*/
		  } /*end dk swap 2 routine*/
	} /*end program*/
end






