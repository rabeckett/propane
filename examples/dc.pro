define PG1 = 1.0.0.0/24
define PG2 = 1.0.1.0/24
define PL1 = 2.0.1.0/24
define PL2 = 2.0.2.0/24

/* define transit(X) = enter(X) and exit(X) 

define T0 = A or B or E or F
define T1 = C or D or G or H 
define T2 = X or Y

define no_valleys = {
	true => valleyfree(T0, T1, T2)
} */

define main = {
	PG1 => end(A),
	PG2 => end(B),
	PL1 => end(E) and internal,
	PL2 => end(F) and internal,
	true => exit(Back1) >> exit(Back2)
}


