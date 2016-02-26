define PG1 = 1.0.0.0/24
define PG2 = 1.0.1.0/24
define PL1 = 2.0.1.0/24
define PL2 = 2.0.2.0/24

define LEAK = 8075:430

define transit(X) = enter(X) and exit(X) 

define notransit = {
	true => not transit(Back1 or Back2)
}

define main = {
	LEAK => end(out),
	PG1 => end(A),
	PG2 => end(B),
	PL1 => end(E) and always(A or B or C or D or E or F or G or H),
	PL2 => end(F) and internal,
	true => exit(Back1) >> exit(Back2)
}

and notransit