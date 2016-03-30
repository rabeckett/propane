define PG1 = 1.0.0.0/24
define PG2 = 1.0.1.0/24
define PL1 = 2.0.1.0/24
define PL2 = 2.0.2.0/24

define Peer = {Back1, Back2}

define ownership = {
	PG1 => end(A),
	PG2 => end(B),
	PL1 => end(E),
	PL2 => end(F),
	true => exit(Back1) >> exit(Back2)
}

define locality = {
	PL1 or PL2 => always(in)
}

define transit(X,Y) = enter(X) and exit(Y)

define notransit = {
	true => not transit(Peer, Peer)
}

define main = ownership and locality and notransit

control {
	aggregate(1.0.0.0/16, in -> out)
}

