define PG1 = 1.0.0.0/24
define PG2 = 1.0.1.0/24
define PL1 = 2.0.1.0/24
define PL2 = 2.0.2.0/24

define Peer = {Peer1, Peer2}

define ownership = {
	PG1 => originate(A),
	PG2 => originate(B),
	PL1 => originate(E),
	PL2 => originate(F),
	true => exit(Peer1) >> exit(Peer2)
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

