define PL1 = 2.0.1.0/24
define PL2 = 2.0.2.0/24
define PG1 = 1.0.0.0/24
define PG2 = 1.0.1.0/24
define PG  = 1.0.0.0/16

define Peer = {Peer1, Peer2}

define routing = {
	PG1 => end(A),
	PG2 => end(B),
	PL1 => end(E),
	PL2 => end(F),
	true => exit(Peer1) >> exit(Peer2)
}

define locality = {
	PL1 or PL2 => always(in)
}

define transit(X,Y) = enter(X) and exit(Y)

define notransit = {
	true => not transit(Peer, Peer)
}

define main = routing and locality and notransit

control {
	aggregate(PG, in -> out)
}