define PL1 = 1.0.0.0/24
define PL2 = 1.0.1.0/24
define PG1 = 2.0.0.0/24
define PG2 = 2.0.1.0/24

define Peer = {Peer1, Peer2}

define transit(X,Y) = enter(X) and exit(Y)

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

define notransit = {
	true => not transit(Peer, Peer)
}

define main = routing and locality and notransit

control {
	aggregate(2.0.0.0/16, in -> out)
}