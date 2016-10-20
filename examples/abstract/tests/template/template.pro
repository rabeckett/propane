define Peer = {Peer1, Peer2}

define transit(X,Y) = enter(X) & exit(Y)

define notransit = {
	true => not transit(Peer, Peer)
}

define routing = {
	T0.$prefix$ => end(T0),
	true => drop #exit(Peer1 >> Peer2)
}

define main = routing & notransit

/* control {
	aggregate($aggregatePrefix$, in -> out),
	tag(8075:1, 0.0.0.0/0, in -> out),
	maxroutes(10, T0 -> T1),
	longest_path(10, T0)
} */