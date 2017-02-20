define main = {
	T0.$prefix$ => end(T0),
	true => exit(Peer1 >> Peer2)
}

control {
	aggregate($aggregatePrefix$, in -> out),
}