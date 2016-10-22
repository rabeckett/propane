define main = {
	T0.$prefix$ => end(T0),
	true => exit(Peer1 >> Peer2)
}

control {
	aggregate($aggregatePrefix$, in -> out),
	tag(8075:1, 0.0.0.0/0, in -> out),
	maxroutes(10, T0 -> T1),
	longest_path(10, T0)
}