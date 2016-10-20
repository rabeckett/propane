define main = {
	T0.$prefix$ => end(T0),
	true => drop
}

control {
	aggregate($aggregatePrefix$, T1 -> T2),
	tag(8075:1, 0.0.0.0/0, T1 -> T2),
	maxroutes(10, T0 -> T1),
	longest_path(10, T0)
}