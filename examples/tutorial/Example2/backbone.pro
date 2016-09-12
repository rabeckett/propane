define Peer = {Sprint, Level3}

define transit(X,Y) = enter(X+Y) & exit(X+Y)

define notransit = { 
	true => not transit(Peer,Peer) 
}

define preferences = { 
	true => exit(R1 >> R2 >> Peer) 
}

define ownership = {
	172.4.1.0/24 => any
}

define main = preferences & ownership & notransit
