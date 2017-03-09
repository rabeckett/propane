# Give meaningful names to ASes/prefixes
define Princeton = as500
define Peer = {Sprint, Level3}
define private = 
  10.0.0.0/[8..32] or 
  172.16.0.0/[12..32] or 
  192.168.0.0/[16..32] or 
  169.254.0.0/[16..32] or 
  240.0.0.0/[4..32] or 
  192.0.0.0/[24..32]

# Prefer to leave through R2 over R1 over a Peer
define preferences = { 
	private => drop,
	true => exit(R2 >> Cust >> Peer) 
}

# Transit traffic between X and Y 
# both enters and leaves through X or Y
define transit(X,Y) = enter(X+Y) & exit(X+Y)

# Always prevent transit between Peers
define notransit = { 
	true => not transit(Peer,Peer) 
}

# Ensure traffic for the prefix 140.180.0.0/16 ends up at Princeton
# This can prevent certain types of route hijacks
define ownership = {
	140.180.0.0/16 => end(Princeton)
}

define main = preferences & ownership & notransit
