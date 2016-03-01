define PCust = 10.0.0.0/24

define routing = {
  true  => exit(R1) >> exit(R2) >> exit(Peer) >> exit(Prov)
}

define ownership = {
  PCust => through(Cust) 
}

define transit(X) = enter(X) and exit(X)

define notransit = {
    true => not transit( {Peer, Prov} )
}

define main = routing and ownership and notransit
