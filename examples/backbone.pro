define PCust = 10.0.0.0/24

define Routing = {
  true  => exit(R1 >> R2 >> Peer >> Prov)
}

define Ownership = {
  PCust => later(Cust) 
}

define transit(X) = enter(X) and exit(X)

define NoTransit = {
    true => not transit(Peer or Prov)
}

define main = Routing and Ownership and NoTransit
