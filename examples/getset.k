define norn() returning agent {
  _caos { SETA $_return(w) NORN; };
}

define set norn(agent norn) {
  _caos { NORN $norn(r); }
}
install {
  norn() = norn();
}
