define unid(agent a) returning numeric {
  _caos { .targ < $a { SETV $_return(w) UNID; }; };
}

install {
  enum (4, 0, 0) { |creature| print(unid(creature)); }
}
