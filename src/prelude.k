define sqrt(numeric v) returning numeric {
	_caos { SETV $_return(w) SQRT $v(r); }
}

define iterator(agent) enum(numeric family, numeric genus, numeric species) {
  agent targ;
  _caos {
## XXX: is this use of .loop correct?
    .loop {
    .targ > $targ {
      ENUM $family(r) $genus(r) $species(r);
    };
    .kaos { _yield(targ); };
    };
    NEXT;
  };
}
define unid(agent a) returning numeric {
  _caos { .targ < $a { SETV $_return(w) UNID; }; };
}
