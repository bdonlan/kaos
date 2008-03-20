numeric x = 4;
numeric y;
_caos {
  .loop {
    REPS 4;
      ADDV $x(rw) 2;
      .let $y = $x;
    REPE;
  };
};
print(y);
