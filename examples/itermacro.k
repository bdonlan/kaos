define iterator reps(numeric n) {
  _caos {
    REPS $n(r);
    .loop { .kaos { _yield(); }; };
    REPE;
  };
}

install {
  numeric count = 4;
  __touch(count);
  reps(count) { print("hello, world!\n"); }
}
