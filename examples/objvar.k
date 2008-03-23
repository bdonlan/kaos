ovar numeric foo;
ovar string bar[10];

install {
  enum(4,0,0) { |x|
	x.foo = 10;
    print(x.bar);
  }
}
