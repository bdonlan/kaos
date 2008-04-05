define velx(agent a) returning numeric {
	_caos {
		.targ < $a {
			.inline let $return = VELX;
		};
	}
}

define badkill(agent a) {
	_caos {
		KILL $a(r);
	}
}

define bar(numeric n) {
	kill(norn());
//	badkill(norn());
	print(n);
}

install {
	bar(velx(norn()));
}
