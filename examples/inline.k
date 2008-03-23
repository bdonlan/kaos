define velx(agent a) returning numeric {
	_caos {
		.targ < $a {
			.inline let $_return = VELX;
		};
	};
}

define norn() returning agent {
	_caos {
		.inline let $_return = NORN;
	};
}

install {
	print(velx(norn()));
}
