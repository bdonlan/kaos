define velx(agent a) returning numeric {
	_caos {
		.targ < $a {
			.inline let $return = VELX;
		};
	};
}

define norn() returning agent {
	_caos {
		.inline let $return = NORN;
	};
}

install {
	print(velx(norn()));
}
