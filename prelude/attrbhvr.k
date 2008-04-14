// ATTR
define numeric A_CARRYABLE = 1;
define numeric A_MOUSEABLE = 2;
define numeric A_ACTIVATEABLE = 4;
define numeric A_GREEDYCABIN = 8;
define numeric A_INVISIBLE = 16;
define numeric A_FLOATABLE = 32;
define numeric A_SUFFERCOLLISIONS = 64;
define numeric A_SUFFERPHYSICS = 128;
define numeric A_CAMERASHY = 256;
define numeric A_OPENCABIN = 512;
define numeric A_ROTATABLE = 1024;
define numeric A_PRESENCE = 2048;

define numeric A_PHYSICS = A_SUFFERCOLLISIONS | A_SUFFERPHYSICS;

// need better folder :(
define attributes(agent a) returning numeric {
        _caos {
                .targ < $a {
                        .inline let $return = ATTR;
                };
        };
}

define set attributes(numeric attr, agent a) {
        _caos {
                .targ < $a {
                        ATTR $attr(r);
                };
        };
}

// BHVR
define numeric B_ACTIVATE1 = 1;
define numeric B_ACTIVATE2 = 2;
define numeric B_DEACTIVATE = 4;
define numeric B_HIT = 8;
define numeric B_EAT = 16;
define numeric B_PICKUP = 32;

define behaviour(agent a) returning numeric {
        _caos {
                .targ < $a {
                        .inline let $return = BHVR;
                };
        };
}

define set behaviour(numeric attr, agent a) {
        _caos {
                .targ < $a {
                        BHVR $attr(r);
                };
        };
}
