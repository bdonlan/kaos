install {
        agent a;

        a = newsimple(1, 1, 100, "oval", 1, 0, 100);
        a.attributes = A_PHYSICS | A_CARRYABLE | A_MOUSEABLE;
        move(a, 211, 196);
        // boolean pls?
        a.dcor = 1;
        a.perm = 51;
}
