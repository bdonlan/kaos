install {
        agent a;

        a = newsimple(1, 1, 100, "oval", 1, 0, 100);
        # constants which we can or together would be nice for this
        attributes(a) = 128 | 64 | 2 | 1;
        move(a, 211, 196);
        # boolean pls?
        dcor(a) = 1;
        perm(a) = 32 | 16 | 2 | 1;
}
