#include <stdio.h>
#include <SWI-Prolog.h>

static foreign_t pl_read_int(term_t to)
{ 
    if (PL_is_variable(to))
    {
        long n;
        if (!scanf("%ld", &n))
            PL_fail;

        return PL_unify_int64(to, n);
    }

    PL_fail;
}

static foreign_t pl_read_float(term_t to)
{ 
    if (PL_is_variable(to))
    {
        double x;
        if (!scanf("%lf", &x))
            PL_fail;

        return PL_unify_float(to, x);
    }

    PL_fail;
}

install_t install_io()
{ 
    PL_register_foreign("read_int", 1, pl_read_int, 0);
    PL_register_foreign("read_float", 1, pl_read_float, 0);
}