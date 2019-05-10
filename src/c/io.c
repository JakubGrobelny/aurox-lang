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

static foreign_t pl_printf(term_t in)
{
    char* str;
    if (PL_get_atom_chars(in, &str))
    {
        printf("%s", str);
        PL_succeed;
    }

    PL_exception;
}

install_t install_io()
{ 
    PL_register_foreign("read_int", 1, pl_read_int, 0);
    PL_register_foreign("read_float", 1, pl_read_float, 0);
    PL_register_foreign("printf", 1, pl_printf, 0);
}