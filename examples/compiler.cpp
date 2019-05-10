#include <functional>

int unwrap(__constructor__<__constructor__<A>> x)
{
    
}


template <typename A, typename B>
std::function<list<B>(list<A>)> map(std::function<A(B)> f)
{
    return [=] (list<A> xs) -> list<B> {
        {
            if (matching(xs, {}, 0))
            {
                return EMPTY_LIST;
            }
        }
        {
            A _x;
            list<A> _xs;

            if (matching(xs, {&_x, &_xs}, 2))
            {
                return list<A>(f(_x), map(f, _xs));
            }
        }
        throw pmerror();
    };
}


list<int64_t> xs = {1, 2, 3, 4, 5};

int main(int argc, char* argv[])
{
    op_pipe_greater(op_pipe_greater(map(op_plus(10), xs), show), print);
    return 0;
}


