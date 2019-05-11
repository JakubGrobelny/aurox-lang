#include <iostream>
#include <functional>
#include <memory>
#include <forward_list>
#include <cstdint>
#include <map>
#include <string>
#include <cstring>
#include <tuple>

enum class unit
{
    unit
};

template <typename T> struct __list_node; 
template <typename T>
struct __list
{
    std::shared_ptr<__list_node<T>> list = nullptr;

    bool is_empty() const
    {
        return !this->list;
    }

    __list preprend(const T& val) const
    {
        std::shared_ptr<__list_node<T>> node(new __list_node<T>(val, *this));
        return __list(node);
    }

    T head() const 
    {
        return this->list->head;
    }

    __list tail() const
    {
        return this->list->tail;
    }

    __list(std::initializer_list<T> xs)
    {
        this->list.reset(new __list_node(*xs.begin()));
        __list_node<T>* ptr = this->list.get();

        for (auto iter = xs.begin() + 1; iter < xs.end(); iter++)
        {
            ptr->tail.list = std::shared_ptr<__list_node<T>>(
                new __list_node(*iter)
            );

            ptr = ptr->tail.list.get();
        }

        ptr->tail.list = nullptr;
    }

    __list() : list(nullptr) {};

    __list(T val)
        : list(new __list_node<T>(val)) {};

    __list(std::shared_ptr<__list_node<T>> node)
        : list(node) {};
};

template <typename T>
struct __list_node
{
    T head;
    __list<T> tail;

    __list_node(const T& head, __list<T> tail)
        : head(head), tail(tail) {};

    __list_node(const T& head) 
        : head(head), tail(__list<T>(nullptr)) {};
};


template <typename T>
struct __val_or_var
{
    enum element_type { var, val };
    element_type type;

    union
    {
        T elem;
        T* ptr;
    };

    bool is_var() const
    {
        return this->type == element_type::var;
    }

    __val_or_var() = delete;
    __val_or_var(T* elem)
        : ptr(elem), type(element_type::var) {};
    __val_or_var(const T& elem)
        : elem(elem), type(element_type::val) {};
};

template <typename T>
struct __constructor
{
    const char* name;
    T value;

    __constructor(const char* name, T value)
        : name(name), value(value) {};
};

struct __enum
{
    const char* name;
    __enum(const char* name = nullptr)
        : name(name) {};
};

bool matching(const __enum& val, __enum&& pattern)
{
    return !strcmp(val.name, pattern.name);
}

template <typename A, typename B>
bool matching(const __constructor<A>& val, __constructor<B>&& pattern)
{
    if (strcmp(val.name, pattern.name))
        return false;
    return matching(val.value, pattern.value);
}

template <typename A, typename B>
bool matching(const __constructor<A>& val, B&& pattern)
{
    return false;
}

template <typename T>
bool matching(const T& val, T* var)
{
    (*var) = val;
    return true;
}

template <typename A, typename B>
bool matching (const A& l, B&& r) 
{
    return l == r;
}

template <typename A>
bool matching(
    const __list<A>& xs, 
    std::initializer_list<__val_or_var<A>> pattern,
    __list<A>* tail
) {
    auto ys = xs;
    auto iter = pattern.begin();

    while (!ys.is_empty())
    {
        if (iter == pattern.end())
        {
            if (!tail)
                return false;
            (*tail) = ys;
            return true;
        }
        else if (iter->is_var())
        {
            if (!matching(ys.head(), iter->ptr))
                return false;
        }
        else
        {
            if (!matching(ys.head(), iter->elem))
                return false;
        }

        ys = ys.tail();
        iter++;
    }

    return false;
}

template <typename T>
std::ostream& operator<<(std::ostream& out, __list<T>& xs) 
{
    out << '[';
    while (!xs.is_empty())
    {
        std::cout << xs.head() << ", ";
        xs = xs.tail();
    }
    out << "\b\b]";

    return out;
}

int main()
{
    const char* nothing = "Nothing";
    const char* something = "Something";
    const char* just = "Just";
    __enum e;

    std::cout << matching(
        __constructor(just, __enum(nothing)),
        __constructor(just, &e)
    ) << std::endl;

    std::cout << e.name << std::endl;

    __list<int> xs = {1,2,3,4,5,6,7,8};


    int first  = -1;
    int third = -1;
    __list<int> tail;

    std::cout << std::endl << matching(
        xs,
        {__val_or_var(&first), __val_or_var(2), __val_or_var(&third)},
        &tail
    ) << std::endl;

    std::cout << first << ' ' << third << std::endl;
    std::cout << tail << std::endl;

    return 0;
}
