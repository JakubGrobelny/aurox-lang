#include <iostream>
#include <functional>
#include <memory>
#include <forward_list>
#include <cstdint>
#include <map>
#include <string>
#include <cstring>

template <typename T> struct __list_node; 
template <typename T>
struct __list
{
    std::shared_ptr<__list_node<T>> list = nullptr;

    bool is_empty()
    {
        return !this->list;
    }

    __list preprend(const T& val)
    {
        std::shared_ptr<__list_node<T>> node(new __list_node<T>(val, *this));
        return __list(node);
    }

    T head()
    {
        return this->list->head;
    }

    __list tail()
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

    __list<int> xs = {1,2,3,4};

    auto ys = xs;
    while (!ys.is_empty())
    {
        std::cout << ys.head() << std::endl;
        ys = ys.tail();
    }

    return 0;
}
