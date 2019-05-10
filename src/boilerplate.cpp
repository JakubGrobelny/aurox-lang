#include <iostream>
#include <functional>
#include <memory>
#include <forward_list>
#include <cstdint>
#include <map>
#include <string>
#include <cstring>

template <typename T>
struct __constructor
{
    const char* name;
    T value;

    __constructor() = delete;
    __constructor(const char* name, T value)
        : name(name), value(value) {};
};

template <typename T>
struct __enum
{
    const char* name;
    __enum() = delete;
    __enum(const char* name)
        : name(name) {};
};

template <typename A, typename B>
bool matching(const __constructor<A>& val, const B& pattern)
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
bool matching (const A& l, B& r) 
{
    return l == r;
}

template <typename A, typename B>
bool matching(const __constructor<A>& val, __constructor<B>& pattern)
{
    if (strcmp(val.name, pattern.name))
        return false;
    return matching(val.value, pattern.value);
}


int main()
{
    int x;
    auto c = __constructor("Just", &x);
    std::cout << matching(__constructor("Just", 42), c) << std::endl;

    std::cout << x << std::endl;

    return 0;
}
