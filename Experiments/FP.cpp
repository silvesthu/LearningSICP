#include <string>
#include <iostream>
#include <tuple>

namespace FP
{
	// http://en.wikipedia.org/wiki/Variadic_template

	template<typename FirstType, typename... LeftTypes>
	FirstType Car(const std::tuple<FirstType, LeftTypes...>& source)
	{
		return std::get<0>(source);
	}

	// ref: https://gist.github.com/mattbierner/6145505

	// type to holder a list of size_t
	template<std::size_t... XS>
	struct Seq
	{
	    template<std::size_t X>
	    using cons = Seq<X, XS...>;
	};
	 
	template<std::size_t start, std::size_t end>
	struct Range
	{
	    static_assert(start < end, "Range: start > end");
	    typedef
	        typename Range<start + 1, end>::type::template cons<start> // what's the ::template part ???
	        type;
	};
	 
	template<std::size_t start>
	struct Range<start, start>
	{
	    typedef Seq<> type;
	};

	template<typename T, std::size_t... N>
	std::tuple<typename std::tuple_element<N, T>::type...> Select(const T& t, Seq<N...>)
	{
		// parameter unpack
		// expand to -> std::get<0>(t), std::get<1>(t), ..., std::get<N>(t)
	    return std::make_tuple(std::get<N>(t)...); 
	}

	// std::tuple_size<TupleType>::value>::type -> std::integral_constant<std::size_t, value>

	// R -> std::integral_constant<std::size_t, value>

	// R() -> value

	template<typename TupleType, typename R = typename Range<1, std::tuple_size<TupleType>::value>::type> // here R should be a type
	auto Cdr(const TupleType& t) -> decltype(Select(t, R())) // make a temp var of R as function parameter
	// use decltype to declare return type before it is deduced
	{
	    return Select(t, R());
	} 
}

namespace MyRange
{
	template<int... left>
	struct S
	{
		template<int first>
		using cons = S<first, left...>;
	};

	template<int i, int j>
	struct A
	{
	    typedef typename A<i+1, j>::type::template cons<i> type;
	};

	// A<j, j> -> S<>
	// A<j-1, j> -> S<j-1>
	// A<j-2, j> -> S<j-2, j-1>
	// ...
	// A<i, j> -> S<i, ..., j>

	template<int i>
	struct A<i, i>
	{
		// Teminate
	    typedef S<> type;
	};

	template<int... i>
	void Print(S<i...> s)
	{
		Out(s);
		std::cout << std::endl;
	}

	template<int i, int... j>
	void Out(S<i, j...>)
	{
		std::cout << i;
		Out(S<j...>()); // Create a temp var of type S, with template parameters left (where data is stored)
	}

	template<>
	void Out(S<>)
	{
		// Terminate
	}
}

int main()
{
	auto empty = std::make_tuple();
	auto one = std::make_tuple("one");
	auto two = std::make_tuple("one", 2);
	auto three = std::make_tuple("one", 2, "three");

	std::cout << FP::Car(two) << std::endl;
	std::cout << FP::Car(FP::Cdr(two)) << std::endl;
	std::cout << FP::Car(FP::Cdr(FP::Cdr(three))) << std::endl;

	std::cout << std::endl;

	MyRange::A<0, 10>::type range; // S<0, 1, 2, ..., N>
	MyRange::Print(range);

	return 0;
}