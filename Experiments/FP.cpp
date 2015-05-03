#include <string>
#include <iostream>
#include <tuple>

namespace FP
{
	template<typename FirstType, typename... LeftTypes>
	FirstType Car(const std::tuple<FirstType, LeftTypes...>& source)
	{
		return std::get<0>(source);
	}
}

int main()
{
	auto tuple = std::make_tuple("one", 2);
	std::cout << FP::Car(tuple) << std::endl;

	return 0;
}