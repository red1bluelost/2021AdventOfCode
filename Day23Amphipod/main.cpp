#include <algorithm>
#include <array>
#include <fstream>
#include <iostream>
#include <iterator>
#include <sstream>
#include <vector>
#include <string>
#include <numeric>
#include <utility>
#include <ranges>
#include <cassert>
#include <memory>
#include <deque>
#include <map>

using Pair = std::pair<std::size_t, std::size_t>;

const std::map<char, int> COST{{'A', 1}, {'B', 10}, {'C', 100}, {'D', 1000}};
const std::array<std::size_t, 7> VALD{1, 2, 4, 6, 8, 10, 11};
auto adj(std::size_t X, std::size_t Y) -> std::vector<Pair> {
  return {{X + 1, Y}, {X - 1, Y}, {X, Y + 1}, {X, Y - 1}};
}

void findRoute(const std::map<Pair, char>& Grid, const int Total, std::map)

int main(int argc, const char *argv[]) {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(nullptr);
  std::cout.tie(nullptr);

  std::fstream File;
  if (argc > 1)
    File.open(argv[1]);
  std::istream &Input = argc == 1 ? std::cin : File;

  std::vector<std::string> Data = std::invoke([&Input] {
    std::vector<std::string> SV;
    std::string S;
    while (std::getline(Input, S)) {
      if (S.back() == '\r')
        S.pop_back();
      SV.push_back(S);
    }
    return SV;
  });
  for (auto &S: Data)
    if (S.length() != Data.front().length())
      S += std::string(Data.front().length() - S.length(), ' ');

  std::map<Pair, char> Grid = std::invoke([&Data] {
    std::map<Pair, char> Grid;
    for (std::size_t Y = 0; Y < Data.size(); ++Y)
      for (std::size_t X = 0; X < Data.front().size(); ++X)
        if (Data[Y][X] == '.' || (Data[Y][X] >= 'A' && Data[Y][X] <= 'D'))
          Grid[std::make_pair(X, Y)] = Data[Y][X];
    return Grid;
  });



  return 0;
}