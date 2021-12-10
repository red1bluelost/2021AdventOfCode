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

int part1(const std::vector<std::vector<int>> &Heights, const std::size_t X, const std::size_t Y) {
  std::size_t L = Heights.size(), W = Heights[0].size();
  int V = Heights[X][Y];
  if (X != std::size_t{0} && V >= Heights[X - 1][Y])
    return 0;
  if (Y != std::size_t{0} && V >= Heights[X][Y - 1])
    return 0;
  if (X != L - 1 && V >= Heights[X + 1][Y])
    return 0;
  if (Y != W - 1 && V >= Heights[X][Y + 1])
    return 0;
  return V + 1;
}

int basinSize(const std::vector<std::vector<int>> &Heights,
              const std::size_t L,
              const std::size_t W,
              const std::size_t X,
              const std::size_t Y,
              std::vector<std::vector<bool>> &Visited) {
  if (X < std::size_t{0} || Y < std::size_t{0} || X >= L || Y >= W || Visited[X][Y])
    return 0;
  Visited[X][Y] = true;
  int V = Heights[X][Y];
  if (V == 9)
    return 0;

  auto B = [&](auto I, auto J) { return basinSize(Heights, L, W, I, J, Visited); };
  return 1 + B(X - 1, Y) + B(X, Y - 1) + B(X + 1, Y) + B(X, Y + 1);
}

int part2(const std::vector<std::vector<int>> &Heights) {
  std::size_t L = Heights.size(), W = Heights[0].size();
  std::vector<std::vector<bool>> Visited(L, std::vector<bool>(W, false));
  std::array<int, 3> MaxSizes{0, 0, 0};

  for (std::size_t X = 0; X < Heights.size(); ++X) {
    for (std::size_t Y = 0; Y < Heights[X].size(); ++Y) {
      int Basin = basinSize(Heights, L, W, X, Y, Visited);
      for (int &I: MaxSizes) {
        if (Basin > I) {
          I = Basin;
          break;
        }
      }
      std::sort(std::begin(MaxSizes), std::end(MaxSizes));
    }
  }

  return std::accumulate(std::begin(MaxSizes), std::end(MaxSizes), 1, std::multiplies{});
}

int main(int argc, const char *argv[]) {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(nullptr);
  std::cout.tie(nullptr);

  std::fstream File;
  if (argc > 1)
    File.open(argv[1]);
  std::istream &Input = argc == 1 ? std::cin : File;

  std::vector<std::vector<int>> Heights;

  for (std::string S; std::getline(Input, S);) {
    if (S[S.length() - 1] == '\r' || S[S.length() - 1] == '\n')
      S.pop_back();
    Heights.emplace_back(S.length());
    std::transform(std::begin(S),
                   std::end(S),
                   std::begin(Heights[Heights.size() - 1]),
                   [](char C) -> int { return C - '0'; });
  }

  int Part1 = 0;

  for (std::size_t X = 0; X < Heights.size(); ++X)
    for (std::size_t Y = 0; Y < Heights[X].size(); ++Y)
      Part1 += part1(Heights, X, Y);

  std::cout << "Part 1 = " << Part1 << "\n";

  std::cout << "Part 2 = " << part2(Heights) << "\n";

  return 0;
}
