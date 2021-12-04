#include <iostream>
#include <fstream>
#include <iterator>
#include <algorithm>
#include <numeric>
#include <vector>

void part1(std::vector<std::string> &Nums, std::size_t LineLength) {
  std::vector<int> Sums(LineLength, 0);

  for (const auto &N: Nums)
    for (std::size_t i = 0; i < LineLength; ++i)
      Sums[i] += N[i] == '1' ? 1 : 0;

  std::size_t Gamma = 0, Epsilon = 0;
  for (std::size_t i = 0, s = LineLength - 1; i < LineLength; ++i, --s) {
    bool OC = Sums[i] * 2 > Nums.size();
    Gamma += (OC ? 1 : 0) << s;
    Epsilon += (!OC ? 1 : 0) << s;
  }

  std::cout << "Part 1 = " << (Gamma * Epsilon) << "\n";
}

void part2(std::vector<std::string> &Nums, std::size_t LineLength) {
  auto OGBegin = std::begin(Nums), OGEnd = std::end(Nums), COBegin = std::begin(Nums), COEnd = std::end(Nums);
  std::size_t SumI =
      std::transform_reduce(OGBegin,
                            OGEnd,
                            std::size_t{0},
                            std::plus<>{},
                            [](const std::string &S) { return S[0] == '1' ? 1 : 0; });
  OGEnd = COBegin = std::partition(OGBegin,
                                   OGEnd,
                                   [K = SumI * 2 >= Nums.size()](const std::string &S) {
                                     return S[0] == (K ? '1' : '0');
                                   });

  for (std::size_t I = 1; std::distance(OGBegin, OGEnd) > 1; ++I) {
    std::size_t Sum =
        std::transform_reduce(OGBegin,
                              OGEnd,
                              std::size_t{0},
                              std::plus<>{},
                              [I](const std::string &S) { return S[I] == '1' ? 1 : 0; });
    OGEnd = std::partition(OGBegin,
                           OGEnd,
                           [K = Sum * 2 >= std::distance(OGBegin, OGEnd), I](const std::string &S) {
                             return S[I] == (K ? '1' : '0');
                           });

  }

  for (std::size_t I = 1; std::distance(COBegin, COEnd) > 1; ++I) {
    std::size_t Sum =
        std::transform_reduce(COBegin,
                              COEnd,
                              std::size_t{0},
                              std::plus<>{},
                              [I](const std::string &S) { return S[I] == '1' ? 1 : 0; });
    COEnd = std::partition(COBegin,
                           COEnd,
                           [K = Sum * 2 >= std::distance(COBegin, COEnd), I](const std::string &S) {
                             return S[I] == (!K ? '1' : '0');
                           });
  }

  const std::string &OGStr = *OGBegin;
  const std::string &COStr = *COBegin;
  std::size_t OGR = 0, COR = 0;
  for (std::size_t i = 0, s = LineLength - 1; i < LineLength; ++i, --s) {
    OGR += (OGStr[i] == '1' ? 1 : 0) << s;
    COR += (COStr[i] == '1' ? 1 : 0) << s;
  }

  std::cout << "Part 2 = " << (OGR * COR) << "\n";
}

int main(int argc, const char *argv[]) {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(nullptr);
  std::cout.tie(nullptr);

  std::fstream File;
  if (argc > 1)
    File.open(argv[1]);
  std::istream &Input = argc == 1 ? std::cin : File;

  std::vector<std::string> Nums;
  std::string S;
  while (File >> S)
    Nums.push_back(S);
  std::size_t LineLength = S.length();

  part1(Nums, LineLength);

  part2(Nums, LineLength);

  return 0;
}
