#include <algorithm>
#include <fstream>
#include <iostream>
#include <iterator>
#include <numeric>
#include <vector>

bool isOne(char C) { return C == '1'; }
std::size_t toBin(char C) { return isOne(C) ? 1 : 0; }
char toChar(bool B) { return B ? '1' : '0'; }

std::size_t binStr2Num(const std::string &Str) {
  return std::accumulate(std::begin(Str),
                         std::end(Str),
                         std::size_t{0},
                         [Shift = Str.length() - 1](std::size_t Acc, char C) mutable {
                           return Acc + (toBin(C) << Shift--);
                         });
}

std::size_t part1(std::vector<std::string> &Nums, std::size_t LineLength) {
  std::vector<std::size_t> Sums(LineLength, 0);

  for (std::size_t i = 0; i < LineLength; ++i)
    Sums[i] = std::count_if(std::cbegin(Nums), std::cend(Nums), [i](const std::string &S) { return isOne(S[i]); });

  std::string Gamma, Epsilon;
  for (std::size_t N: Sums) {
    bool OC = N * 2 > Nums.size();
    Gamma.push_back(toChar(OC));
    Epsilon.push_back(toChar(!OC));
  }

  return binStr2Num(Gamma) * binStr2Num(Epsilon);
}

template<bool MostCommon>
void partitionPart2(std::vector<std::string>::iterator &B, std::vector<std::string>::iterator &E) {
  for (std::size_t I = 1; std::distance(B, E) > 1; ++I) {
    std::size_t Sum =
        std::transform_reduce(B, E, std::size_t{0},
                              std::plus<>{},
                              [I](const std::string &S) { return isOne(S[I]); });
    E = std::partition(B, E,
                       [C = toChar(MostCommon == Sum * 2 >= std::distance(B, E)), I](
                           const std::string &S) {
                         return S[I] == C;
                       });

  }
}

std::size_t part2(std::vector<std::string> &Nums) {
  auto OGBegin = std::begin(Nums), OGEnd = std::end(Nums), COBegin = std::begin(Nums), COEnd = std::end(Nums);
  std::size_t SumI = std::transform_reduce(OGBegin, OGEnd, std::size_t{0},
                                           std::plus<>{},
                                           [](const std::string &S) { return S[0] == '1' ? 1 : 0; });
  OGEnd = COBegin = std::partition(OGBegin,
                                   OGEnd,
                                   [C = toChar(SumI * 2 >= Nums.size())](const std::string &S) {
                                     return S[0] == C;
                                   });

  partitionPart2<true>(OGBegin, OGEnd);
  partitionPart2<false>(COBegin, COEnd);

  return binStr2Num(*OGBegin) * binStr2Num(*COBegin);
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

  std::cout << "Part 1 = " << part1(Nums, LineLength) << "\n";
  std::cout << "Part 2 = " << part2(Nums) << "\n";

  return 0;
}
