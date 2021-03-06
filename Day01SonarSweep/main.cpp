#include <iostream>
#include <fstream>
#include <iterator>
#include <algorithm>
#include <numeric>
#include <vector>

#ifndef PART
#define PART 1
#endif

int main(int argc, const char *argv[]) {
  std::fstream File;
  if (argc > 1)
    File.open(argv[1]);

  std::istream &Input = argc == 1 ? std::cin : File;

  std::vector<int> Sonar;

  int N;
  while (Input >> N)
    Sonar.push_back(N);

#if PART == 1
  int Result = std::inner_product(std::cbegin(Sonar),
                                  std::next(std::cend(Sonar), -1),
                                  std::next(std::cbegin(Sonar)),
                                  int{0},
                                  std::plus<int>{},
                                  [](int L, int R) {
                                    return L < R ? 1 : 0;
                                  });

#elif PART == 2
  for (int i = 0; i < Sonar.size() - 2; ++i)
    Sonar[i] = Sonar[i] + Sonar[i + 1] + Sonar[i + 2];

  int Result = std::inner_product(std::cbegin(Sonar),
                                  std::next(std::cend(Sonar), -3),
                                  std::next(std::cbegin(Sonar)),
                                  int{0},
                                  std::plus<int>{},
                                  [](int L, int R) {
                                    return L < R ? 1 : 0;
                                  });
#endif

  std::cout << Result << "\n";

  return 0;
}
