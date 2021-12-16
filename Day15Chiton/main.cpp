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

class Matrix {
  std::size_t L, W;
  std::vector<int> Data;

public:
  Matrix(std::size_t Li, std::size_t Wi, std::vector<int> &&Di) : L{Li}, W{Wi}, Data{std::move(Di)} {}

  int *at(std::size_t X, std::size_t Y) {
    if (X < std::size_t{0} || Y < std::size_t{0} || X >= L || Y >= W)
      return nullptr;
    return &Data[X * L + Y];
  }

  const int *at(std::size_t X, std::size_t Y) const {
    if (X < std::size_t{0} || Y < std::size_t{0} || X >= L || Y >= W)
      return nullptr;
    return &Data[X * L + Y];
  }

  const int &front() const { return Data.front(); }
  const int &back() const { return Data.back(); }
};

void solve(std::size_t L, std::size_t W, const Matrix &OrigMat, Matrix &DMat) {
  for (bool Changed = true; Changed;) {
    Changed = false;
    for (std::size_t I = 0; I < L; ++I) {
      for (std::size_t J = 0; J < W; ++J) {
        int *D = DMat.at(I, J);
        const int *V = OrigMat.at(I, J);
        auto Move = [&](std::size_t X, std::size_t Y) -> bool {
          if (int *A = DMat.at(X, Y); A && *V + *A < *D) {
            *D = *V + *A;
            return true;
          }
          return false;
        };
        Changed |= Move(I - 1, J);
        Changed |= Move(I + 1, J);
        Changed |= Move(I, J - 1);
        Changed |= Move(I, J + 1);
      }
    }
  }
}

int main(int argc, const char *argv[]) {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(nullptr);
  std::cout.tie(nullptr);

  std::fstream File;
  if (argc > 1)
    File.open(argv[1]);
  std::istream &Input = argc == 1 ? std::cin : File;

  std::vector<int> ValsIn;
  std::size_t L = 0, W = 0;
  for (std::string S; std::getline(Input, S); L++) {
    if (S[S.length() - 1] == '\r' || S[S.length() - 1] == '\n')
      S.pop_back();
    std::transform(std::begin(S),
                   std::end(S),
                   std::back_inserter(ValsIn),
                   [](char C) -> int { return C - '0'; });
    W = S.length();
  }

  std::vector<int> ValsIn5x;
  ValsIn5x.reserve(L * 5 * W * 5);
  for (std::size_t I = 0; I < L; ++I)
    for (std::size_t J = 0; J < 5; ++J)
      if (J == 0)
        std::copy_n(std::next(std::begin(ValsIn), I * W),
                    W,
                    std::back_inserter(ValsIn5x));
      else
        std::transform(std::next(std::end(ValsIn5x), -W),
                       std::end(ValsIn5x),
                       std::back_inserter(ValsIn5x),
                       [](int I) { return I == 9 ? 1 : I + 1; });
  for (std::size_t J = 1; J < 5; ++J)
    std::transform(std::next(std::end(ValsIn5x), -W * 5 * L),
                   std::end(ValsIn5x),
                   std::back_inserter(ValsIn5x),
                   [](int I) { return I == 9 ? 1 : I + 1; });

  Matrix DMat(L, W, std::vector<int>(ValsIn.size(), std::numeric_limits<int>::max() - 11));
  Matrix OrigMat(L, W, std::move(ValsIn));
  *DMat.at(0, 0) = *OrigMat.at(0, 0);

  solve(L, W, OrigMat, DMat);
  std::cout << "Part 1 = " << (DMat.back() - DMat.front()) << "\n";

  Matrix DMat5x(L * 5, W * 5, std::vector<int>(ValsIn5x.size(), std::numeric_limits<int>::max() - 11));
  Matrix OrigMat5x(L * 5, W * 5, std::move(ValsIn5x));
  *DMat5x.at(0, 0) = *OrigMat5x.at(0, 0);

  solve(L * 5, W * 5, OrigMat5x, DMat5x);
  std::cout << "Part 1 = " << (DMat5x.back() - DMat5x.front()) << "\n";

  return 0;
}
