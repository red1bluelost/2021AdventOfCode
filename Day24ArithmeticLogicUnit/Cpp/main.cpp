#include <array>
#include <iostream>

namespace {
struct Block {
  int Div;
  int Check;
  int Offset;
};

constexpr std::array<Block, 14> MyBlocks = {
    {
        {1, 10, 10},
        {1, 13, 5},
        {1, 15, 12},
        {26, -12, 12},
        {1, 14, 16},
        {26, -2, 4},
        {1, 13, 15},
        {26, -12, 3},
        {1, 15, 7},
        {1, 11, 11},
        {26, -3, 2},
        {26, -13, 12},
        {26, -12, 4},
        {26, -13, 11}
    }
};

int runBlock(int Z, int I, Block Blk) {
  int A = ((Z % 26) + Blk.Check) != I ? 1 : 0;
  int B = A ? 26 : 1;
  int C = (Z / Blk.Div) * B;
  int D = A ? (I + Blk.Offset) : 0;
  return C + D;
}

int runInput(std::array<int, 14> Input) {
  int Z = 0;
  for (std::size_t I = 0; I < Input.size(); ++I) {
    Z = runBlock(Z, Input[I], MyBlocks[I]);
  }
  return Z;
}

std::array<int, 14> nextLargestNumber(std::array<int, 14> N) {
  for (std::size_t I = N.size() - 1; I >= std::size_t{0}; --I) {
    N[I] -= 1;
    if (N[I] != 0)
      return N;
    N[I] = 9;
  }
  __builtin_unreachable();
}

template<class CharT, class Traits>
std::basic_ostream<CharT, Traits> &
operator<<(std::basic_ostream<CharT, Traits> &OS, const std::array<int, 14> &N) {
  for (int I: N)
    OS << I;
  return OS;
}

std::array<int, 14> largestInput() {
  std::array<int, 14> Number = {9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9};
  while (true) {
    if (runInput(Number) == 0) {
      return Number;
    }
    Number = nextLargestNumber(Number);
  }
  __builtin_unreachable();
}
} // namespace

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(nullptr);
  std::cout.tie(nullptr);

  auto Res = largestInput();
  std::cout << "Part 1 = " << Res;
  return 0;
}