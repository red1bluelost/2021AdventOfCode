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

class Octopi {
  std::size_t L, W;
  std::vector<std::vector<int>> Levels;
  std::vector<std::vector<bool>> Flashed;

  int CurrentCount;

  void clearFlashed() {
    for (auto &R: Flashed)
      for (std::vector<bool>::reference B: R)
        B = false;
  }

  void visit(std::size_t X, std::size_t Y) {
    if (X < std::size_t{0} || Y < std::size_t{0} || X >= L || Y >= W || Flashed[X][Y])
      return;
    int &V = Levels[X][Y];
    V++;
    if (V > 9) {
      V = 0;
      Flashed[X][Y] = true;
      CurrentCount++;
      visitNeighbors(X, Y);
    }
  }

  void visitNeighbors(std::size_t X, std::size_t Y) {
    visit(X - 1, Y - 1);
    visit(X, Y - 1);
    visit(X + 1, Y - 1);
    visit(X - 1, Y);
    visit(X + 1, Y);
    visit(X - 1, Y + 1);
    visit(X, Y + 1);
    visit(X + 1, Y + 1);
  }

public:
  Octopi(std::vector<std::vector<int>> I) : L{I.size()}, W{I.front().size()}, Levels(I),
                                            Flashed(I.size(), std::vector<bool>(I.front().size(), false)) {}

  int step() {
    clearFlashed();
    CurrentCount = 0;
    for (std::size_t I = 0; I < L; ++I) {
      for (std::size_t J = 0; J < W; ++J) {
        visit(I, J);
      }
    }
    return CurrentCount;
  }

  bool didAllFlash() {
    return std::ranges::all_of(Flashed,
                               [](const auto &R) -> bool { return std::ranges::all_of(R, std::identity{}); });

  }
};

int main(int argc, const char *argv[]) {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(nullptr);
  std::cout.tie(nullptr);

  std::fstream File;
  if (argc > 1)
    File.open(argv[1]);
  std::istream &Input = argc == 1 ? std::cin : File;

  std::vector<std::vector<int>> Levels;

  for (std::string S; std::getline(Input, S);) {
    if (S[S.length() - 1] == '\r' || S[S.length() - 1] == '\n')
      S.pop_back();
    Levels.emplace_back(S.length());
    std::transform(std::begin(S),
                   std::end(S),
                   std::begin(Levels[Levels.size() - 1]),
                   [](char C) -> int { return C - '0'; });
  }

  Octopi O(Levels);

  int P1 = 0;
  int SyncStep = -1;
  int I;
  for (I = 1; I <= 100; ++I) {
    P1 += O.step();
    if (SyncStep == -1 && O.didAllFlash()) {
      SyncStep = I;
    }
  }

  std::cout << "Part 1 = " << P1 << "\n";

  if (SyncStep == -1) {
    while (!O.didAllFlash())
      O.step(), I++;
    SyncStep = I - 1;
  }

  std::cout << "Part 2 = " << SyncStep << "\n";

  return 0;
}
