#include <algorithm>
#include <fstream>
#include <iostream>
#include <iterator>
#include <sstream>
#include <vector>
#include <string>
#include <numeric>
#include <utility>

struct Point {
  int X, Y;
  bool operator<=>(const Point &) const = default;
};

template<class CharT, class Traits>
std::basic_ostream<CharT, Traits> &operator<<(std::basic_ostream<CharT, Traits> &OS, const Point &P) {
  return OS << "(" << P.X << "," << P.Y << ")";
}

struct Line {
  Point Start, End;

  bool isFlatOnX() const { return Start.Y == End.Y; }
  bool isFlatOnY() const { return Start.X == End.X; }
  bool isIncreasingOnX() const { return Start.X <= End.X; }
  bool isIncreasingOnY() const { return Start.Y <= End.Y; }

  class Iterator {
    friend Line;
    const Line &Parent;
    Point Value;
    Iterator(const Line &Par, Point P) : Parent(Par), Value(P) {}
  public:
    Point operator*() { return Value; }
    Iterator &operator++() {
      if (Value == Parent.End)
        Value = {-1, -1};
      else if (Parent.isFlatOnX())
        Value = {Value.X + (Parent.isIncreasingOnX() ? 1 : -1), Value.Y};
      else if (Parent.isFlatOnY())
        Value = {Value.X, Value.Y + (Parent.isIncreasingOnY() ? 1 : -1)};
      else
        Value = {Value.X + (Parent.isIncreasingOnX() ? 1 : -1), Value.Y + (Parent.isIncreasingOnY() ? 1 : -1)};
      return *this;
    }
    bool operator==(const Iterator &Other) const { return Value == Other.Value; };
  };

  Iterator begin() const { return Iterator(*this, Start); }
  Iterator end() const { return Iterator(*this, {-1, -1}); }
};

template<class CharT, class Traits>
std::basic_ostream<CharT, Traits> &operator<<(std::basic_ostream<CharT, Traits> &OS, const Line &L) {
  return OS << L.Start << " -> " << L.End;
}

Point readLines(std::istream &Input, std::vector<Line> &Lines) {
  Point BottomRight = {0, 0};
  for (std::string S; std::getline(Input, S);) {
    std::stringstream SS{S};
    Line L;
    auto Updater = [](const std::string &N, int &Assign, int &Bound) {
      int I = std::stoi(N);
      Assign = I;
      Bound = std::max(Bound, I);
    };
    // "x1,y1 -> x2,y2\n"
    if (std::string N; std::getline(SS, N, ','))
      Updater(N, L.Start.X, BottomRight.X);
    if (std::string N; std::getline(SS, N, ' '))
      Updater(N, L.Start.Y, BottomRight.Y);
    if (std::string N; std::getline(SS, N, ' '))
      (void) 0;
    if (std::string N; std::getline(SS, N, ','))
      Updater(N, L.End.X, BottomRight.X);
    if (std::string N; std::getline(SS, N, '\n'))
      Updater(N, L.End.Y, BottomRight.Y);
    Lines.push_back(L);
  }
  return {BottomRight.X + 1, BottomRight.Y + 1};
}

void printPlot(const std::vector<std::vector<int>> &Plot) {
  for (std::size_t J = 0; J < Plot.size(); ++J) {
    for (std::size_t I = 0; I < Plot[J].size(); ++I) {
      if (Plot[I][J] > 0)
        std::cout << Plot[I][J];
      else
        std::cout << ".";
    }
    std::cout << "\n";
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

  std::vector<Line> Lines;
  Point Dimensions = readLines(Input, Lines);

  std::vector<std::vector<int>> Plot1(Dimensions.X, std::vector<int>(Dimensions.Y, 0));
  std::vector<std::vector<int>> Plot2(Dimensions.X, std::vector<int>(Dimensions.Y, 0));

  for (const Line &L: Lines)
    if (L.isFlatOnX() || L.isFlatOnY())
      for (const Point P: L)
        Plot1[P.X][P.Y]++, Plot2[P.X][P.Y]++;
    else
      for (const Point P: L)
        Plot2[P.X][P.Y]++;

  auto Counter = [](const std::vector<std::vector<int>> &Plot) {
    return std::accumulate(std::cbegin(Plot), std::cend(Plot), 0,
                           [](int Acc, const std::vector<int> &V) {
                             return Acc + std::count_if(std::cbegin(V),
                                                        std::cend(V),
                                                        [](int I) { return I >= 2; });
                           });
  };

  int Part1 = Counter(Plot1);
  int Part2 = Counter(Plot2);

  std::cout << "Part 1 = " << Part1 << "\n";
  std::cout << "Part 2 = " << Part2 << "\n";

  return 0;
}
