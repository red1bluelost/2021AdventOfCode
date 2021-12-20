#include <algorithm>
#include <fstream>
#include <iostream>
#include <iterator>
#include <vector>
#include <string>
#include <numeric>
#include <utility>
#include <cassert>
#include <set>

struct Point {
  int X, Y, Z;
  constexpr auto operator<=>(const Point &) const = default;
  constexpr Point operator+(const Point &P) const { return {X + P.X, Y + P.Y, Z + P.Z}; }

  Point relate(Point P) const { return {P.X - X, P.Y - Y, P.Z - Z}; }
  Point reorient(int I) const;

  int manhattanDistance(Point P) const { return std::abs(P.X - X) + std::abs(P.Y - Y) + std::abs(P.Z - Z); }
};

template<class CharT, class Traits>
std::basic_ostream<CharT, Traits> &operator<<(std::basic_ostream<CharT, Traits> &OS, const Point &P) {
  return OS << "(" << P.X << "," << P.Y << "," << P.Z << ")";
}

struct Scanner {
  std::vector<Point> Beacons;
  Point RelativeToZero{0, 0, 0};

  std::vector<Point> transpose(int Orientation, Point Base) const {
    std::vector<Point> Copy(Beacons.size());
    std::transform(std::cbegin(Beacons), std::cend(Beacons), std::begin(Copy),
                   std::bind(&Point::relate, Base, std::placeholders::_1));
    std::transform(std::cbegin(Copy), std::cend(Copy), std::begin(Copy),
                   std::bind(&Point::reorient, std::placeholders::_1, Orientation));
    return Copy;
  }

  static bool overlap(Scanner &Base, Scanner &Other);
};

bool Scanner::overlap(Scanner &Base, Scanner &Other) {
  for (int OO = 0; OO < 24; ++OO) {
    for (std::size_t BaseI = 0; BaseI < Base.Beacons.size(); BaseI++) {
      for (std::size_t OtherI = 0; OtherI < Other.Beacons.size(); OtherI++) {
        std::vector<Point> B = Base.transpose(0, Base.Beacons[BaseI]);
        std::vector<Point> O = Other.transpose(OO, Other.Beacons[OtherI]);
        std::vector<std::size_t> BM;
        std::vector<std::size_t> OM;
        for (std::size_t BI = 0; BI < B.size(); BI++) {
          for (std::size_t OI = 0; OI < O.size(); OI++) {
            if (B[BI] == O[OI]) {
              BM.push_back(BI);
              OM.push_back(OI);
            }
          }
        }
        if (BM.size() == 12) {
          std::cout << "Detected with " << OO << std::endl;
          Point ORB = Other.Beacons[OM.front()].reorient(OO).relate(Base.Beacons[BM.front()]);
          Other.RelativeToZero = ORB;
          std::transform(std::cbegin(Other.Beacons), std::cend(Other.Beacons), std::begin(Other.Beacons),
                         [ORB, OO](Point V) { return ORB + V.reorient(OO); });
          return true;
        }
      }
    }
  }
  return false;
}

int main(int argc, const char *argv[]) {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(nullptr);
  std::cout.tie(nullptr);

  std::fstream File;
  if (argc > 1)
    File.open(argv[1]);
  std::istream &Input = argc == 1 ? std::cin : File;

  std::vector<Scanner> Scanners;
  while (Input) {
    std::string S;
    std::getline(Input, S);
    std::vector<Point> Points;
    while (std::getline(Input, S)) {
      int X, Y, Z;
      if (std::sscanf(S.c_str(), "%d,%d,%d", &X, &Y, &Z) != 3)
        break;
      Points.push_back({X, Y, Z});
    }
    Scanners.push_back({std::move(Points)});
  }

  std::set<std::size_t> Located = {0};
  while (Located.size() != Scanners.size())
    for (std::size_t L: Located)
      for (std::size_t I = 0; I < Scanners.size(); ++I)
        if (!Located.contains(I) && Scanner::overlap(Scanners[L], Scanners[I]))
          Located.insert(I);

  std::vector<Point> AllBeacons;
  for (const Scanner &S: Scanners)
    std::copy(std::begin(S.Beacons), std::end(S.Beacons), std::back_inserter(AllBeacons));
  std::cout << "Part 1 = " << std::set<Point>(std::begin(AllBeacons), std::end(AllBeacons)).size() << std::endl;

  int Max = 0;
  for (std::size_t I = 0; I < Scanners.size(); ++I)
    for (std::size_t J = 0; J < Scanners.size(); ++J)
      Max = std::max(Max, Scanners[I].RelativeToZero.manhattanDistance(Scanners[J].RelativeToZero));

  std::cout << "Part 2 = " << Max << std::endl;

  return 0;
}

Point Point::reorient(int I) const {
  assert(I >= 0 && I < 24 && "Only 24 for possible options.");
  switch (I) {
  case 0:return {X, Y, Z};
  case 1:return {X, -Z, Y};
  case 2:return {X, -Y, -Z};
  case 3:return {X, Z, -Y};
  case 4:return {-X, -Y, Z};
  case 5:return {-X, -Z, -Y};
  case 6:return {-X, Y, -Z};
  case 7:return {-X, Z, Y};
  case 8:return {Y, -X, Z};
  case 9:return {Y, -Z, -X};
  case 10:return {Y, X, -Z};
  case 11:return {Y, Z, X};
  case 12:return {-Y, X, Z};
  case 13:return {-Y, -Z, X};
  case 14:return {-Y, -X, -Z};
  case 15:return {-Y, Z, -X};
  case 16:return {Z, Y, -X};
  case 17:return {Z, -X, -Y};
  case 18:return {Z, -Y, X};
  case 19:return {Z, X, Y};
  case 20:return {-Z, Y, X};
  case 21:return {-Z, -X, Y};
  case 22:return {-Z, -Y, -X};
  case 23:return {-Z, X, -Y};
  }
  __builtin_unreachable();
}
