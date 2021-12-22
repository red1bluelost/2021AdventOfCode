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

struct Cube {
  long long Xs, Xe, Ys, Ye, Zs, Ze;
  bool On;

  constexpr auto operator<=>(const Cube &) const = default;
  [[nodiscard]] constexpr long long size() const {
    assert(Xe >= Xs && Ye >= Ys && Ze >= Zs && "Start is greater than end.");
    return (Xe - Xs + 1) * (Ye - Ys + 1) * (Ze - Zs + 1);
  }
  [[nodiscard]] constexpr bool overlap(Cube O) const {
    return !(Xs > O.Xe || Xe < O.Xs || Ys > O.Ye || Ye < O.Ys || Zs > O.Ze || Ze < O.Zs);
  }

  std::vector<Cube> split(Cube O) const {
    std::vector<Cube> Cuts;
    Cube ToCut = *this;
    if (O.Xs > ToCut.Xs && O.Xs <= ToCut.Xe) {
      Cube Cut = ToCut;
      Cut.Xe = O.Xs - 1;
      Cuts.push_back(Cut);
      ToCut.Xs = O.Xs;
    }
    if (O.Xe >= ToCut.Xs && O.Xe < ToCut.Xe) {
      Cube Cut = ToCut;
      Cut.Xs = O.Xe + 1;
      Cuts.push_back(Cut);
      ToCut.Xe = O.Xe;
    }
    if (O.Ys > ToCut.Ys && O.Ys <= ToCut.Ye) {
      Cube Cut = ToCut;
      Cut.Ye = O.Ys - 1;
      Cuts.push_back(Cut);
      ToCut.Ys = O.Ys;
    }
    if (O.Ye >= ToCut.Ys && O.Ye < ToCut.Ye) {
      Cube Cut = ToCut;
      Cut.Ys = O.Ye + 1;
      Cuts.push_back(Cut);
      ToCut.Ye = O.Ye;
    }
    if (O.Zs > ToCut.Zs && O.Zs <= ToCut.Ze) {
      Cube Cut = ToCut;
      Cut.Ze = O.Zs - 1;
      Cuts.push_back(Cut);
      ToCut.Zs = O.Zs;
    }
    if (O.Ze >= ToCut.Zs && O.Ze < ToCut.Ze) {
      Cube Cut = ToCut;
      Cut.Zs = O.Ze + 1;
      Cuts.push_back(Cut);
      ToCut.Ze = O.Ze;
    }
    return Cuts;
  }
};

template<class CharT, class Traits>
std::basic_ostream<CharT, Traits> &
operator<<(std::basic_ostream<CharT, Traits> &OS, const Cube &C) {
  return OS << (C.On ? "on " : "off ")
            << "x=" << C.Xs << ".." << C.Xe
            << ",y=" << C.Ys << ".." << C.Ye
            << ",z=" << C.Zs << ".." << C.Ze;
}

int main(int argc, const char *argv[]) {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(nullptr);
  std::cout.tie(nullptr);

  std::fstream File;
  if (argc > 1)
    File.open(argv[1]);
  std::istream &Input = argc == 1 ? std::cin : File;

  std::vector<Cube> Steps;

  std::string Si;
  while (Input >> Si) {
    if (Si == "end")
      break;
    Cube Sp;
    Sp.On = Si == "on";
    Input >> Si;
    assert(std::sscanf(Si.c_str(), "x=%lld..%lld,y=%lld..%lld,z=%lld..%lld",
                       &Sp.Xs, &Sp.Xe, &Sp.Ys, &Sp.Ye, &Sp.Zs, &Sp.Ze) == 6 &&
        "Should scan six values");
    Steps.push_back(Sp);
  }

  constexpr std::size_t P1Size = 101;
  std::vector<std::vector<std::vector<bool>>>
      P1(P1Size, std::vector<std::vector<bool>>(P1Size, std::vector<bool>(P1Size, false)));

  for (const Cube S: Steps) {
    if ((S.Xs < -50 && S.Xe < -50) || (S.Xs > 50 && S.Xe > 50) ||
        (S.Ys < -50 && S.Ye < -50) || (S.Ys > 50 && S.Ye > 50) ||
        (S.Zs < -50 && S.Ze < -50) || (S.Zs > 50 && S.Ze > 50))
      continue;
    for (long long X = std::clamp(S.Xs, -50ll, 50ll) + 50ll; X <= std::clamp(S.Xe, -50ll, 50ll) + 50ll; ++X)
      for (long long Y = std::clamp(S.Ys, -50ll, 50ll) + 50ll; Y <= std::clamp(S.Ye, -50ll, 50ll) + 50ll; ++Y)
        for (long long Z = std::clamp(S.Zs, -50ll, 50ll) + 50ll; Z <= std::clamp(S.Ze, -50ll, 50ll) + 50ll; ++Z)
          P1[X][Y][Z] = S.On;
  }

  long long P1Count = 0;
  for (const auto &D2: P1)
    for (const auto &D1: D2)
      for (const auto &B: D1)
        if (B)
          P1Count++;

  std::cout << "Part 1 = " << P1Count << std::endl;

  std::vector<Cube> OR;
  for (const Cube C: Steps) {
    std::vector<Cube> TD;
    std::vector<Cube> TA;
    for (const Cube &S: OR) {
      if (S.overlap(C)) {
        TD.push_back(S);
        auto TR = S.split(C);
        TA.insert(std::cend(TA), std::cbegin(TR), std::cend(TR));
      }
    }
    for (const Cube &D: TD)
      OR.erase(std::find(std::begin(OR), std::end(OR), D));
    for (const Cube &A: TA)
      OR.push_back(A);
    if (C.On)
      OR.push_back(C);
  }

  std::cout << "Part 2 = "
            << std::accumulate(std::cbegin(OR), std::cend(OR), 0ll,
                               [](long long Acc, Cube C) {
                                 return Acc + C.size();
                               })
            << std::endl;

  return 0;
}