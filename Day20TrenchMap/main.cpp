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

class Image;

template<class CharT, class Traits>
std::basic_ostream<CharT, Traits> &operator<<(std::basic_ostream<CharT, Traits> &OS, const Image &I);

int parseNum(const std::vector<bool> &V) {
  return std::accumulate(std::cbegin(V), std::cend(V), 0,
                         [](int Acc, bool B) { return (Acc << 1) + B; });
}

class Image {
  bool InfinitePixel = false;
  std::vector<bool> Data;

  using ref = typename std::vector<bool>::reference;
  using const_ref = typename std::vector<bool>::const_reference;

  int getKernelNum(int X, int Y) const {
    std::vector<bool> Pixels;
    Pixels.reserve(9);
    for (int J = -1; J <= 1; ++J) {
      for (int I = -1; I <= 1; ++I) {
        auto Opt = at(X + I, Y + J);
        Pixels.push_back(Opt.has_value() ? *Opt : InfinitePixel);
      }
    }
    return parseNum(Pixels);
  }

public:
  const std::size_t L, W;

  Image(std::size_t Li, std::size_t Wi, std::vector<bool> &&Di) : Data{std::move(Di)}, L{Li}, W{Wi} {}

  std::optional<ref> at(int X, int Y) {
    return (X < 0 || Y < 0 || static_cast<std::size_t>(X) >= W || static_cast<std::size_t>(Y) >= L) ?
           std::optional<ref>{} :
           Data[Y * W + X];
  }

  std::optional<const_ref> at(int X, int Y) const {
    return (X < 0 || Y < 0 || static_cast<std::size_t>(X) >= W || static_cast<std::size_t>(Y) >= L) ?
           std::optional<const_ref>{} :
           Data[Y * W + X];
  }

  Image &applyIEA(const std::vector<bool> &IEA) {
    assert(IEA.size() == 512 && "IAE must have size of 512");
    Image C = *this;
    InfinitePixel = IEA[InfinitePixel ? 511 : 0];
    for (std::size_t Y = 0; Y < L; ++Y)
      for (std::size_t X = 0; X < W; ++X)
        *at(X, Y) = IEA[C.getKernelNum(X, Y)];
    return *this;
  }

  std::optional<int> litPixels() const {
    if (InfinitePixel)
      return std::nullopt;
    return std::count_if(std::cbegin(Data), std::cend(Data), std::identity{});
  }
};

template<class CharT, class Traits>
std::basic_ostream<CharT, Traits> &
operator<<(std::basic_ostream<CharT, Traits> &OS, const Image &I) {
  for (std::size_t Y = 0; Y < I.L; ++Y) {
    for (std::size_t X = 0; X < I.W; ++X) {
      OS << (*I.at(X, Y) ? '#' : '.');
    }
    OS << '\n';
  }
  return OS;
}

Image pad(const Image &I, int P) {
  std::size_t NL = I.L + 2 * P, NW = I.W + 2 * P;
  Image Padded{NL, NW, std::vector<bool>(NL * NW)};
  for (std::size_t Y = 0; Y < I.L; ++Y)
    for (std::size_t X = 0; X < I.W; ++X)
      *Padded.at(X + P, Y + P) = *I.at(X, Y);
  return Padded;
}

int main(int argc, const char *argv[]) {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(nullptr);
  std::cout.tie(nullptr);

  std::fstream File;
  if (argc > 1)
    File.open(argv[1]);
  std::istream &Input = argc == 1 ? std::cin : File;

  std::vector<bool> IEA = std::invoke([&Input] {
    std::vector<bool> V;
    V.reserve(512);
    std::string S;
    std::getline(Input, S);
    if (S.back() == '\r')
      S.pop_back();
    std::transform(std::cbegin(S), std::cend(S), std::back_inserter(V),
                   [](char C) { return C == '#'; });
    return V;
  });

  Image II = std::invoke([&Input] {
    std::vector<bool> V;
    std::size_t L = 0, W = 0;
    std::string S;
    std::getline(Input, S);
    while (std::getline(Input, S)) {
      if (S.back() == '\r')
        S.pop_back();
      std::transform(std::cbegin(S), std::cend(S), std::back_inserter(V),
                     [](char C) { return C == '#'; });
      W = S.length();
      L++;
    }
    return Image(L, W, std::move(V));
  });

  std::cout << "Part 1 = " << pad(II, 5).applyIEA(IEA).applyIEA(IEA).litPixels().value_or(-1) << std::endl;

  Image I50x = pad(II, 100);
  for (int I = 0; I < 50; ++I)
    I50x.applyIEA(IEA);

  std::cout << "Part 2 = " << I50x.litPixels().value_or(-1) << std::endl;
  return 0;
}