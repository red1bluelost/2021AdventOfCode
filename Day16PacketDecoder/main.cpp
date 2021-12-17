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
#include <memory>

class Packet {
protected:
  int Version;
  int TypeId;
public:
  using ValueType = long long;
  Packet(int V, int I) : Version{V}, TypeId{I} {}
  virtual int sumUpVersions() const = 0;
  virtual ValueType evaluate() const = 0;
};

class Literal : public Packet {
  ValueType Value;
public:
  Literal(int V, int I, ValueType Val) : Packet(V, I), Value{Val} {}
  int sumUpVersions() const override {
    return Version;
  }
  ValueType evaluate() const override {
    return Value;
  }
};

class Operator : public Packet {
  std::vector<std::unique_ptr<Packet>> SubPackets;
public:
  Operator(int V, int I, std::vector<std::unique_ptr<Packet>> &&SPs) : Packet(V, I), SubPackets{std::move(SPs)} {}
  int sumUpVersions() const override {
    return Version + std::accumulate(std::cbegin(SubPackets),
                                     std::cend(SubPackets),
                                     0,
                                     [](int Acc, const auto &P) { return Acc + P->sumUpVersions(); });
  }
  ValueType evaluate() const override {
    switch (TypeId) {
    case 0:
      return std::accumulate(std::cbegin(SubPackets),
                             std::cend(SubPackets),
                             ValueType{0},
                             [](ValueType Acc, const auto &P) { return Acc + P->evaluate(); });
    case 1:
      return std::accumulate(std::cbegin(SubPackets),
                             std::cend(SubPackets),
                             ValueType{1},
                             [](ValueType Acc, const auto &P) { return Acc * P->evaluate(); });
    case 2:
      return std::accumulate(std::cbegin(SubPackets),
                             std::cend(SubPackets),
                             std::numeric_limits<ValueType>::max(),
                             [](ValueType Acc, const auto &P) { return std::min(Acc, P->evaluate()); });
    case 3:
      return std::accumulate(std::cbegin(SubPackets),
                             std::cend(SubPackets),
                             std::numeric_limits<ValueType>::min(),
                             [](ValueType Acc, const auto &P) { return std::max(Acc, P->evaluate()); });
    case 5:return SubPackets[0]->evaluate() > SubPackets[1]->evaluate();
    case 6:return SubPackets[0]->evaluate() < SubPackets[1]->evaluate();
    case 7:return SubPackets[0]->evaluate() == SubPackets[1]->evaluate();
    default: std::exit(1);
    }
  }
};

int parseNum(std::vector<bool>::const_iterator BS, int L) {
  int I = 0;
  for (; L > 0; --L)
    I = (I << 1) + *BS++;
  return I;
}

std::pair<std::unique_ptr<Packet>, std::vector<bool>::const_iterator>
buildPacket(std::vector<bool>::const_iterator BS) {
  auto GetNum = [&BS](int L) -> int {
    int R = parseNum(BS, L);
    BS = std::next(BS, L);
    return R;
  };
  int Version = GetNum(3);
  int TypeId = GetNum(3);
  switch (TypeId) {
  case 4: { // Literal
    Packet::ValueType Value = 0;
    while (true) {
      bool Ret = *BS++;
      Value = (Value << 4) + GetNum(4);
      if (!Ret)
        return {std::make_unique<Literal>(Version, TypeId, Value), BS};
    }
  }
  default: { // Operator
    bool LenTypeId = *BS++;
    std::vector<std::unique_ptr<Packet>> SubPackets;
    if (LenTypeId) {
      for (int N = GetNum(11); N > 0; --N) {
        auto[P, NextBS] = buildPacket(BS);
        SubPackets.push_back(std::move(P));
        BS = NextBS;
      }
    } else {
      int BitsInSubPackets = GetNum(15);
      auto StartBS = BS;
      while (BitsInSubPackets > std::distance(StartBS, BS)) {
        auto[P, NextBS] = buildPacket(BS);
        SubPackets.push_back(std::move(P));
        BS = NextBS;
      }
    }
    return {std::make_unique<Operator>(Version, TypeId, std::move(SubPackets)), BS};
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

  std::vector<bool> Bits;
  std::string S;
  std::getline(Input, S);
  for (char C: S) {
    uint8_t I = (C >= '0' && C <= '9')
                ? C - '0' :
                C - 'A' + 10;
    Bits.push_back(I & 0b1000);
    Bits.push_back(I & 0b0100);
    Bits.push_back(I & 0b0010);
    Bits.push_back(I & 0b0001);
  }

  auto[P, BS] = buildPacket(std::cbegin(Bits));

  std::cout << "Part 1 = " << P->sumUpVersions() << "\n";
  std::cout << "Part 2 = " << P->evaluate() << "\n";

  return 0;
}
