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
#include <exception>

constexpr bool isDigit(char C) {
  return C >= '0' && C <= '9';
}
namespace {
struct Regular;
struct Pair;
struct SnailNumber {
  Pair *Parent = nullptr;
  virtual std::string string() const = 0;

  static Regular *findLeft(Pair *X);
  static Regular *findRight(Pair *X);

  virtual std::unique_ptr<SnailNumber> explode(int Depth) = 0;
  virtual std::unique_ptr<SnailNumber> split() = 0;

  bool reduce();

  static std::unique_ptr<SnailNumber>
  add(std::unique_ptr<SnailNumber> LHS, std::unique_ptr<SnailNumber> RHS);

  virtual int magnitude() const = 0;
};

struct Regular : public SnailNumber {
  int Value;

  Regular(int V) : Value{V} {}

  std::string string() const override { return std::to_string(Value); };

  std::unique_ptr<SnailNumber> explode(int) override { return nullptr; }
  std::unique_ptr<SnailNumber> split() override;

  int magnitude() const override;
};

struct Pair : public SnailNumber {
  std::unique_ptr<SnailNumber> Left, Right;
  Pair(std::unique_ptr<SnailNumber> L, std::unique_ptr<SnailNumber> R)
      : Left{std::move(L)}, Right{std::move(R)} {
    Left->Parent = this;
    Right->Parent = this;
  }
  std::string string() const override { return "[" + Left->string() + "," + Right->string() + "]"; };

  std::unique_ptr<SnailNumber> explode(int Depth) override;
  std::unique_ptr<SnailNumber> split() override {
    if (auto L = Left->split())
      return L;
    if (auto R = Right->split())
      return R;
    return nullptr;
  }

  int magnitude() const override;
};

bool SnailNumber::reduce() {
  bool WasReduce = false;
  while (explode(0))
    WasReduce = true;
  if (split())
    WasReduce = true;
  return WasReduce && reduce();
}

std::unique_ptr<SnailNumber> SnailNumber::add(std::unique_ptr<SnailNumber> LHS, std::unique_ptr<SnailNumber> RHS) {
  std::unique_ptr<SnailNumber> Res = std::make_unique<Pair>(std::move(LHS), std::move(RHS));
  Res->reduce();
  return Res;
}

Regular *SnailNumber::findLeft(Pair *X) {
  if (!X || !X->Parent)
    return nullptr;
  if (X->Parent->Right.get() == static_cast<SnailNumber *>(X)) {
    SnailNumber *Dive = X->Parent->Left.get();
    while (!dynamic_cast<Regular *>(Dive))
      Dive = dynamic_cast<Pair *>(Dive)->Right.get();
    return dynamic_cast<Regular *>(Dive);
  }
  return findLeft(X->Parent);
}
Regular *SnailNumber::findRight(Pair *X) {
  if (!X || !X->Parent)
    return nullptr;
  if (X->Parent->Left.get() == static_cast<SnailNumber *>(X)) {
    SnailNumber *Dive = X->Parent->Right.get();
    while (!dynamic_cast<Regular *>(Dive))
      Dive = dynamic_cast<Pair *>(Dive)->Left.get();
    return dynamic_cast<Regular *>(Dive);
  }
  return findRight(X->Parent);
}

std::unique_ptr<SnailNumber> Regular::split() {
  if (Value < 10)
    return nullptr;

  bool IsLeft = Parent->Left.get() == this;

  std::unique_ptr<SnailNumber>
      Self = std::make_unique<Pair>(std::make_unique<Regular>(Value / 2),
                                    std::make_unique<Regular>((Value + 1) / 2));
  Self->Parent = Parent;
  Self.swap(IsLeft ? Parent->Left : Parent->Right);
  return Self;
}

int Regular::magnitude() const {
  return Value;
}

std::unique_ptr<SnailNumber> Pair::explode(int Depth) {
  if (Depth < 4) {
    if (auto L = Left->explode(Depth + 1))
      return L;
    if (auto R = Right->explode(Depth + 1))
      return R;
    return nullptr;
  }

  bool IsLeft = Parent->Left.get() == this;

  int LV = dynamic_cast<Regular *>(Left.get())->Value;
  int RV = dynamic_cast<Regular *>(Right.get())->Value;

  if (Regular *R = findLeft(this))
    R->Value += LV;
  if (Regular *R = findRight(this))
    R->Value += RV;

  std::unique_ptr<SnailNumber> Self = std::make_unique<Regular>(0);
  Self->Parent = Parent;
  Self.swap(IsLeft ? Parent->Left : Parent->Right);

  return Self;
}

int Pair::magnitude() const {
  return 3 * Left->magnitude() + 2 * Right->magnitude();
}

constexpr bool validInput(std::string_view SV) {
  return SV.front() == '[' && SV.back() == ']' &&
      std::accumulate(std::begin(SV), std::end(SV), 0, [](int Acc, char C) {
        if (C == '[')
          return Acc + 1;
        if (C == ']')
          return Acc - 1;
        return Acc;
      }) == 0;
}

std::unique_ptr<SnailNumber> makeSnailNumber(std::string_view SV) {
  if (!validInput(SV))
    throw std::invalid_argument(std::string(SV));

  std::unique_ptr<SnailNumber> Left, Right;

  // Find Left
  std::string_view::const_iterator B = std::next(std::begin(SV));
  std::string_view::const_iterator M = std::next(B);
  if (isDigit(*B)) {
    int V = *B - '0';
    while (isDigit(*M)) {
      V = V * 10 + (*M++ - '0');
    }
    Left = std::make_unique<Regular>(V);
  } else {
    for (int Count = 0; !(*M == ']' && Count == 0); ++M) {
      if (char C = *M; C == '[')
        Count++;
      else if (C == ']')
        Count--;
    }
    M = std::next(M);
    Left = makeSnailNumber(std::string_view(B, M));
  }
  B = std::next(M);
  M = std::next(B);
  if (isDigit(*B)) {
    Right = std::make_unique<Regular>(*B - '0');
    int V = *B - '0';
    while (isDigit(*M)) {
      V = V * 10 + (*M++ - '0');
    }
  } else {
    Right = makeSnailNumber(std::string_view(B, std::next(std::end(SV), -1)));
  }

  return std::make_unique<Pair>(std::move(Left), std::move(Right));
}
} // namespace

int main(int argc, const char *argv[]) {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(nullptr);
  std::cout.tie(nullptr);

  std::fstream File;
  if (argc > 1)
    File.open(argv[1]);
  std::istream &Input = argc == 1 ? std::cin : File;

  std::vector<std::unique_ptr<SnailNumber>> Numbers;
  std::vector<std::vector<std::unique_ptr<SnailNumber>>> NumbersFuck;

  for (std::string S; std::getline(Input, S);) {
    if (S.back() == '\r' || S.back() == '\n')
      S.pop_back();
    Numbers.insert(std::begin(Numbers), makeSnailNumber(S));

    NumbersFuck.emplace_back();
    NumbersFuck.back().reserve(1000);
    for (std::size_t I = 0; I < 1000; ++I)
      NumbersFuck.back().push_back(makeSnailNumber(S));
  }

  auto ExtractBack = [](std::vector<std::unique_ptr<SnailNumber>> &V) {
    std::unique_ptr<SnailNumber> R = std::move(V.back());
    V.pop_back();
    return R;
  };
  std::unique_ptr<SnailNumber> Value = ExtractBack(Numbers);
  while (!Numbers.empty()) {
    std::unique_ptr<SnailNumber> B = ExtractBack(Numbers);
    Value = SnailNumber::add(std::move(Value), std::move(B));
  }

  std::cout << Value->string() << std::endl;
  std::cout << "Part 1 = " << Value->magnitude() << std::endl;

  int Max = 0;
  for (std::size_t I = 0; I < NumbersFuck.size(); ++I)
    for (std::size_t J = 0; J < NumbersFuck.size(); ++J)
      if (I != J)
        Max = std::max(
            Max,
            SnailNumber::add(ExtractBack(NumbersFuck[I]), ExtractBack(NumbersFuck[J]))->magnitude()
        );

  std::cout << "Part 2 = " << Max << std::endl;

  return 0;
}
