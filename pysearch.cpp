#include <array>
#include <cstdio>
#include <unordered_map>
#include <valarray>
#include <vector>

using Vec = std::valarray<int>;
struct Input { const char *name; Vec vec; };

// ---- start of parameters ---

// http://golf.shinh.org/p.rb?reversed+even+or+odd+first
// static const Input inputs[] = {
//                   {"n", {100, 100, 100, 100,     53,  53,  53,  53}},
//                   {"x", {100,  98,   2,  99,     53,   1,  20,   4}}, };
// static const Vec goal = { 98,  96,  99,  97,     51,  52,  18,   2};

static const Input inputs[] = { {"n", {3,5,6,28, 31, 33, 34, 35, 36, 37, 38}} };

static const Vec goal = {5,6,7,31, 33, 34, 35, 36, 37, 38,40};

const int max_length = 14;

static const int literals[] = {2,3}; // 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,};

// ---- end of parameters ---

enum class Operator : int32_t {
  Or = 0x300,
  SpaceOr = 0x301,
  OrSpace = 0x302,
  SpaceOrSpace = 0x303,
  Lt = 0x500,
  Leq = 0x501,
  Gt = 0x502,
  Geq = 0x503,
  Eq = 0x504,
  Neq = 0x505,
  BitOr = 0x600,
  BitXor = 0x700,
  BitAnd = 0x800,
  BitShl = 0x900,
  BitShr = 0x901,
  Add = 0xA00,
  Sub = 0xA01,
  Mul = 0xB00,
  Mod = 0xB01,
  Div = 0xB02,
  Neg = 0xC00,
  BitNeg = 0xC01,
  Parens = 0xFE00,
  Literal = 0xFF00,
};

struct Expr {
  const Expr *left;
  const Expr *right;
  Operator op;
  int32_t literal;
  inline int32_t prec() const { return static_cast<int32_t>(op) >> 8; }
  int length;
};

using Pair = std::array<Expr, 2>;

void print_operator(Operator op) {
  switch (op) {
    case Operator::Or: printf("or"); break;
    case Operator::SpaceOr: printf(" or"); break;
    case Operator::OrSpace: printf("or "); break;
    case Operator::SpaceOrSpace: printf(" or "); break;
    case Operator::Lt: printf("<"); break;
    case Operator::Leq: printf("<="); break;
    case Operator::Gt: printf(">"); break;
    case Operator::Geq: printf(">="); break;
    case Operator::Eq: printf("=="); break;
    case Operator::Neq: printf("!="); break;
    case Operator::BitOr: printf("|"); break;
    case Operator::BitXor: printf("^"); break;
    case Operator::BitAnd: printf("&"); break;
    case Operator::BitShl: printf("<<"); break;
    case Operator::BitShr: printf(">>"); break;
    case Operator::Add: printf("+"); break;
    case Operator::Sub: printf("-"); break;
    case Operator::Mul: printf("*"); break;
    case Operator::Mod: putchar('%'); break;
    case Operator::Div: printf("/"); break;
    case Operator::Neg: printf("-"); break;
    case Operator::BitNeg: printf("~"); break;
    case Operator::Parens: printf("("); break;
    default: break;
  }
}

void print_expression(const Expr *expr) {
  if (expr->left != nullptr) {
    print_expression(expr->left);
  }
  print_operator(expr->op);
  if (expr->right != nullptr) {
    print_expression(expr->right);
    if (expr->op == Operator::Parens) {
      putchar(')');
    }
  } else if (expr->literal != 0) {
    if (expr->literal < 0) {
      printf("%s", inputs[~expr->literal].name);
    } else {
      printf("%d", expr->literal);
    }
  }
}

struct VecHasher {
  std::size_t operator()(const Vec &v) const {
    int h = 0;
    for (const auto x : v)
      h ^= x + 0x9e3779b9 + (h << 6) + (x >> 2);
    return h;
  }
};

struct VecEqual {
  bool operator()(const Vec &v1, const Vec &v2) const {
    return (v1 == v2).min();
  }
};

// cache[output] = best expressions yielding it
using Cache = std::unordered_map<Vec, Pair, VecHasher, VecEqual>;

static Cache cache = Cache();

// "3or" and ")or" are valid, but "nor" isn't.
bool ok_before_keyword(const Expr *e) {
  if (e->right == nullptr) {
    return e->literal >= 0;
  } else {
    return e->op == Operator::Parens || ok_before_keyword(e->right);
  }
}

// "or3", "orn" are invalid. Need a unary op or parens.
bool ok_after_keyword(const Expr *e) {
  if (e->left == nullptr) {
    return e->op != Operator::Literal;
  } else {
    return ok_after_keyword(e->left);
  }
}

int positive_integer_length(int k) {
  int l = 1;
  while (k >= 10) k /= 10, l++;
  return l;
}

inline void cache_if_better(Cache& level, const Vec& output, const Expr& expr) {
  // if it doesn't exist, "old" is a default-constructed Expr with prec == 0, length == 0.
  auto& old = level[output];
  if (expr.prec() > old[1].prec() && (old[1].length == 0 || expr.length <= old[1].length)) {
    old[1] = expr;
  }
  if ((old[0].length == 0 || expr.length < old[0].length) && expr.prec() >= old[0].prec()) {
    old[0] = expr;
  }
}

void find_expressions() {
  for (int i = 0; i < sizeof(inputs) / sizeof(inputs[0]); i++) {
    const auto e = Expr{nullptr, nullptr, Operator::Literal, ~i, 1};
    cache[inputs[i].vec] = {e, e};
  }
  for (const auto l : literals) {
    const auto e = Expr{nullptr, nullptr, Operator::Literal, l, positive_integer_length(l)};
    cache[0 * goal + l] = {e, e};
  }

  for (const auto &[oR, pR] : cache) for (const auto &eR : pR) if (eR.length < 8) {
    // 1-byte operators
    for (const auto &[oL, pL] : cache) for (const auto &eL : pL) if (eL.length < 8) {
      if (eL.prec() >= 5 && eR.prec() > 5) {
        cache_if_better(cache, +(oL < oR), Expr{&eL, &eR, Operator::Lt, 0, eL.length + eR.length + 1});
      }
      if (eL.prec() >= 6 && eR.prec() > 6) {
        cache_if_better(cache, oL | oR, Expr{&eL, &eR, Operator::BitOr, 0, eL.length + eR.length + 1});
      }
      if (eL.prec() >= 7 && eR.prec() > 7) {
        cache_if_better(cache, oL ^ oR, Expr{&eL, &eR, Operator::BitXor, 0, eL.length + eR.length + 1});
      }
      if (eL.prec() >= 8 && eR.prec() > 8) {
        cache_if_better(cache, oL & oR, Expr{&eL, &eR, Operator::BitAnd, 0, eL.length + eR.length + 1});
      }
      if (eL.prec() >= 10 && eR.prec() > 10) {
        cache_if_better(cache, oL + oR, Expr{&eL, &eR, Operator::Add, 0, eL.length + eR.length + 1});
        cache_if_better(cache, oL - oR, Expr{&eL, &eR, Operator::Sub, 0, eL.length + eR.length + 1});
      }
      if (eL.prec() >= 11 && eR.prec() > 11) {
        cache_if_better(cache, oL * oR, Expr{&eL, &eR, Operator::Mul, 0, eL.length + eR.length + 1});
        if ((oR != 0).min()) {
          auto mod = ((oL % oR) + oR) % oR;
          cache_if_better(cache, mod, Expr{&eL, &eR, Operator::Mod, 0, eL.length + eR.length + 1});
          cache_if_better(cache, (oL - mod) / oR, Expr{&eL, &eR, Operator::Div, 0, eL.length + eR.length + 1});
        }
      }
      if (eL.prec() >= 3 && eR.prec() > 3) {
        if (ok_before_keyword(&eL) && ok_after_keyword(&eR))
          cache_if_better(cache, oL + oR * +(oL == 0), Expr{&eL, &eR, Operator::Or, 0, eL.length + eR.length + 2});
      }
      if (eL.prec() >= 5 && eR.prec() > 5) {
        cache_if_better(cache, +(oL <= oR), Expr{&eL, &eR, Operator::Leq, 0, eL.length + eR.length + 2});
      }
      if (eL.prec() > 9 && eR.prec() >= 9 && (oR >= 0).min() && (oR <= 31).min()) {
        cache_if_better(cache, oL << oR, Expr{&eL, &eR, Operator::BitShl, 0, eL.length + eR.length + 2});
        cache_if_better(cache, oL >> oR, Expr{&eL, &eR, Operator::BitShr, 0, eL.length + eR.length + 2});
      }
      if (eL.prec() >= 3 && eR.prec() > 3) {
        if (!ok_before_keyword(&eL) && ok_after_keyword(&eR))
          cache_if_better(cache, oL + oR * +(oL == 0), Expr{&eL, &eR, Operator::SpaceOr, 0, eL.length + eR.length + 3});
        if (ok_before_keyword(&eL) && !ok_after_keyword(&eR))
          cache_if_better(cache, oL + oR * +(oL == 0), Expr{&eL, &eR, Operator::OrSpace, 0, eL.length + eR.length + 3});
      }
    }
    if (eR.op >= Operator::Parens)
      continue;
    cache_if_better(cache, oR, Expr{nullptr, &eR, Operator::Parens, 0, eR.length + 2});
    if (eR.prec() >= 12) {
      cache_if_better(cache, ~oR, Expr{nullptr, &eR, Operator::BitNeg, 0, eR.length + 1});
      cache_if_better(cache, -oR, Expr{nullptr, &eR, Operator::Neg, 0, eR.length + 1});
    }
  }
}

int main() {
  printf("sizeof(Expr) = %zu\n", sizeof(Expr));
  for (int n = 1; n <= max_length; n++) {
    printf("Finding depth %d...\n", n);
    find_expressions();
    printf("Found %zu expressions.\n", cache.size());
  }
  for (const auto &[output, pair] : cache) {
    if ((output != goal).max())
      continue;
    print_expression(&pair[0]);
  }
  printf("That's all.\n");
  return 0;
}
