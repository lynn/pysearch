#include <cstdio>
#include <unordered_map>
#include <valarray>
#include <vector>

using Vec = std::valarray<int>;
struct Input { const char *name; Vec vec; };

// ---- start of parameters ---

// http://golf.shinh.org/p.rb?reversed+even+or+odd+first
static const Input inputs[] = {
                  {"n", {100, 100, 100, 100,     53,  53,  53,  53}},
                  {"x", {100,  98,   2,  99,     53,   1,  20,   4}}, };
static const Vec goal = { 98,  96,  99,  97,     51,  52,  18,   2};

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
  int32_t prec() const { return static_cast<int32_t>(op) >> 8; }
};

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

// cache[length][output] = highest-prec expression of that length yielding that output
using CacheLevel = std::unordered_map<Vec, Expr, VecHasher, VecEqual>;
using Cache = std::unordered_map<int, CacheLevel>;
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

void cache_if_better(CacheLevel& level, const Vec& output, const Expr& expr) {
  // if it doesn't exist, "old" is a default-constructed Expr with prec == 0 so it all works out.
  auto old = level[output];
  if (expr.prec() > old.prec()) {
    level[output] = expr;
  }
}

void find_expressions(int n) {
  auto &cn = cache[n];
  if (n == 1) {
    for (int i = 0; i < sizeof(inputs) / sizeof(inputs[0]); i++) {
      cn[inputs[i].vec] = Expr{nullptr, nullptr, Operator::Literal, ~i};
    }
  }
  for (const auto l : literals) {
    if (positive_integer_length(l) == n)
      cn[0 * goal + l] = Expr{nullptr, nullptr, Operator::Literal, l};
  }

  for (int nR = 1; nR < n; nR++) {
    for (const auto &[oR, eR] : cache[nR]) {
      // 1-byte operators
      for (const auto &[oL, eL] : cache[n - nR - 1]) {
        if (eL.prec() >= 5 && eR.prec() > 5) {
          cache_if_better(cn, +(oL < oR), Expr{&eL, &eR, Operator::Lt, 0});
        }
        if (eL.prec() >= 6 && eR.prec() > 6) {
          cache_if_better(cn, oL | oR, Expr{&eL, &eR, Operator::BitOr, 0});
        }
        if (eL.prec() >= 7 && eR.prec() > 7) {
          cache_if_better(cn, oL ^ oR, Expr{&eL, &eR, Operator::BitXor, 0});
        }
        if (eL.prec() >= 8 && eR.prec() > 8) {
          cache_if_better(cn, oL & oR, Expr{&eL, &eR, Operator::BitAnd, 0});
        }
        if (eL.prec() >= 10 && eR.prec() > 10) {
          cache_if_better(cn, oL + oR, Expr{&eL, &eR, Operator::Add, 0});
          cache_if_better(cn, oL - oR, Expr{&eL, &eR, Operator::Sub, 0});
        }
        if (eL.prec() >= 11 && eR.prec() > 11) {
          cache_if_better(cn, oL * oR, Expr{&eL, &eR, Operator::Mul, 0});
          if ((oR != 0).min()) {
            auto mod = ((oL % oR) + oR) % oR;
            cache_if_better(cn, mod, Expr{&eL, &eR, Operator::Mod, 0});
            cache_if_better(cn, (oL - mod) / oR, Expr{&eL, &eR, Operator::Div, 0});
          }
        }
      }
      // 2-byte operators
      for (const auto &[oL, eL] : cache[n - nR - 2]) {
        if (eL.prec() >= 3 && eR.prec() > 3) {
          if (ok_before_keyword(&eL) && ok_after_keyword(&eR))
            cache_if_better(cn, oL + oR * +(oL == 0), Expr{&eL, &eR, Operator::Or, 0});
        }
        if (eL.prec() >= 5 && eR.prec() > 5) {
          cache_if_better(cn, +(oL <= oR), Expr{&eL, &eR, Operator::Leq, 0});
        }
        if (eL.prec() > 9 && eR.prec() >= 9 && (oR >= 0).min() && (oR <= 31).min()) {
          cache_if_better(cn, oL << oR, Expr{&eL, &eR, Operator::BitShl, 0});
          cache_if_better(cn, oL >> oR, Expr{&eL, &eR, Operator::BitShr, 0});
        }
      }

      // 3-byte operators
      for (const auto &[oL, eL] : cache[n - nR - 3]) {
        if (eL.prec() >= 3 && eR.prec() > 3) {
          if (!ok_before_keyword(&eL) && ok_after_keyword(&eR))
            // TODO spaces around or
            cache_if_better(cn, oL + oR * +(oL == 0), Expr{&eL, &eR, Operator::SpaceOr, 0});
          if (ok_before_keyword(&eL) && !ok_after_keyword(&eR))
            cache_if_better(cn, oL + oR * +(oL == 0), Expr{&eL, &eR, Operator::OrSpace, 0});
        }
      }
    }
  }
  for (const auto &[oR, eR] : cache[n - 2]) {
    if (eR.op >= Operator::Parens)
      continue;
    cn[oR] = Expr{nullptr, &eR, Operator::Parens, 0};
  }
  for (const auto &[oR, eR] : cache[n - 1]) {
    if (eR.prec() >= 12) {
      cache_if_better(cn, ~oR, Expr{nullptr, &eR, Operator::BitNeg, 0});
      cache_if_better(cn, -oR, Expr{nullptr, &eR, Operator::Neg, 0});
    }
  }
}

int main() {
  printf("sizeof(Expr) = %zu\n", sizeof(Expr));
  bool no_results = true;
  for (int n = 1; n <= max_length; n++) {
    printf("Finding length %d...\n", n);
    find_expressions(n);
    printf("Found %zu expressions.\n", cache[n].size());
    bool first = true;
    for (const auto &[oR, eR] : cache[n]) {
      if ((oR != goal).max())
        continue;
      if (first) {
        printf("\n--- Length %d ---\n", n);
        first = false;
        no_results = false;
      }
      print_expression(&eR);
      puts("");
    }
  }
  if (no_results) puts("\nNo results found.");
  puts("");
  return 0;
}
