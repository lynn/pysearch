use std::fmt::Display;

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operator {
    Or = 0x30,
    SpaceOr = 0x31,
    OrSpace = 0x32,
    // SpaceOrSpace = 0x303,
    Lt = 0x50,
    Le = 0x51,
    // Gt = 0x502,
    // Ge = 0x503,
    // Eq = 0x504,
    // Ne = 0x505,
    BitOr = 0x60,
    BitXor = 0x70,
    BitAnd = 0x80,
    BitShl = 0x90,
    BitShr = 0x91,
    Add = 0xA0,
    Sub = 0xA1,
    Mul = 0xB0,
    Mod = 0xB1,
    Div1 = 0xB2,
    Div2 = 0xB3,
    Gcd = 0xB4,
    Neg = 0xC0,
    BitNeg = 0xC1,
    Exp = 0xD0,
    Parens = 0xE0,
    Literal = 0xF0,
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Or => write!(f, "or"),
            Operator::SpaceOr => write!(f, " or"),
            Operator::OrSpace => write!(f, "or "),
            // Operator::SpaceOrSpace => write!(f, " or "),
            Operator::Lt => write!(f, "<"),
            Operator::Le => write!(f, "<="),
            // Operator::Gt => write!(f, ">"),
            // Operator::Ge => write!(f, ">="),
            // Operator::Eq => write!(f, "=="),
            // Operator::Ne => write!(f, "!="),
            Operator::BitOr => write!(f, "|"),
            Operator::BitXor => write!(f, "^"),
            Operator::BitAnd => write!(f, "&"),
            Operator::BitShl => write!(f, "<<"),
            Operator::BitShr => write!(f, ">>"),
            Operator::Add => write!(f, "+"),
            Operator::Sub => write!(f, "-"),
            Operator::Mul => write!(f, "*"),
            Operator::Mod => write!(f, "%"),
            Operator::Div1 => write!(f, "/"),
            Operator::Div2 => write!(f, "//"),
            Operator::Gcd => write!(f, "∨"),
            Operator::Neg => write!(f, "-"),
            Operator::BitNeg => write!(f, "~"),
            Operator::Exp => write!(f, "**"),
            Operator::Parens => write!(f, "("),
            Operator::Literal => write!(f, ""),
        }
    }
}
