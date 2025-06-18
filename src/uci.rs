
use crate::types::{PieceType, Square};

/// Parse a UCI string like "e2e4" or "e7e8q"
pub fn parse_uci(s: &str) -> Result<(Square, Square, Option<PieceType>), String> {
    if s.len() < 4 {
        return Err("Invalid UCI string".to_string());
    }

    let from = Square::from_str(&s[0..2])?;
    let to = Square::from_str(&s[2..4])?;

    let promotion = if s.len() == 5 {
        match &s[4..5] {
            "q" => Some(PieceType::Queen),
            "r" => Some(PieceType::Rook),
            "b" => Some(PieceType::Bishop),
            "n" => Some(PieceType::Knight),
            _ => None,
        }
    } else {
        None
    };

    Ok((from, to, promotion))
}

pub fn generate_uci(from: Square, to: Square, prom: Option<PieceType>) -> String {
    let mut s = format!("{}{}", from, to);
    if let Some(promotion) = prom {
        s.push(match promotion {
            PieceType::Queen => 'q',
            PieceType::Rook => 'r',
            PieceType::Bishop => 'b',
            PieceType::Knight => 'n',
            _ => return s, // Only handle promotions for these pieces
        });
    }
    s
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_move_from_uci() {
        assert_eq!(parse_uci("e2e4"), Ok((Square::E2, Square::E4, None)));
        assert_eq!(parse_uci("e7e8q"), Ok((Square::E7, Square::E8, Some(PieceType::Queen))));
        assert_eq!(parse_uci("a1b2"), Ok((Square::A1, Square::B2, None)));
        assert_eq!(parse_uci("h7h8n"), Ok((Square::H7, Square::H8, Some(PieceType::Knight))));
        assert_eq!(parse_uci("invalid"), Err("Invalid file character: i".to_string()));
        assert_eq!(parse_uci("e2"), Err("Invalid UCI string".to_string()));
    }

    #[test]
    fn test_uci_from_move() {
        let m1 = (Square::E2, Square::E4, None);
        let m2 = (Square::E7, Square::E8, Some(PieceType::Queen));
        let m3 = (Square::A1, Square::B2, None);
        let m4 = (Square::H7, Square::H8, Some(PieceType::Knight));
        assert_eq!(generate_uci(m1.0, m1.1, m1.2), "e2e4");
        assert_eq!(generate_uci(m2.0, m2.1, m2.2), "e7e8q");
        assert_eq!(generate_uci(m3.0, m3.1, m3.2), "a1b2");
        assert_eq!(generate_uci(m4.0, m4.1, m4.2), "h7h8n");
    }

}