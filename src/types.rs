use std::fmt::Display;
use std::ops::Add;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::board::BoardState;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Direction {
    N,
    S,
    E,
    W,
    NE,
    NW,
    SE,
    SW,

    // applicable for knight moves
    NNE,
    NNW,
    SSE,
    SSW,
    ENE,
    ESE,
    WNW,
    WSW,
}

/// Represents a square on a chessboard using (row, col) coordinates, where both values are in the range 0..8.
///
/// The `Square` struct encapsulates the position of a square on an 8x8 chessboard.
/// - The first field (`usize`) represents the row (rank), with 0 being the lowest rank (rank 1 in chess notation).
/// - The second field (`usize`) represents the column (file), with 0 being the leftmost file (file 'a' in chess notation).
///
/// # Examples
///
/// ```
/// use brute_move::types::Square;
/// 
/// let square = Square::new(0, 0); // Corresponds to "a1" in chess notation
/// assert!(square.validate());
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Square(usize, usize); // (row, col) where 0 <= row, col < 8

impl Square {

    pub const A1: Self = Square(0, 0);
    pub const A2: Self = Square(1, 0);
    pub const A3: Self = Square(2, 0);
    pub const A4: Self = Square(3, 0);
    pub const A5: Self = Square(4, 0);
    pub const A6: Self = Square(5, 0);
    pub const A7: Self = Square(6, 0);
    pub const A8: Self = Square(7, 0);
    pub const B1: Self = Square(0, 1);
    pub const B2: Self = Square(1, 1);
    pub const B3: Self = Square(2, 1);
    pub const B4: Self = Square(3, 1);
    pub const B5: Self = Square(4, 1);
    pub const B6: Self = Square(5, 1);
    pub const B7: Self = Square(6, 1);
    pub const B8: Self = Square(7, 1);
    pub const C1: Self = Square(0, 2);
    pub const C2: Self = Square(1, 2);
    pub const C3: Self = Square(2, 2);
    pub const C4: Self = Square(3, 2);
    pub const C5: Self = Square(4, 2);
    pub const C6: Self = Square(5, 2);
    pub const C7: Self = Square(6, 2);
    pub const C8: Self = Square(7, 2);
    pub const D1: Self = Square(0, 3);
    pub const D2: Self = Square(1, 3);
    pub const D3: Self = Square(2, 3);
    pub const D4: Self = Square(3, 3);
    pub const D5: Self = Square(4, 3);
    pub const D6: Self = Square(5, 3);
    pub const D7: Self = Square(6, 3);
    pub const D8: Self = Square(7, 3);
    pub const E1: Self = Square(0, 4);
    pub const E2: Self = Square(1, 4);
    pub const E3: Self = Square(2, 4);
    pub const E4: Self = Square(3, 4);
    pub const E5: Self = Square(4, 4);
    pub const E6: Self = Square(5, 4);
    pub const E7: Self = Square(6, 4);
    pub const E8: Self = Square(7, 4);
    pub const F1: Self = Square(0, 5);
    pub const F2: Self = Square(1, 5);
    pub const F3: Self = Square(2, 5);
    pub const F4: Self = Square(3, 5);
    pub const F5: Self = Square(4, 5);
    pub const F6: Self = Square(5, 5);
    pub const F7: Self = Square(6, 5);
    pub const F8: Self = Square(7, 5);
    pub const G1: Self = Square(0, 6);
    pub const G2: Self = Square(1, 6);
    pub const G3: Self = Square(2, 6);
    pub const G4: Self = Square(3, 6);
    pub const G5: Self = Square(4, 6);
    pub const G6: Self = Square(5, 6);
    pub const G7: Self = Square(6, 6);
    pub const G8: Self = Square(7, 6);
    pub const H1: Self = Square(0, 7);
    pub const H2: Self = Square(1, 7);
    pub const H3: Self = Square(2, 7);
    pub const H4: Self = Square(3, 7);
    pub const H5: Self = Square(4, 7);
    pub const H6: Self = Square(5, 7);
    pub const H7: Self = Square(6, 7);
    pub const H8: Self = Square(7, 7);

    pub fn is_neighbor(&self, other: &Square) -> bool {
        if !self.validate() || !other.validate() {
            return false; // Invalid squares cannot be neighbors
        }
        let dirs = [
            Direction::N, Direction::S, Direction::E, Direction::W,
            Direction::NE, Direction::NW, Direction::SE, Direction::SW,
        ];
        dirs.iter().any(|&dir| {
            let new_square = self + dir;
            new_square.is_some() && new_square.unwrap() == *other
        })
    }

    pub fn is_promotion_rank(&self, for_color: Color) -> bool {
        match for_color {
            Color::White => self.0 == 6,
            Color::Black => self.0 == 1,
        }
    }

    /// Creates a new `Square` instance with the specified rank and file.
    pub fn new(rank: usize, file: usize) -> Self {
        Self(rank, file)
    }

    /// Returns the rank (row) of the square.
    pub fn r(&self) -> usize {
        self.0
    }

    /// Returns the file (column) of the square.
    pub fn f(&self) -> usize {
        self.1
    }

    /// Returns `false` if the square is invalid (outside the board).
    pub fn validate(&self) -> bool {
        self.0 < 8 && self.1 < 8
    }

    /// Create a `Square` from a string representation like "e2" or "h8".
    pub fn from_str(s: &str) -> Result<Self, String> {
        if s.len() != 2 {
            return Err(format!("Invalid square string length: {}", s));
        }
        let file = match s.chars().nth(0) {
            Some('a'..='h') => s.chars().nth(0).unwrap() as usize - 'a' as usize,
            _ => return Err(format!("Invalid file character: {}", s.chars().nth(0).unwrap())),
        };
        let rank = match s.chars().nth(1) {
            Some('1'..='8') => s.chars().nth(1).unwrap() as usize - '1' as usize,
            _ => return Err(format!("Invalid rank character: {}", s.chars().nth(1).unwrap())),
        };
        Ok(Self(rank, file))
    }

    pub fn from_index(index: usize) -> Self {
        if index >= 64 {
            panic!("Index out of bounds for a chessboard square: {}", index);
        }
        let rank = 7 - index / 8; // Integer division to get the rank
        let file = index % 8; // Modulus to get the file
        Self(rank, file)
    }

    pub fn to_string(&self) -> String {
        let file = (self.1 as u8 + b'a') as char; // Convert file index to character
        let rank = (self.0 as u8 + b'1') as char; // Convert rank index to character
        format!("{}{}", file, rank)
    }

    pub fn is_empty(&self, pieces: &BoardState) -> bool {
        if !self.validate() {
            return true; // Invalid square is empty
        }
        pieces.get_piece(self).is_none()
    }

}

impl Add<Direction> for &Square {
    type Output = Option<Square>;

    fn add(self, rhs: Direction) -> Option<Square> {
        let sq = self.clone();
        sq + rhs
    }
}

impl Add<Direction> for Square {
    type Output = Option<Square>;

    fn add(self, rhs: Direction) -> Option<Square> {
        // Convert the direction to (dr, df)
        let (dr, df) = match rhs {
            Direction::N  => (1, 0),
            Direction::S  => (-1, 0),
            Direction::E  => (0, 1),
            Direction::W  => (0, -1),
            Direction::NE => (1, 1),
            Direction::NW => (1, -1),
            Direction::SE => (-1, 1),
            Direction::SW => (-1, -1),
            Direction::NNE => (1, 2),
            Direction::NNW => (1, -2),
            Direction::SSE => (-1, 2),
            Direction::SSW => (-1, -2),
            Direction::ENE => (2, 1),
            Direction::ESE => (2, -1),
            Direction::WNW => (-2, 1),
            Direction::WSW => (-2, -1),
        };

        let new_r = self.0 as isize + dr;
        let new_f = self.1 as isize + df;

        if new_r < 0 || new_r >= 8 || new_f < 0 || new_f >= 8 {
            None
        } else {
            Some(Square(new_r as usize, new_f as usize))
        }
    }
}

impl Display for Square {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl PartialOrd for Square {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.0 == other.0 {
            Some(self.1.cmp(&other.1))
        } else {
            Some(self.0.cmp(&other.0))
        }
    }
}

impl Ord for Square {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.0, self.1).cmp(&(other.0, other.1))
    }
}

/// Represents the different types of chess pieces.
///
/// The `PieceType` enum enumerates all possible types of pieces in chess:
/// - `Pawn`
/// - `Knight`
/// - `Bishop`
/// - `Rook`
/// - `Queen`
/// - `King`
///
/// # Examples
///
/// ```
/// use brute_move::types::PieceType;
///
/// let piece = PieceType::Queen;
/// match piece {
///     PieceType::Queen => println!("This is a queen!"),
///     _ => println!("This is not a queen."),
/// }
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PieceType {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

/// Represents the color of a chess piece.
/// The `Color` enum defines the two possible colors for chess pieces:
/// - `White`
/// - `Black`
///
/// # Examples
/// 
/// ```
/// use brute_move::types::Color;
///
/// let color = Color::White;
/// match color {
///     Color::White => println!("This is a white piece!"),
///     Color::Black => println!("This is a black piece!"),
/// }
/// 
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Copy, JsonSchema)]
pub enum Color {
    White,
    Black,
}

impl Display for Color {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Color::White => write!(f, "white"),
            Color::Black => write!(f, "black"),
        }
    }
}

impl Add<u8> for Color {
    type Output = Color;

    fn add(self, rhs: u8) -> Color {
        match self {
            Color::Black => {
                if rhs % 2 == 0 {
                    Color::Black
                } else {
                    Color::White
                }
            }
            Color::White => {
                if rhs % 2 == 0 {
                    Color::White
                } else {
                    Color::Black
                }
            }
        }
    }
}

/// Represents a chess piece with its type and color.
/// The `Piece` struct combines a `PieceType` and a `Color` to represent a chess piece.
/// 
/// # Examples
///
/// ```
/// use brute_move::types::{Piece, PieceType, Color};
/// let piece = Piece {
///     piece_type: PieceType::Queen,
///     color: Color::White,
/// };
/// assert_eq!(piece.piece_type, PieceType::Queen);
/// assert_eq!(piece.color, Color::White);
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Piece {
    pub piece_type: PieceType,
    pub color: Color,
}

impl Piece {

    /// Returns the character representation of the piece.
    pub fn to_char(&self) -> char {
        let ch = match self.piece_type {
            PieceType::Pawn => 'P',
            PieceType::Knight => 'N',
            PieceType::Bishop => 'B',
            PieceType::Rook => 'R',
            PieceType::Queen => 'Q',
            PieceType::King => 'K',
        };
        if self.color == Color::Black {
            ch.to_ascii_lowercase()
        } else {
            ch.to_ascii_uppercase()
        }
    }
}

/// Represents the castling rights for both players.
/// The `CastlingRights` struct holds boolean flags indicating whether each player can castle kingside or queenside.
/// 
/// NOTE: The castling rights indicate whether the player has moved the king or rook involved in castling.
/// the rights are **not** affected by additional rules i.e.:
/// - The king must not be in check.
/// - The squares between the king and rook must be unoccupied.
/// - The king must not pass through or end up in a square that is attacked by an opponent's piece.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CastlingRights {
    pub white_k: bool,
    pub white_q: bool,
    pub black_k: bool,
    pub black_q: bool,
}

impl Default for CastlingRights {
    fn default() -> Self {
        return CastlingRights {
            white_k: true,
            white_q: true,
            black_k: true,
            black_q: true,
        };
    }
}

impl CastlingRights {
    pub fn to_target_squares(&self, for_color: Color) -> Vec<Square> {
        let mut res = Vec::new();
        match for_color {
            Color::White => {
                if self.white_k { res.push(Square::G1); } // Kingside castling target square
                if self.white_q { res.push(Square::C1); } // Queenside castling target square
            },
            Color::Black => {
                if self.black_k { res.push(Square::G8); } // Kingside castling target square
                if self.black_q { res.push(Square::C8); } // Queenside castling target square
            },
        }
        res
    }

    pub fn to_passing_squares(&self, for_color: Color) -> Vec<Square> {
        let mut res = Vec::new();
        match for_color {
            Color::White => {
                if self.white_k { res.push(Square::F1); } // Kingside castling passing square
                if self.white_q { res.push(Square::D1); } // Queenside castling passing square
            },
            Color::Black => {
                if self.black_k { res.push(Square::F8); } // Kingside castling passing square
                if self.black_q { res.push(Square::D8); } // Queenside castling passing square
            },
        }
        res
    }

    pub fn to_starting_squares(&self, for_color: Color) -> Vec<Square> {
        let mut res = Vec::new();
        match for_color {
            Color::White => {
                if self.white_k {
                    res.push(Square::E1); // Kingside castling starting square
                }
                if self.white_q {
                    res.push(Square::E1); // Queenside castling starting square
                }
            },
            Color::Black => {
                if self.black_k {
                    res.push(Square::E8); // Kingside castling starting square
                }
                if self.black_q {
                    res.push(Square::E8); // Queenside castling starting square
                }
            },
        }
        res
    }

    pub fn to_castling_rays(&self, color: Color) -> Vec<Square> {
        let mut res = Vec::new();
        match color {
            Color::White => {
                if self.white_q { res.extend(vec![Square::D1, Square::C1, Square::B1]); }
                if self.white_k { res.extend(vec![Square::F1, Square::G1]); }
            },
            Color::Black => {
                if self.black_k { res.extend(vec![Square::F8, Square::G8]); }
                if self.black_q { res.extend(vec![Square::D8, Square::C8, Square::B8]); }
            },
        }
        res
    }
}

/// Represents a promotion in chess.
/// The `Promotion` struct is used when a pawn reaches the opposite end of the board and can be promoted to a different piece.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Promotion(pub PieceType);

impl Promotion {
    /// Creates a new `Promotion` instance with the specified piece type.
    pub fn validate(&self) -> bool {
        matches!(self.0, PieceType::Queen | PieceType::Rook | PieceType::Bishop | PieceType::Knight)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Move {
    pub from: Square,
    pub to: Square,
    pub promotion: Option<Promotion>, // Optional promotion if the move is a pawn promotion
}

impl PartialOrd for Move {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.from.cmp(&other.from).then_with(|| self.to.cmp(&other.to)))
    }
}

impl Ord for Move {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.from.cmp(&other.from).then_with(|| self.to.cmp(&other.to))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_square_from_index() {
        let square = Square::from_index(0);
        assert_eq!(square, Square(7, 0));

        let square = Square::from_index(63);
        assert_eq!(square, Square(0, 7));
    }

    #[test]
    fn test_add_numbers_to_color() {
        let color = Color::White + 1;
        assert_eq!(color, Color::Black);

        let color = Color::Black + 2;
        assert_eq!(color, Color::Black);

        let color = Color::White + 3;
        assert_eq!(color, Color::Black);
    }

    #[test]
    fn test_color_display() {
        let white = Color::White;
        let black = Color::Black;

        assert_eq!(format!("{}", white), "white");
        assert_eq!(format!("{}", black), "black");
    }

}