use crate::types::{Color, Piece, PieceType};

pub const WR: Option<Piece> = Some(Piece {
    piece_type: PieceType::Rook,
    color: Color::White,
});

pub const WN: Option<Piece> = Some(Piece {
    piece_type: PieceType::Knight,
    color: Color::White,
});

pub const WB: Option<Piece> = Some(Piece {
    piece_type: PieceType::Bishop,
    color: Color::White,
});

pub const WQ: Option<Piece> = Some(Piece {
    piece_type: PieceType::Queen,
    color: Color::White,
});

pub const WK: Option<Piece> = Some(Piece {
    piece_type: PieceType::King,
    color: Color::White,
});

pub const WP: Option<Piece> = Some(Piece {
    piece_type: PieceType::Pawn,
    color: Color::White,
});

pub const BR: Option<Piece> = Some(Piece {
    piece_type: PieceType::Rook,
    color: Color::Black,
});

pub const BN: Option<Piece> = Some(Piece {
    piece_type: PieceType::Knight,
    color: Color::Black,
});

pub const BB: Option<Piece> = Some(Piece {
    piece_type: PieceType::Bishop,
    color: Color::Black,
});

pub const BQ: Option<Piece> = Some(Piece {
    piece_type: PieceType::Queen,
    color: Color::Black,
});

pub const BK: Option<Piece> = Some(Piece {
    piece_type: PieceType::King,
    color: Color::Black,
});

pub const BP: Option<Piece> = Some(Piece {
    piece_type: PieceType::Pawn,
    color: Color::Black,
});

pub const NN: Option<Piece> = None;
