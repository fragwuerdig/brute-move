use crate::{board::BoardState, types::{CastlingRights, Color, Piece, PieceType, Square}};

pub const STARTING_POSITION: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

fn parse_position_part(part: &str, board: &BoardState) -> Result<BoardState, String> {
    
    let mut board = board.clone();
    let rows = part.split('/').collect::<Vec<_>>();
    
    if rows.len() != 8 {
        return Err("Incorrect number of rows".to_string());
    }

    for (row_idx, row) in rows.iter().enumerate() {
        let mut col = 0;
        for ch in row.chars() {
            if ch.is_ascii_digit() {
                col += ch.to_digit(10).unwrap() as usize;
            } else {
                let color = if ch.is_uppercase() {
                    Color::White
                } else {
                    Color::Black
                };

                let piece_type = match ch.to_ascii_lowercase() {
                    'p' => PieceType::Pawn,
                    'n' => PieceType::Knight,
                    'b' => PieceType::Bishop,
                    'r' => PieceType::Rook,
                    'q' => PieceType::Queen,
                    'k' => PieceType::King,
                    _ => return Err(format!("Invalid piece character: {}", ch)),
                };

                board.set_piece(Square::new(7 - row_idx, col), Piece { color, piece_type });
                col += 1;
            }
        }
        if col != 8 {
            return Err(format!("Invalid square count in FEN row {} (is {})", row_idx, col));
        }
    }

    Ok(board)
}

fn parse_turn_part(part: &str) -> Result<Color, String> {
    match part {
        "w" => Ok(Color::White),
        "b" => Ok(Color::Black),
        _ => Err(format!("Invalid turn part: {}", part)),
    }
}

fn parse_castling_part(part: &str) -> Result<CastlingRights, String> {
    
    let mut rights = CastlingRights{
        white_k: false,
        white_q: false,
        black_k: false,
        black_q: false,
    };
    
    for ch in part.chars() {
        match ch {
            'K' => {
                if rights.white_k {
                    return Err("Duplicate white king-side castling right".to_string());
                }
                rights.white_k = true
            },
            'Q' => { 
                if rights.white_q {
                    return Err("Duplicate white queen-side castling right".to_string());
                }
                rights.white_q = true
            },
            'k' => {
                if rights.black_k {
                    return Err("Duplicate black king-side castling right".to_string());
                }
                rights.black_k = true
            },
            'q' => {
                if rights.black_q {
                    return Err("Duplicate black queen-side castling right".to_string());
                }
                rights.black_q = true
            },
            '-' => {
                if rights.white_k || rights.white_q || rights.black_k || rights.black_q {
                    return Err("no castling right '-' cannot be combined with other rights".to_string());
                }
                if part.chars().count() > 1 {
                    return Err("no castling right '-' cannot be combined with other rights".to_string());
                }
                return Ok(rights); // '-' means no castling rights
            },
            _ => return Err(format!("Invalid castling character: {}", ch)),
        }
    }

    Ok(rights)

}

fn parse_en_passant_part(part: &str, board: &BoardState) -> Result<Option<Square>, String> {

    if part.chars().count() == 1 {
        if part == "-" {
            return Ok(None); // No en passant square
        } else {
            return Err(format!("Invalid en passant part: {}", part));
        }
    }

    if part.chars().count() != 2 {
        return Err(format!("Invalid en passant part length: {}", part));
    }

    let sq = Square::from_str(part);
    if let Err(e) = sq {
        return Err(format!("Invalid en passant square: {}", e));
    }

    if let Ok(sq) = sq {
        // if white's turn, en passant square must be on rank 6
        // if black's turn, en passant square must be on rank 3
        if (board.turn == Color::White && sq.r() != 5) || (board.turn == Color::Black && sq.r() != 2) {
            return Err(format!("Invalid en passant square for turn {:?}: {}", board.turn, part));
        }

        // make sure the en passant square has the correct pawn
        // TODO: maybe move this into a general Board validation
        let opponent_pawn_row = if board.turn == Color::White { 4 } else { 3 };
        let opponent_pawn_square = Square::new(opponent_pawn_row, sq.f());
        let opponent_piece = board.get_piece(&opponent_pawn_square);

        if let Some(piece) = opponent_piece {
            if piece.piece_type != PieceType::Pawn {
                return Err(format!("En passant only valid after double move of opponent pawn"));
            }
            if piece.color == board.turn{
                return Err(format!("En passant only valid after double move of opponent pawn"));
            }
        } else {
            return Err(format!("En passant only valid after double move of opponent pawn"));
        }   

        return Ok(Some(sq));
    } else {
        return Err(format!("Invalid en passant square: {}", part));
    }

}

fn parse_positive_integer(part: &str) -> Result<u32, String> {
    part.parse::<u32>().map_err(|_| format!("Invalid positive integer: {}", part))
}

pub fn parse_fen(fen: &str) -> Result<BoardState, String> {
    let board = BoardState::new();

    let parts: Vec<&str> = fen.split_whitespace().collect();
    if parts.len() < 4 {
        return Err("FEN string must have at least 4 parts".to_string());
    }
    if parts.len() > 6 {
        return Err("FEN string has too many parts".to_string());
    }

    let positions_part = parts[0];
    let mut res = parse_position_part(positions_part, &board)?;

    let turn_part = parts[1];
    let turn = parse_turn_part(turn_part)?;
    res.set_turn(turn);

    let castling_part = parts[2];
    let castling_rights = parse_castling_part(castling_part)?;
    res.set_castling_rights(castling_rights);

    let en_passant_part = parts[3];
    let en_passant_square = parse_en_passant_part(en_passant_part, &res)?;
    res.set_en_passant(en_passant_square);

    let halfmove_part = parts.get(4).unwrap_or(&"0");
    let halfmove_clock = parse_positive_integer(halfmove_part)?;
    res.set_halfmove_clock(halfmove_clock);

    let fullmove_part = parts.get(5).unwrap_or(&"1");
    let fullmove_number = parse_positive_integer(fullmove_part)?;
    if fullmove_number < 1 {
        return Err(format!("Fullmove number must be at least 1, got: {}", fullmove_number));
    }
    res.set_fullmove_number(fullmove_number);

    Ok(res)
}

pub fn generate_fen(board: &BoardState, brief: bool) -> String {
    let mut fen = String::new();

    // Generate position part
    for r in (0..8).rev() {
        let mut empty_count = 0;
        for f in 0..8 {
            let square = Square::new(r, f);
            if let Some(piece) = board.get_piece(&square) {
                if empty_count > 0 {
                    fen.push_str(&empty_count.to_string());
                    empty_count = 0;
                }
                fen.push(piece.to_char());
            } else {
                empty_count += 1;
            }
        }
        if empty_count > 0 {
            fen.push_str(&empty_count.to_string());
        }
        if r > 0 {
            fen.push('/');
        }
    }

    // Add turn part
    fen.push(' ');
    fen.push(if board.turn == Color::White { 'w' } else { 'b' });

    // Add castling rights part
    fen.push(' ');
    let rights = board.castling_rights.clone();
    if rights.white_k { fen.push('K'); }
    if rights.white_q { fen.push('Q'); }
    if rights.black_k { fen.push('k'); }
    if rights.black_q { fen.push('q'); }
    if !rights.white_k && !rights.white_q && !rights.black_k && !rights.black_q {
        fen.push('-'); // No castling rights
    }

    // Add en passant part
    fen.push(' ');
    if let Some(en_passant) = board.en_passant.clone() {
        fen.push_str(&en_passant.to_string());
    } else {
        fen.push('-');
    }

    // Add halfmove clock and fullmove number
    if !brief {
        fen.push_str(&format!(" {} {}", board.halfmove_clock, board.fullmove_number));
    }
    
    fen
}
  
#[cfg(test)]
mod tests {
    use super::*;
    use crate::{board::BoardState, constants::{BR, NN, WR}};

    #[test]
    fn test_parse_fen_default_board() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let board = parse_fen(fen).unwrap();
        let expected_board = BoardState::default();
        assert_eq!(board, expected_board);
    }

    #[test]
    fn test_parse_fen_empty_board() {
        let fen = "8/8/8/8/8/8/8/8 w KQkq - 0 1";
        let board = parse_fen(fen).unwrap();
        let expected_board = BoardState::new();
        assert_eq!(board, expected_board);
    }

    #[test]
    fn test_parse_fen_brief_notation() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -";
        let board = parse_fen(fen).unwrap();
        let expected_board = BoardState::default();
        assert_eq!(board, expected_board);
    }

    #[test]
    fn test_parse_fen_brief_notation_with_halfmove() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 25";
        let result = parse_fen(fen).unwrap();
        let mut expected_board = BoardState::default();
        expected_board.set_halfmove_clock(25);
        assert_eq!(result, expected_board);
    }

    #[test]
    fn test_parse_fen_invalid_row_count() {

        // too many rows
        let fen = "rnbqkbnr/ppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR/8 w KQkq - 0 1";
        let result = parse_fen(fen);
        assert!(result.is_err());
        assert_eq!(result.err().unwrap(), "Incorrect number of rows");

        // too few rows
        let fen = "rnbqkbnr/ppppppp/8/8/8/8/PPPPPPPP w KQkq - 0 1";
        let result = parse_fen(fen);
        assert!(result.is_err());
        assert_eq!(result.err().unwrap(), "Incorrect number of rows");

    }

    #[test]
    fn test_parse_fen_invalid_column_count() {
        
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let result = parse_fen(fen);
        assert!(result.is_err());
        assert_eq!(result.err().unwrap(), "Invalid square count in FEN row 6 (is 9)");
        
        let fen = "rnbqkbnr/pppppppp/8/8/8/7/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let result = parse_fen(fen);
        assert!(result.is_err());
        assert_eq!(result.err().unwrap(), "Invalid square count in FEN row 5 (is 7)");

        let fen = "rnbqkbnr/pppppppp/8/8/8/R2p/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let result = parse_fen(fen);
        assert!(result.is_err());
        assert_eq!(result.err().unwrap(), "Invalid square count in FEN row 5 (is 4)");

        let fen = "rnbqkbnr/pppppppp/8/8/8/R2p3r1/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let result = parse_fen(fen);
        assert!(result.is_err());
        assert_eq!(result.err().unwrap(), "Invalid square count in FEN row 5 (is 9)");
    
    }

    #[test]
    fn test_parse_fen_invalid_piece_character() {
        let fen = "rnbqkbnx/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let result = parse_fen(fen);
        assert!(result.is_err());
        assert_eq!(result.err().unwrap(), "Invalid piece character: x");
    }

    #[test]
    fn test_parse_fen_invalid_turn_part() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR x KQkq - 0 1";
        let result = parse_fen(fen);
        assert!(result.is_err());
        assert_eq!(result.err().unwrap(), "Invalid turn part: x");
    }

    #[test]
    fn test_parse_fen_double_castling_rights() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQKQ - 0 1";
        let result = parse_fen(fen);
        assert!(result.is_err());
        assert_eq!(result.err().unwrap(), "Duplicate white king-side castling right");
    }

    #[test]
    fn test_parse_fen_invalid_castling_character() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQx - 0 1";
        let result = parse_fen(fen);
        assert!(result.is_err());
        assert_eq!(result.err().unwrap(), "Invalid castling character: x");
    }

    #[test]
    fn test_parse_fen_invalid_combined_castling_rights() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w K- - 0 1";
        let result = parse_fen(fen);
        assert!(result.is_err());
        assert_eq!(result.err().unwrap(), "no castling right '-' cannot be combined with other rights");

        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w -K - 0 1";
        let result = parse_fen(fen);
        assert!(result.is_err());
        assert_eq!(result.err().unwrap(), "no castling right '-' cannot be combined with other rights");
    }

    #[test]
    fn test_parse_fen_invalid_no_castling_rights() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1";
        let result = parse_fen(fen).unwrap();
        let mut expected = BoardState::default();
        expected.set_castling_rights(CastlingRights {
            white_k: false,
            white_q: false,
            black_k: false,
            black_q: false,
        });
        assert_eq!(result, expected);
    }

    #[test]
    fn test_parse_fen_some_castling_rights() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w qK - 0 1";
        let result = parse_fen(fen).unwrap();
        let mut expected = BoardState::default();
        expected.set_castling_rights(CastlingRights {
            white_k: true,
            white_q: false,
            black_k: false,
            black_q: true,
        });
        assert_eq!(result, expected);
    }

    #[test]
    fn test_parse_fen_en_passant_square() {
        let fen = "rnbqkbnr/pppp1ppp/8/4p3/8/8/PPPPPPPP/RNBQKBNR w KQkq e6 0 1";
        let result = parse_fen(fen).unwrap();
        let mut expected = BoardState::default();
        expected.pieces = result.pieces.clone();
        expected.set_en_passant(Some(Square::new(5, 4)));
        assert_eq!(result, expected);
    }

    #[test]
    fn test_parse_fen_en_passant_square_cannot_be_due_to_turn() {
        let fen = "rnbqkbnr/pppp1ppp/8/4p3/8/8/PPPPPPPP/RNBQKBNR b KQkq e6 0 1";
        let result = parse_fen(fen).unwrap_err();
        assert_eq!(result, "Invalid en passant square for turn Black: e6");
    }

    #[test]
    fn test_parse_fen_en_passant_square_cannot_be_no_opponent_pawn_double_move() {
        let fen = "rnbqkbnr/pppppppp/8/4P3/8/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 1";
        let result = parse_fen(fen).unwrap_err();
        assert_eq!(result, "En passant only valid after double move of opponent pawn");

        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq e6 0 1";
        let result = parse_fen(fen).unwrap_err();
        assert_eq!(result, "En passant only valid after double move of opponent pawn");
    }

    #[test]
    fn test_serialize_fen_default_board() {
        let board = BoardState::default();
        let fen = generate_fen(&board, false);
        assert_eq!(fen, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    }

    #[test]
    fn test_serialize_fen_empty_board() {
        let board = BoardState::new();
        let fen = generate_fen(&board, false);
        assert_eq!(fen, "8/8/8/8/8/8/8/8 w KQkq - 0 1");
    }

    #[test]
    fn test_serialize_fen_rook_fence() {
        let mut board = BoardState::new();
        board.with_pieces([
            NN, BR, NN, WR, NN, BR, NN, WR,
            BR, NN, WR, NN, BR, NN, WR, NN,
            NN, BR, NN, WR, NN, BR, NN, WR,
            BR, NN, WR, NN, NN, NN, WR, NN,
            NN, BR, NN, NN, NN, BR, NN, WR,
            BR, NN, WR, NN, BR, NN, WR, NN,
            NN, BR, NN, WR, NN, BR, NN, WR,
            BR, NN, WR, NN, BR, NN, WR, NN,
        ]);
        board.set_turn(Color::White);
        board.set_castling_rights(CastlingRights {
            white_k: true,
            white_q: false,
            black_k: false,
            black_q: true,
        });
        board.set_en_passant(Some(Square::new(5, 4)));
        board.set_halfmove_clock(10);
        board.set_fullmove_number(2);

        let fen = generate_fen(&board, false);
        assert_eq!(fen, "1r1R1r1R/r1R1r1R1/1r1R1r1R/r1R3R1/1r3r1R/r1R1r1R1/1r1R1r1R/r1R1r1R1 w Kq e6 10 2");
    }

}