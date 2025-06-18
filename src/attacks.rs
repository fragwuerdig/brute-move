use crate::types::{Color, Direction, Move, PieceType, Promotion, Square};
use crate::board::BoardState;

fn squares_are_empty(
    blocked: &BoardState,
    squares: Vec<Square>,
) -> bool {
    squares.iter().all(|sq| { blocked.get_piece(sq).is_none() })
}

fn get_castling_ray(target: &Square) -> Vec<Square> {
    if !target.validate() {
        return Vec::new();
    }
    match target {
        &Square::G1 => vec![Square::F1, Square::G1],
        &Square::G8 => vec![Square::F8, Square::G8],
        &Square::C1 => vec![Square::D1, Square::C1, Square::B1],
        &Square::C8 => vec![Square::D8, Square::C8, Square::B8],
        _ => Vec::new(),
    }
}

fn push_square_and_break_if_needed(
    rays: &mut Vec<Square>,
    color: Color,
    attacked_square: &Square,
    blocked: &BoardState,
) -> bool {
    if !attacked_square.validate() {
        return true;
    }

    match blocked.get_piece(attacked_square) {
        Some(p) => {
            if p.color != color {
                rays.push(attacked_square.clone());
            }
            true
        }
        None => {
            rays.push(attacked_square.clone());
            false
        }
    }
}

fn scan_direction(
    rays: &mut Vec<Square>,
    color: Color,
    blocked: &BoardState,
    start: &Square,
    dir: Direction,
    max_radius: u8,
) {
    let mut distance_traveled: u8 = 0;
    let mut current = start.clone();
    loop {
        if distance_traveled >= max_radius {
            break; // Stop if we have reached the maximum radius
        }
        let attacked_square: Option<Square> = current + dir;
        if let Some(new_square) = attacked_square {
            let is_break = push_square_and_break_if_needed(rays, color, &new_square, blocked);
            current = new_square;
            if is_break {
                break;
            }
        } else {
            break; // If the square is out of bounds, stop scanning
        }
        distance_traveled += 1;
    }
}

/// Calculate all attacked squares by a rook at the current square.
/// This function scans in all four cardinal directions (up, down, left, right)
/// it raycasts from the current square until it hits the edge of the board or a piece.
/// a square is considered attacked if
/// - is within the free view of the rook
/// - it is either empty or occupied by an opponent's piece
/// 
/// Input:
/// - `current`: The square where the rook is located.
/// - `color`: The color of the rook.
/// - `blocked`: The current current `BoardState`
/// 
/// Returns:
/// - a `Vec<Square>` containing all squares attacked by the rook.
/// 
pub fn rook_attacks(current: &Square, color: Color, blocked: &BoardState, max_radius: u8) -> Vec<Square> {
    if !current.validate() {
        return Vec::new();
    }
    let mut rays = Vec::new();
    for dir in [Direction::N, Direction::S, Direction::E, Direction::W] {
        scan_direction(&mut rays, color, blocked, current, dir, max_radius);
    }
    rays.sort();
    rays
}

pub fn bishop_attacks(current: &Square, color: Color, blocked: &BoardState, max_radius: u8) -> Vec<Square> {
    if !current.validate() {
        return Vec::new();
    }
    let mut rays = Vec::new();
    for dir in [Direction::NE, Direction::NW, Direction::SE, Direction::SW] {
        scan_direction(&mut rays, color, blocked, current, dir, max_radius);
    }
    rays.sort();
    rays
}

pub fn queen_attacks(current: &Square, color: Color, blocked: &BoardState, max_radius: u8) -> Vec<Square> {
    if !current.validate() {
        return Vec::new();
    }
    let mut rays = rook_attacks(current, color, blocked, max_radius);
    rays.extend(bishop_attacks(current, color, blocked, max_radius));
    rays
}

pub fn king_attacks(current: &Square, color: Color, blocked: &BoardState) -> Vec<Square> {
    if !current.validate() {
        return Vec::new();
    }
    let mut rays = queen_attacks(current, color, blocked, 1);
    rays.sort();
    rays
}

pub fn king_castling_targets(color: Color, board: &BoardState) -> Vec<Square> {
    let targets = board.castling_rights.to_target_squares(color);
    let passing = board.castling_rights.to_passing_squares(color);
    let starting = board.castling_rights.to_starting_squares(color);
    
    if starting.is_empty() {
        return Vec::new(); // No castling rights for this color
    }

    if is_attacked(starting[0].clone(), color+1, board) {
        return Vec::new(); // Cannot castle if the starting square is attacked
    }

    let non_attacked_targets: Vec<_> = targets
        .into_iter()
        .filter(|sq|  !is_attacked(sq.clone(), color+1, board))
        .collect();

    let non_attacked_passing: Vec<_> = passing
        .into_iter()
        .filter(|sq| !is_attacked(sq.clone(), color+1, board))
        .collect();

    // only yield the non-attacked targets
    // that have non-attacked passing squares
    // as neighbors make use of the method
    // is_neighbor in the Square struct
    let res: Vec<Square> = non_attacked_targets
        .into_iter()
        .filter(|target| {
            non_attacked_passing.iter().any(|passing_sq| {
                passing_sq.is_neighbor(target)
            })
        })
        .collect();

    // filter out targets that have a non-empty castling rays
    let mut res: Vec<Square> = res.into_iter().filter(|sq| {
        let castling_ray = get_castling_ray(sq);
        if !squares_are_empty(board, castling_ray) {
            return false; // If the castling ray is not empty, skip this target
        }
        true
    }).collect();

    res.sort();
    res

}

pub fn knight_attacks(current: &Square, color: Color, blocked: &BoardState) -> Vec<Square> {
    if !current.validate() {
        return Vec::new();
    }
    let mut knight_moves = [
        current + Direction::NNE,
        current + Direction::NNW,
        current + Direction::SSE,
        current + Direction::SSW,
        current + Direction::ENE,
        current + Direction::ESE,
        current + Direction::WNW,
        current + Direction::WSW,
    ].into_iter()
        .filter(|sq| sq.is_some())
        .map(|sq| sq.unwrap())
        .filter(|sq| {
            let piece = blocked.get_piece(sq);
            match piece {
                Some(p) => p.color != color, // Only add if the piece is not of the same color
                None => true, // Empty squares are always added
            }
        })
        .collect::<Vec<Square>>();
    knight_moves.sort();
    knight_moves
}

pub fn pawn_moves(current: &Square, color: Color, blocked: &BoardState) -> Vec<Square> {
    if !current.validate() {
        return Vec::new();
    }

    let mut moves = Vec::new();
    let direction = match color {
        Color::White => 1,  // White pawns move up the board
        Color::Black => -1, // Black pawns move down the board
    };

    // Forward move
    let forward_rank = current.r() as i8 + direction;
    if forward_rank >= 0 && forward_rank < 8 {
        let forward_square = Square::new(forward_rank as usize, current.f());
        if forward_square.validate() && blocked.get_piece(&forward_square).is_none() {
            moves.push(forward_square);

            // Double move from starting position
            if is_pawn_starting_square(current, color) {
                let double_forward_rank = current.r() as i8 + 2 * direction;
                let double_forward_square = Square::new(double_forward_rank as usize, current.f());
                if double_forward_square.validate() && blocked.get_piece(&double_forward_square).is_none() {
                    moves.push(double_forward_square);
                }
            }
        }
    }

    moves
}

pub fn is_pawn_ending_square(square: &Square, color: Color) -> bool {
    match color {
        Color::White => square.r() == 7, // White pawns reach the 8th rank
        Color::Black => square.r() == 0, // Black pawns reach the 1st rank
    }
}

pub fn is_pawn_starting_square(square: &Square, color: Color) -> bool {
    match color {
        Color::White => square.r() == 1,
        Color::Black => square.r() == 6,
    }
}

pub fn pawn_attacks(current: &Square, color: Color) -> Vec<Square> {
    if !current.validate() {
        return Vec::new();
    }

    let mut attacks = Vec::new();
    let direction: i8 = match color {
        Color::White => 1,  // White pawns attack up the board
        Color::Black => -1, // Black pawns attack down the board
    };

    // Capture moves
    for file_offset in [-1, 1] {
        let capture_file = current.f() as i8 + file_offset;
        if capture_file < 0 || capture_file >= 8 {
            continue; // Skip out of bounds files
        }
        let capture_square = Square::new((current.r() as i8 + direction) as usize, capture_file as usize);
        if capture_square.validate() {
            attacks.push(capture_square);
        }
    }

    attacks
}

fn pawn_promotion_moves( 
    input_moves: Vec<Move>,
    color: Color,
) -> Vec<Move> {
    // hm - actuall, it's not possible for a pawn to have promoting and non-promoting moves at the same time
    let (promo_moves, non_promo_moves): (Vec<_>, Vec<_>) = input_moves
        .into_iter()
        .partition(|mv| mv.from.is_promotion_rank(color));
    let mut extend_promo_moves = Vec::new();
    for mv in promo_moves {
        for promo in [
            PieceType::Queen,
            PieceType::Rook,
            PieceType::Bishop,
            PieceType::Knight,
        ] {
            let mut promoted_move = mv.clone();
            promoted_move.promotion = Some(Promotion {
                0: promo,
            });
            extend_promo_moves.push(promoted_move);
        }
    }
    extend_promo_moves.extend(non_promo_moves);
    extend_promo_moves.sort();
    extend_promo_moves
}

pub fn is_attacked(sq: Square, by_color: Color, board: &BoardState) -> bool {
    // check if the square is attacked by any piece of the opposite color
    for (i, piece) in board.pieces.iter().enumerate() {
        let from_sq = Square::from_index(i);
        if from_sq == sq {
            continue; // Skip the square itself
        }
        if let Some(p) = piece {
            if p.color != by_color {
                continue; // Skip pieces that are not of the attacking color
            }
            match p.piece_type {
                PieceType::Pawn => {
                    if pawn_attacks(&from_sq, p.color).contains(&sq) {
                        return true;
                    }
                }
                PieceType::Knight => {
                    if knight_attacks(&from_sq, p.color, board).contains(&sq) {
                        return true;
                    }
                }
                PieceType::Bishop => {
                    if bishop_attacks(&from_sq, p.color, board, 8).contains(&sq) {
                        return true;
                    }
                }
                PieceType::Rook => {
                    if rook_attacks(&from_sq, p.color, board, 8).contains(&sq) {
                        return true;
                    }
                }
                PieceType::Queen => {
                    if queen_attacks(&from_sq, p.color, board, 8).contains(&sq) {
                        return true;
                    }
                }
                PieceType::King => {
                    if king_attacks(&from_sq, p.color, board).contains(&sq) {
                        return true;
                    }
                }
            }
        }
    }
    false
}

pub fn collect_all_pseudo_legal_moves(board: &BoardState, color: Color) -> Vec<Move> {
    let mut moves = Vec::new();

    for (i, piece_opt) in board.pieces.iter().enumerate() {
        if let Some(piece) = piece_opt {
            if piece.color != color {
                continue; // Only consider pieces of the current color
            }

            let from = Square::from_index(i);
            let attacks = match piece.piece_type {
                PieceType::Pawn => {
                    let mut moves = pawn_moves(&from, color, board);
                    let attacks = pawn_attacks(&from, color);
                    moves.extend(attacks);
                    let moves = moves.into_iter().map(|to| Move {
                        from: from.clone(),
                        to,
                        promotion: None,
                    }).collect();
                    let moves = pawn_promotion_moves(moves, color);
                    moves
                }
                PieceType::Knight => {
                    let mut moves = knight_attacks(&from, color, board);
                    let attacks = knight_attacks(&from, color, board);
                    moves.extend(attacks);
                    let moves = moves.into_iter().map(|to| Move {
                        from: from.clone(),
                        to,
                        promotion: None,
                    }).collect();
                    moves
                },
                PieceType::Bishop => {
                    bishop_attacks(&from, color, board, 8)
                        .into_iter()
                        .map(|to| Move {
                            from: from.clone(),
                            to,
                            promotion: None,
                        })
                        .collect()
                },
                PieceType::Rook => {
                    rook_attacks(&from, color, board, 8)
                        .into_iter()
                        .map(|to| Move {
                            from: from.clone(),
                            to,
                            promotion: None,
                        })
                        .collect()
                },
                PieceType::Queen => {
                    queen_attacks(&from, color, board, 8)
                        .into_iter()
                        .map(|to| Move {
                            from: from.clone(),
                            to,
                            promotion: None,
                        })
                        .collect()
                },
                PieceType::King => {
                    let attacks = king_attacks(&from, color, board);
                    let castling_targets = king_castling_targets(color, board);
                    let mut moves = attacks.into_iter().map(|to| Move {
                        from: from.clone(),
                        to,
                        promotion: None,
                    }).collect::<Vec<_>>();
                    moves.extend(castling_targets.into_iter().map(|to| Move {
                        from: from.clone(),
                        to,
                        promotion: None,
                    }));
                    moves
                },
            };
            moves.extend(attacks);
        }
    }
    moves

}

#[cfg(test)]
mod tests {
    use crate::types::CastlingRights;

    use super::*;
    use super::super::constants::{NN, WP, WR, BP, BR, WN, WB, BB};

    #[test]
    fn test_pawn_promotion_moves() {
        
        let board = BoardState::new();
        let current = Square::E7;
        let mut attacks = pawn_attacks(&current, Color::White);
        let moves = pawn_moves(&current, Color::White, &board);
        attacks.extend(moves);
        let moves: Vec<Move> = attacks.into_iter().map(|to| Move {
            from: current.clone(),
            to,
            promotion: None,
        }).collect();
        let promo = pawn_promotion_moves(moves.clone(), Color::White);
        let mut expected = vec![
            Move { from: Square::E7, to: Square::E8, promotion: Some(Promotion(PieceType::Queen)) },
            Move { from: Square::E7, to: Square::E8, promotion: Some(Promotion(PieceType::Rook)) },
            Move { from: Square::E7, to: Square::E8, promotion: Some(Promotion(PieceType::Bishop)) },
            Move { from: Square::E7, to: Square::E8, promotion: Some(Promotion(PieceType::Knight)) },
            Move { from: Square::E7, to: Square::F8, promotion: Some(Promotion(PieceType::Queen)) },
            Move { from: Square::E7, to: Square::F8, promotion: Some(Promotion(PieceType::Rook)) },
            Move { from: Square::E7, to: Square::F8, promotion: Some(Promotion(PieceType::Bishop)) },
            Move { from: Square::E7, to: Square::F8, promotion: Some(Promotion(PieceType::Knight)) },
            Move { from: Square::E7, to: Square::D8, promotion: Some(Promotion(PieceType::Queen)) },
            Move { from: Square::E7, to: Square::D8, promotion: Some(Promotion(PieceType::Rook)) },
            Move { from: Square::E7, to: Square::D8, promotion: Some(Promotion(PieceType::Bishop)) },
            Move { from: Square::E7, to: Square::D8, promotion: Some(Promotion(PieceType::Knight)) },
        ];
        expected.sort();
        assert_eq!(promo, expected);

        // Test with no promotion moves
        let current = Square::E5;
        let mut attacks = pawn_attacks(&current, Color::White);
        let moves = pawn_moves(&current, Color::White, &board);
        attacks.extend(moves);
        let mut moves: Vec<Move> = attacks.into_iter().map(|to| Move {
            from: current.clone(),
            to,
            promotion: None,
        }).collect();
        moves.sort();
        let promo = pawn_promotion_moves(moves.clone(), Color::White);
        assert_eq!(promo, moves);
    }

    #[test]
    fn test_rook_attacks_invalid_from() {
        let board = BoardState::new();
        
        // invalid "from" square
        let current = Square::new(8, 8);
        let rays = rook_attacks(&current, Color::White, &board, 8);
        assert!(rays.is_empty());
    }

    #[test]
    fn test_rook_attacks_from_corner_free_view() {

        let board = BoardState::new();
        
        let from = Square::new(0, 0);
        let rays = rook_attacks(&from, Color::White, &board, 8);
        assert_eq!(rays, vec![
            Square::new(0, 1),
            Square::new(0, 2),
            Square::new(0, 3),
            Square::new(0, 4),
            Square::new(0, 5),
            Square::new(0, 6),
            Square::new(0, 7),
            Square::new(1, 0),
            Square::new(2, 0),
            Square::new(3, 0),
            Square::new(4, 0),
            Square::new(5, 0),
            Square::new(6, 0),
            Square::new(7, 0),
        ]);

        let from = Square::new(7, 7);
        let rays = rook_attacks(&from, Color::White, &board, 8);
        assert_eq!(rays, vec![
            Square::new(0, 7),
            Square::new(1, 7),
            Square::new(2, 7),
            Square::new(3, 7),
            Square::new(4, 7),
            Square::new(5, 7),
            Square::new(6, 7),
            Square::new(7, 0),
            Square::new(7, 1),
            Square::new(7, 2),
            Square::new(7, 3),
            Square::new(7, 4),
            Square::new(7, 5),
            Square::new(7, 6),
        ]);

        let from = Square::new(0, 7);
        let rays = rook_attacks(&from, Color::White, &board, 8);
        assert_eq!(rays, vec![
            Square::new(0, 0),
            Square::new(0, 1),
            Square::new(0, 2),
            Square::new(0, 3),
            Square::new(0, 4),
            Square::new(0, 5),
            Square::new(0, 6),
            Square::new(1, 7),
            Square::new(2, 7),
            Square::new(3, 7),
            Square::new(4, 7),
            Square::new(5, 7),
            Square::new(6, 7),
            Square::new(7, 7),
        ]);

        let from = Square::new(7, 0);
        let rays = rook_attacks(&from, Color::White, &board, 8);
        assert_eq!(rays, vec![
            Square::new(0, 0),
            Square::new(1, 0),
            Square::new(2, 0),
            Square::new(3, 0),
            Square::new(4, 0),
            Square::new(5, 0),
            Square::new(6, 0),
            Square::new(7, 1),
            Square::new(7, 2),
            Square::new(7, 3),
            Square::new(7, 4),
            Square::new(7, 5),
            Square::new(7, 6),
            Square::new(7, 7)
        ]);

    }

    #[test]
    fn test_rook_attacks_from_middle_free_view() {

        let board = BoardState::new();
        let from = Square::new(3, 3);
        let rays = rook_attacks(&from, Color::White, &board, 8);

        assert_eq!(rays, vec![
            Square::new(0, 3),
            Square::new(1, 3),
            Square::new(2, 3),
            Square::new(3, 0),
            Square::new(3, 1),
            Square::new(3, 2),
            Square::new(3, 4),
            Square::new(3, 5),
            Square::new(3, 6),
            Square::new(3, 7),
            Square::new(4, 3),
            Square::new(5, 3),
            Square::new(6, 3),
            Square::new(7, 3),
        ]);

    }

    #[test]
    fn test_rook_attack_from_middle_with_blocked_squares() {
        let mut blocked = BoardState::new();
        blocked.with_pieces([
            NN, NN, NN, WP, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, WP, WR, NN, WP, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);

        let current = Square::new(3, 3);
        let rays = rook_attacks(&current, Color::White, &blocked, 8);
        let mut expected = [
            Square::new(3, 4),
            Square::new(2, 3),
            Square::new(1, 3),
            Square::new(0, 3),
            Square::new(4, 3),
            Square::new(5, 3),
            Square::new(6, 3),
        ];
        expected.sort();
        assert_eq!(rays, expected);
    
    }

    #[test]
    fn test_rook_attacks_from_corner_with_blocked_squares() {
        let mut blocked = BoardState::new();
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, WP,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, WN, NN, NN, NN, NN, NN,
        ]);

        let current = Square::new(0, 0);
        let rays = rook_attacks(&current, Color::White, &blocked, 8);
        let mut expected = vec![
            Square::new(0, 1),
            Square::new(1, 0),
            Square::new(2, 0),
            Square::new(3, 0),
            Square::new(4, 0),
            Square::new(5, 0),
            Square::new(6, 0),
            Square::new(7, 0),
        ];
        expected.sort();
        assert_eq!(rays, expected);

        let current = Square::new(7, 7);
        let rays = rook_attacks(&current, Color::White, &blocked, 8);
        let mut expected = vec![
            Square::new(7, 0),
            Square::new(7, 1),
            Square::new(7, 2),
            Square::new(7, 3),
            Square::new(7, 4),
            Square::new(7, 5),
            Square::new(7, 6),
            Square::new(6, 7),
        ];
        expected.sort();
        assert_eq!(rays, expected);

        let current = Square::new(0, 7);
        let rays = rook_attacks(&current, Color::White, &blocked, 8);
        let mut expected = vec![
            Square::new(0, 3),
            Square::new(0, 4),
            Square::new(0, 5),
            Square::new(0, 6),
            Square::new(1, 7),
            Square::new(2, 7),
            Square::new(3, 7),
            Square::new(4, 7),
        ];
        expected.sort();
        assert_eq!(rays, expected);

        let current = Square::new(7, 0);
        let rays = rook_attacks(&current, Color::White, &blocked, 8);
        let mut expected = vec![
            Square::new(7, 1),
            Square::new(7, 2),
            Square::new(7, 3),
            Square::new(7, 4),
            Square::new(7, 5),
            Square::new(7, 6),
            Square::new(7, 7),
            Square::new(6, 0),
            Square::new(5, 0),
            Square::new(4, 0),
            Square::new(3, 0),
            Square::new(2, 0),
            Square::new(1, 0),
            Square::new(0, 0),
        ];
        expected.sort();
        assert_eq!(rays, expected);

    }

    #[test]
    fn test_rook_attacks_completely_blocked() {
        let mut blocked = BoardState::new();
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, WP, NN, NN, NN, NN,
            NN, NN, WP, NN, WP, NN, NN, NN,
            NN, NN, NN, WP, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);

        let current = Square::new(3, 3);
        let rays = rook_attacks(&current, Color::White, &blocked, 8);
        assert!(rays.is_empty());
    }

    #[test]
    fn test_rook_attacks_completely_blocked_but_can_capture() {
        let mut blocked = BoardState::new();
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, BP, NN, NN, NN, NN,
            NN, NN, WP, NN, BP, NN, NN, NN,
            NN, NN, NN, WP, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);

        let current = Square::new(3, 3);
        let rays = rook_attacks(&current, Color::White, &blocked, 8);
        let mut expected = vec![
            Square::new(3, 4),
            Square::new(4, 3),
        ];
        expected.sort();
        assert_eq!(rays, expected);
    }

    #[test]
    fn test_bishop_attacks_invalid_from() {
        let blocked = BoardState::new();
        
        // invalid "from" square
        let current = Square::new(8, 8);
        let rays = bishop_attacks(&current, Color::White, &blocked, 8);
        assert!(rays.is_empty());
    }

    #[test]
    fn test_bishop_attacks_from_corner_free_view() {
        
        let blocked = BoardState::new();
        
        let from = Square::new(0, 0);
        let rays = bishop_attacks(&from, Color::White, &blocked, 8);
        let mut expected = vec![
            Square::new(1, 1),
            Square::new(2, 2),
            Square::new(3, 3),
            Square::new(4, 4),
            Square::new(5, 5),
            Square::new(6, 6),
            Square::new(7, 7),
        ];
        expected.sort();
        assert_eq!(rays, expected);

        let from = Square::new(7, 7);
        let rays = bishop_attacks(&from, Color::White, &blocked, 8);
        let mut expected = vec![
            Square::new(6, 6),
            Square::new(5, 5),
            Square::new(4, 4),
            Square::new(3, 3),
            Square::new(2, 2),
            Square::new(1, 1),
            Square::new(0, 0),
        ];
        expected.sort();
        assert_eq!(rays, expected);

        let from = Square::new(0, 7);
        let rays = bishop_attacks(&from, Color::White, &blocked, 8);
        let mut expected = vec![
            Square::new(1, 6),
            Square::new(2, 5),
            Square::new(3, 4),
            Square::new(4, 3),
            Square::new(5, 2),
            Square::new(6, 1),
            Square::new(7, 0),
        ];
        expected.sort();
        assert_eq!(rays, expected);

        let from = Square::new(7, 0);
        let rays = bishop_attacks(&from, Color::White, &blocked, 8);
        let mut expected = vec![
            Square::new(6, 1),
            Square::new(5, 2),
            Square::new(4, 3),
            Square::new(3, 4),
            Square::new(2, 5),
            Square::new(1, 6),
            Square::new(0, 7),
        ];
        expected.sort();
        assert_eq!(rays, expected);

    }

    #[test]
    fn test_bishop_attacks_from_edge_free_view() {
        
        let blocked = BoardState::new();
        
        let from = Square::new(3, 0);
        let rays = bishop_attacks(&from, Color::White, &blocked, 8);
        let mut expected = vec![
            Square::new(4, 1),
            Square::new(2, 1),
            Square::new(5, 2),
            Square::new(1, 2),
            Square::new(6, 3),
            Square::new(0, 3),
            Square::new(7, 4),
        ];
        expected.sort();
        assert_eq!(rays, expected);

        let from = Square::new(3, 7);
        let rays = bishop_attacks(&from, Color::White, &blocked, 8);
        let mut expected = vec![
            Square::new(7, 3),
            Square::new(6, 4),
            Square::new(5, 5),
            Square::new(4, 6),
            Square::new(2, 6),
            Square::new(1, 5),
            Square::new(0, 4),
        ];
        expected.sort();
        assert_eq!(rays, expected);

    }

    #[test]
    fn test_bishop_attacks_from_middle_free_view() {
        
        let blocked = BoardState::new();
        
        let from = Square::new(3, 3);
        let rays = bishop_attacks(&from, Color::White, &blocked, 8);
        let mut expected = vec![
            Square::new(4, 4),
            Square::new(5, 5),
            Square::new(6, 6),
            Square::new(7, 7),
            Square::new(2, 2),
            Square::new(1, 1),
            Square::new(0, 0),
            Square::new(4, 2),
            Square::new(5, 1),
            Square::new(6, 0),
            Square::new(2, 4),
            Square::new(1, 5),
            Square::new(0, 6),
        ];
        expected.sort();
        assert_eq!(rays, expected);

    }

    #[test]
    fn test_bishop_attacks_from_corner_and_edge_with_blocked_squares() {
        let mut blocked = BoardState::new();
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, WP, NN, NN, NN,
            NN, NN, WP, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);

        let current = Square::new(0, 0);
        let rays = bishop_attacks(&current, Color::White, &blocked, 8);
        let mut expected = vec![
            Square::new(1, 1),
        ];
        expected.sort();
        assert_eq!(rays.as_ref(), expected);

        let current = Square::new(7, 7);
        let rays = bishop_attacks(&current, Color::White, &blocked, 8);
        let mut expected = vec![
            Square::new(6, 6),
            Square::new(5, 5),
            Square::new(4, 4),
            Square::new(3, 3),
        ];
        expected.sort();
        assert_eq!(rays.as_ref(), expected);

        let current = Square::new(0, 7);
        let rays = bishop_attacks(&current, Color::White, &blocked, 8);
        let mut expected = vec![
            Square::new(1, 6),
            Square::new(2, 5),
        ];
        expected.sort();
        assert_eq!(rays.as_ref(), expected);

        let current = Square::new(7, 0);
        let rays = bishop_attacks(&current, Color::White, &blocked, 8);
        let mut expected = vec![
            Square::new(6, 1),
            Square::new(5, 2),
            Square::new(4, 3),
        ];
        expected.sort();
        assert_eq!(rays.as_ref(), expected);

        let current = Square::new(4, 0);
        let rays = bishop_attacks(&current, Color::White, &blocked, 8);
        let mut expected = vec![
            Square::new(3, 1),
            Square::new(5, 1),
            Square::new(6, 2),
            Square::new(7, 3),
        ];
        expected.sort();
        assert_eq!(rays.as_ref(), expected);

    }

    #[test]
    fn test_bishop_attacks_and_is_completely_blocked() {
        let mut blocked = BoardState::new();
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, WP, NN, WP, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, WP, NN, WP, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);

        let current = Square::new(3, 3);
        let rays = bishop_attacks(&current, Color::White,&blocked, 8);
        assert!(rays.is_empty());
    }

    #[test]
    fn test_bishop_attacks_and_is_completely_blocked_but_can_capture() {
        let mut blocked = BoardState::new();
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, BP, NN, WP, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, BP, NN, WP, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);

        let current = Square::new(3, 3);
        let rays = bishop_attacks(&current, Color::White,&blocked, 8);
        let mut expected = vec![
            Square::new(4, 2),
            Square::new(2, 2),
        ];
        expected.sort();
        assert_eq!(rays.as_ref(), expected);
    }

    #[test]
    fn test_queen_attacks_invalid_from() {
        let blocked = BoardState::new();
        
        // invalid "from" square
        let current = Square::new(8, 8);
        let rays = queen_attacks(&current, Color::White, &blocked, 8);
        assert!(rays.is_empty());
    }

    // bishop and rook are subset of queen, so we can just test the complete blockage
    #[test]
    fn test_queen_attacks_and_is_completely_blocked() {
        let mut blocked = BoardState::new();
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, WP, WR, WP, NN, NN, NN,
            NN, NN, WP, NN, WP, NN, NN, NN,
            NN, NN, WP, WR, WP, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);

        let current = Square::new(3 ,3);
        let rays = queen_attacks(&current ,Color::White,&blocked, 8);
        assert!(rays.is_empty());
    }

    #[test]
    fn test_queen_attacks_and_is_completely_blocked_but_can_capture() {
        let mut blocked = BoardState::new();
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, BP, WR, WP, NN, NN, NN,
            NN, NN, WP, NN, WP, NN, NN, NN,
            NN, NN, WP, BR, WP, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN ,NN,
        ]);

        let current = Square::new(3 ,3);
        let rays = queen_attacks(&current, Color::White,&blocked, 8);
        let mut expected = vec![
            Square::new(4, 2),
            Square::new(2, 3),
        ];
        expected.sort();
        assert_eq!(rays.as_ref(), expected);
    }

    #[test]
    fn test_king_attacks_invalid_from() {
        let blocked = BoardState::new();

        // invalid starting square
        let current = Square::new(8, 8);
        let attacks = king_attacks(&current, Color::White, &blocked);
        assert_eq!(attacks, Vec::<Square>::new());
    }

    #[test]
    fn test_king_attacks_from_corners() {
        let blocked = BoardState::new();

        let current = Square::new(0, 0);
        let attacks = king_attacks(&current, Color::White, &blocked);
        let mut expected = vec![
            Square::new(0, 1),
            Square::new(1, 0),
            Square::new(1, 1),
        ];
        expected.sort();
        assert_eq!(attacks, expected);

        let current = Square::new(7, 7);
        let attacks = king_attacks(&current, Color::White, &blocked);
        let mut expected = vec![
            Square::new(6, 6),
            Square::new(6, 7),
            Square::new(7, 6),
        ];
        expected.sort();
        assert_eq!(attacks, expected);
        
        let current = Square::new(0, 7);
        let attacks = king_attacks(&current, Color::White, &blocked);
        let mut expected = vec![
            Square::new(0, 6),
            Square::new(1, 6),
            Square::new(1, 7),
        ];
        expected.sort();
        assert_eq!(attacks, expected);

        let current = Square::new(7, 0);
        let attacks = king_attacks(&current, Color::White, &blocked);
        let mut expected = vec![
            Square::new(6, 0),
            Square::new(6, 1),
            Square::new(7, 1),
        ];
        expected.sort();
        assert_eq!(attacks, expected);

    }

    #[test]
    fn test_white_king_castling_targets_no_blockers() {
        let mut blocked = BoardState::new();
        blocked.with_castling_rights(CastlingRights {
            white_k: true,
            white_q: true,
            black_k: true,
            black_q: true,
        });

        let targets = king_castling_targets(Color::White, &blocked);
        let mut expected = vec![
            Square::new(0, 6), // kingside castling
            Square::new(0, 2), // queenside castling
        ];
        expected.sort();
        assert_eq!(targets, expected);

    }

    #[test]
    fn test_black_king_castling_targets_no_blockers() {
        let mut blocked = BoardState::new();
        blocked.with_castling_rights(CastlingRights {
            white_k: true,
            white_q: true,
            black_k: true,
            black_q: true,
        });

        let targets = king_castling_targets(Color::Black, &blocked);
        let mut expected = vec![
            Square::new(7, 6), // kingside castling
            Square::new(7, 2), // queenside castling
        ];
        expected.sort();
        assert_eq!(targets, expected);
    }

    #[test]
    fn test_white_king_castling_targets_no_castling_right() {
        let mut blocked = BoardState::new();
        blocked.with_castling_rights(CastlingRights {
            white_k: false,
            white_q: false,
            black_k: true,
            black_q: true,
        });

        // no castling rights for white
        let targets = king_castling_targets(Color::White, &blocked);
        assert_eq!(targets, vec![]); // No castling rights, so no targets

        // only kingside castling rights
        blocked.with_castling_rights(CastlingRights {
            white_k: true,
            white_q: false,
            black_k: true,
            black_q: true,
        });
        let targets = king_castling_targets(Color::White, &blocked);
        assert_eq!(targets, vec![Square::new(0, 6)]);

        // only queenside castling rights
        blocked.with_castling_rights(CastlingRights {
            white_k: false,
            white_q: true,
            black_k: true,
            black_q: true,
        });
        let targets = king_castling_targets(Color::White, &blocked);
        assert_eq!(targets, vec![Square::new(0, 2)]);
    }

    #[test]
    fn test_black_king_castling_targets_no_castling_right() {
        let mut blocked = BoardState::new();
        blocked.with_castling_rights(CastlingRights {
            white_k: true,
            white_q: true,
            black_k: false,
            black_q: false,
        });

        // no castling rights for black
        let targets = king_castling_targets(Color::Black, &blocked);
        assert_eq!(targets, vec![]); // No castling rights, so no targets

        // only kingside castling rights
        blocked.with_castling_rights(CastlingRights {
            white_k: true,
            white_q: true,
            black_k: true,
            black_q: false,
        });
        let targets = king_castling_targets(Color::Black, &blocked);
        assert_eq!(targets, vec![Square::new(7, 6)]);

        // only queenside castling rights
        blocked.with_castling_rights(CastlingRights {
            white_k: true,
            white_q: true,
            black_k: false,
            black_q: true,
        });
        let targets = king_castling_targets(Color::Black, &blocked);
        assert_eq!(targets, vec![Square::new(7, 2)]);
    }
    
    #[test]
    fn test_white_king_castling_targets_blocked_ray() {
        
        let mut blocked = BoardState::new();
        blocked.with_castling_rights(CastlingRights {
            white_k: true,
            white_q: true,
            black_k: true,
            black_q: true,
        });
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, WB, NN, WB, NN, BB, NN, NN,
        ]);

        let targets = king_castling_targets(Color::White, &blocked);
        assert_eq!(targets, vec![]);

        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, WB, NN, NN, NN, BB, NN, NN,
        ]);

        let targets = king_castling_targets(Color::White, &blocked);
        assert_eq!(targets, vec![]);

    }

    #[test]
    fn test_black_king_castling_targets_blocked_ray() {
        
        let mut blocked = BoardState::new();
        blocked.with_castling_rights(CastlingRights {
            white_k: true,
            white_q: true,
            black_k: true,
            black_q: true,
        });
        blocked.with_pieces([
            NN, WB, NN, WB, NN, BB, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);

        let targets = king_castling_targets(Color::Black, &blocked);
        assert_eq!(targets, vec![]);

        blocked.with_pieces([
            NN, WB, NN, NN, NN, BB, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);

        let targets = king_castling_targets(Color::Black, &blocked);
        assert_eq!(targets, vec![]);

    }
   
    #[test]
    fn test_white_king_castling_targets_passing_fields_are_attacked_by_enemy() {

        let mut blocked = BoardState::new();
        blocked.with_castling_rights(CastlingRights {
            white_k: true,
            white_q: true,
            black_k: true,
            black_q: true,
        });
        
        // kingside passing field attacked by enemy rook
        blocked.with_pieces([
            NN, NN, NN, NN, NN, BR, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);
        let targets = king_castling_targets(Color::White, &blocked);
        assert_eq!(targets, vec![Square::new(0, 2)]);

        // kingside target field attacked by enemy rook
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, BR, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);
        let targets = king_castling_targets(Color::White, &blocked);
        assert_eq!(targets, vec![Square::new(0, 2)]);

        // king attacked by enemy rook
        blocked.with_pieces([
            NN, NN, NN, NN, BR, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);
        let targets = king_castling_targets(Color::White, &blocked);
        assert_eq!(targets, vec![]);

        // queenside passing field attacked by enemy rook
        blocked.with_pieces([
            NN, NN, NN, BR, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);
        let targets = king_castling_targets(Color::White, &blocked);
        assert_eq!(targets, vec![Square::new(0, 6)]);

        // queenside target field attacked by enemy rook
        blocked.with_pieces([
            NN, NN, BR, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);
        let targets = king_castling_targets(Color::White, &blocked);
        assert_eq!(targets, vec![Square::new(0, 6)]);

    }

    #[test]
    fn test_white_king_castling_targets_passing_fields_are_attacked_by_friendly() {

        let mut blocked = BoardState::new();
        blocked.with_castling_rights(CastlingRights {
            white_k: true,
            white_q: true,
            black_k: true,
            black_q: true,
        });
        
        // kingside passing field attacked by enemy rook
        blocked.with_pieces([
            NN, NN, NN, NN, NN, WR, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);
        let targets = king_castling_targets(Color::White, &blocked);
        let mut expected = vec![Square::new(0, 2), Square::new(0, 6)];
        expected.sort();
        assert_eq!(targets, expected.clone());

        // kingside target field attacked by enemy rook
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, WR, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);
        let targets = king_castling_targets(Color::White, &blocked);
        assert_eq!(targets, expected.clone());

        // king attacked by enemy rook
        blocked.with_pieces([
            NN, NN, NN, NN, WR, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);
        let targets = king_castling_targets(Color::White, &blocked);
        assert_eq!(targets, expected.clone());

        // queenside passing field attacked by enemy rook
        blocked.with_pieces([
            NN, NN, NN, WR, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);
        let targets = king_castling_targets(Color::White, &blocked);
        assert_eq!(targets, expected.clone());

        // queenside target field attacked by enemy rook
        blocked.with_pieces([
            NN, NN, WR, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);
        let targets = king_castling_targets(Color::White, &blocked);
        assert_eq!(targets, expected.clone());

    }

    #[test]
    fn test_black_king_castling_targets_passing_fields_are_attacked_by_enemy() {

        let mut blocked = BoardState::new();
        blocked.with_castling_rights(CastlingRights {
            white_k: true,
            white_q: true,
            black_k: true,
            black_q: true,
        });
        
        // kingside passing field attacked by enemy rook
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, WR, NN, NN,
        ]);
        let targets = king_castling_targets(Color::Black, &blocked);
        assert_eq!(targets, vec![Square::new(7, 2)]);

        // kingside target field attacked by enemy rook
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, WR, NN,
        ]);
        let targets = king_castling_targets(Color::Black, &blocked);
        assert_eq!(targets, vec![Square::new(7, 2)]);

        // king attacked by enemy rook
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, WR, NN, NN, NN,
        ]);
        let targets = king_castling_targets(Color::Black, &blocked);
        assert_eq!(targets, vec![]);

        // queenside passing field attacked by enemy rook
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, WR, NN, NN, NN, NN,
        ]);
        let targets = king_castling_targets(Color::Black, &blocked);
        assert_eq!(targets, vec![Square::new(7, 6)]);

        // queenside target field attacked by enemy rook
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, WR, NN, NN, NN, NN, NN,
        ]);
        let targets = king_castling_targets(Color::Black, &blocked);
        assert_eq!(targets, vec![Square::new(7, 6)]);

    }

    #[test]
    fn test_black_king_castling_targets_passing_fields_are_attacked_by_friendly() {

        let mut blocked = BoardState::new();
        blocked.with_castling_rights(CastlingRights {
            white_k: true,
            white_q: true,
            black_k: true,
            black_q: true,
        });
        
        // kingside passing field attacked by enemy rook
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, BR, NN, NN,
        ]);
        let targets = king_castling_targets(Color::Black, &blocked);
        let mut expected = vec![Square::new(7, 2), Square::new(7, 6)];
        expected.sort();
        assert_eq!(targets, expected.clone());

        // kingside target field attacked by enemy rook
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, BR, NN,
        ]);
        let targets = king_castling_targets(Color::Black, &blocked);
        assert_eq!(targets, expected.clone());

        // king attacked by enemy rook
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, BR, NN, NN, NN,
        ]);
        let targets = king_castling_targets(Color::Black, &blocked);
        assert_eq!(targets, expected.clone());

        // queenside passing field attacked by enemy rook
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, BR, NN, NN, NN, NN,
        ]);
        let targets = king_castling_targets(Color::Black, &blocked);
        assert_eq!(targets, expected.clone());

        // queenside target field attacked by enemy rook
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, BR, NN, NN, NN, NN, NN,
        ]);
        let targets = king_castling_targets(Color::Black, &blocked);
        assert_eq!(targets, expected.clone());

    }

     #[test]
    fn test_white_pawn_moves_invalid_from() {
        let blocked = BoardState::new();
        let current = Square::new(8, 8); // Invalid square
        let moves = pawn_moves(&current, Color::White, &blocked);
        assert_eq!(moves, Vec::<Square>::new()); // No moves from invalid square
    }

    #[test]
    fn test_black_pawn_moves_invalid_from() {
        let blocked = BoardState::new();
        let current = Square::new(8, 8); // Invalid square
        let moves = pawn_moves(&current, Color::Black, &blocked);
        assert_eq!(moves, Vec::<Square>::new()); // No moves from invalid square
    }

    #[test]
    fn test_white_pawn_moves_no_blockers() {
        let blocked = BoardState::new();

        // from starting position
        let current = Square::new(1, 1); // White pawn on b7
        let moves = pawn_moves(&current, Color::White, &blocked);
        assert_eq!(moves, vec![
            Square::new(2, 1), // Move forward to b6
            Square::new(3, 1), // Double move to b5
        ]);

        // from non-starting position
        let current = Square::new(4, 4); // White pawn on e5
        let moves = pawn_moves(&current, Color::White, &blocked);
        assert_eq!(moves, vec![
            Square::new(5, 4), // Move forward to e6
        ]);
    }

    #[test]
    fn test_black_pawn_moves_no_blockers() {
        let blocked = BoardState::new();

        // from starting position
        let current = Square::new(6, 1); // Black pawn on b7
        let moves = pawn_moves(&current, Color::Black, &blocked);
        assert_eq!(moves, vec![
            Square::new(5, 1), // Move forward to b6
            Square::new(4, 1), // Double move to b5
        ]);

        // from non-starting position
        let current = Square::new(4, 4); // Black pawn on e5
        let moves = pawn_moves(&current, Color::Black, &blocked);
        assert_eq!(moves, vec![
            Square::new(3, 4), // Move forward to e4
        ]);
    }

    #[test]
    fn test_white_pawn_moves_from_start_is_blocked_by_friendly_or_enemy() {
        let mut blocked = BoardState::new();
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, BP, NN, WP, NN, NN, NN,
            NN, BP, NN, WP, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);

        // single and double move blocked by enemy piece
        let current = Square::new(1, 1);
        let moves = pawn_moves(&current, Color::White, &blocked);
        assert_eq!(moves, vec![]); 

        // double move blocked by enemy piece
        let current = Square::new(1, 2);
        let moves = pawn_moves(&current, Color::White, &blocked);
        assert_eq!(moves, vec![Square::new(2, 2)]); // Only single move available

        // single move blocked by friendly piece
        let current = Square::new(1, 3);
        let moves = pawn_moves(&current, Color::White, &blocked);
        assert_eq!(moves, vec![]); // No moves available

        // double move blocked by friendly piece
        let current = Square::new(1, 4);
        let moves = pawn_moves(&current, Color::White, &blocked);
        assert_eq!(moves, vec![Square::new(2, 4)]); // Only single move available
    
    }

    #[test]
    fn test_black_pawn_moves_from_start_is_blocked_by_friendly_or_enemy() {
        let mut blocked = BoardState::new();
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, WP, NN, BP, NN, NN, NN, NN,
            NN, NN, WP, NN, BP, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);

        // single and double move blocked by enemy piece
        let current = Square::new(6, 1);
        let moves = pawn_moves(&current, Color::Black, &blocked);
        assert_eq!(moves, vec![]); 

        // double move blocked by enemy piece
        let current = Square::new(6, 2);
        let moves = pawn_moves(&current, Color::Black, &blocked);
        assert_eq!(moves, vec![Square::new(5, 2)]); // Only single move available

        // single move blocked by friendly piece
        let current = Square::new(6, 3);
        let moves = pawn_moves(&current, Color::Black, &blocked);
        assert_eq!(moves, vec![]); // No moves available

        // double move blocked by friendly piece
        let current = Square::new(6, 4);
        let moves = pawn_moves(&current, Color::Black, &blocked);
        assert_eq!(moves, vec![Square::new(5, 4)]); // Only single move available
    
    }

    #[test]
    fn test_white_pawn_moves_from_intermediate_is_blocked_by_friendly_or_enemy() {
        let mut blocked = BoardState::new();
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, BP, WP, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);

        let current = Square::new(3, 2); // White pawn on b5
        let moves = pawn_moves(&current, Color::White, &blocked);
        assert_eq!(moves, vec![]);

        let current = Square::new(3, 3); // White pawn on c5
        let moves = pawn_moves(&current, Color::White, &blocked);
        assert_eq!(moves, vec![]);
    }

    #[test]
    fn test_black_pawn_moves_from_intermediate_is_blocked_by_friendly_or_enemy() {
        let mut blocked = BoardState::new();
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, WP, BP, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);

        let current = Square::new(4, 2); // White pawn on b5
        let moves = pawn_moves(&current, Color::Black, &blocked);
        assert_eq!(moves, vec![]);

        let current = Square::new(4, 3); // White pawn on c5
        let moves = pawn_moves(&current, Color::Black, &blocked);
        assert_eq!(moves, vec![]);
    }

    #[test]
    fn test_knight_attacks_from_cornes_no_blockers() {
        let blocked = BoardState::new();

        // Knight on a1
        let current = Square::new(0, 0);
        let attacks = knight_attacks(&current, Color::White, &blocked);
        let mut expected = vec![
            Square::new(2, 1), // b3
            Square::new(1, 2), // c2
        ];
        expected.sort();
        assert_eq!(attacks, expected);

        // Knight on h1
        let current = Square::new(0, 7);
        let attacks = knight_attacks(&current, Color::White, &blocked);
        let mut expected = vec![
            Square::new(2, 6), // g3
            Square::new(1, 5), // f2
        ];
        expected.sort();
        assert_eq!(attacks, expected);

        // Knight on a8
        let current = Square::new(7, 0);
        let attacks = knight_attacks(&current, Color::White, &blocked);
        let mut expected = vec![
            Square::new(5, 1), // b6
            Square::new(6, 2), // c7
        ];
        expected.sort();
        assert_eq!(attacks, expected);

        // Knight on h8
        let current = Square::new(7, 7);
        let attacks = knight_attacks(&current, Color::White, &blocked);
        let mut expected = vec![
            Square::new(5, 6), // g7
            Square::new(6, 5), // f6
        ];
        expected.sort();
        assert_eq!(attacks, expected);
    }

    #[test]
    fn test_knight_attacks_from_middle_no_blockers() {
        let blocked = BoardState::new();

        // Knight on d4
        let current = Square::new(3, 3);
        let attacks = knight_attacks(&current, Color::White, &blocked);
        let mut expected = vec![
            Square::new(1, 2), // b3
            Square::new(1, 4), // b5
            Square::new(2, 1), // c2
            Square::new(2, 5), // c6
            Square::new(4, 1), // e2
            Square::new(4, 5), // e6
            Square::new(5, 2), // f3
            Square::new(5, 4), // f5
        ];
        expected.sort();
        assert_eq!(attacks, expected);
    }

    #[test]
    fn test_knight_attacks_from_middle_is_completely_blocked() {
        let mut blocked = BoardState::new();
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, WP, NN, WP, NN, NN, NN,
            NN, WP, NN, NN, NN, WP, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, WP, NN, NN, NN, WP, NN, NN,
            NN, NN, WP, NN, WP, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);

        // Knight on a1
        let current = Square::new(4, 3);
        let attacks = knight_attacks(&current, Color::White, &blocked);
        assert_eq!(attacks.len(), 0); // No attacks available
    }

    #[test]
    fn test_knight_attacks_from_middle_is_completely_blocked_but_can_capture() {
        let mut blocked = BoardState::new();
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, WP, NN, BP, NN, NN, NN,
            NN, BP, NN, NN, NN, WP, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, WP, NN, NN, NN, WP, NN, NN,
            NN, NN, WP, NN, WP, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);

        // Knight on a1
        let current = Square::new(4, 3);
        let attacks = knight_attacks(&current, Color::White, &blocked);
        let mut expected = vec![
            Square::new(5, 1),
            Square::new(6, 4),
        ];
        expected.sort();
        assert_eq!(attacks, expected);
    
    }

    #[test]
    fn test_knight_can_escape_from_blocking_enemy_neighbors() {
        let mut blocked = BoardState::new();
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, BP, BP, BP, NN, NN, NN, NN,
            NN, BP, NN, BP, NN, NN, NN, NN,
            NN, BP, BP, BP, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);

        // Knight on a1
        let current = Square::new(4, 2);
        let attacks = knight_attacks(&current, Color::White, &blocked);
        let mut expected = vec![
            Square::new(2, 1), // b3
            Square::new(2, 3), // b5
            Square::new(3, 0), // c2
            Square::new(3, 4), // c6
            Square::new(5, 0), // e2
            Square::new(5, 4), // e6
            Square::new(6, 1), // f3
            Square::new(6, 3), // f5
        ];
        expected.sort();
        assert_eq!(attacks, expected);
    
    }

    #[test]
    fn test_knight_can_escape_from_blocking_friendly_neighbors() {
        let mut blocked = BoardState::new();
        blocked.with_pieces([
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, WP, WP, WP, NN, NN, NN, NN,
            NN, WP, NN, WP, NN, NN, NN, NN,
            NN, WP, WP, WP, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
            NN, NN, NN, NN, NN, NN, NN, NN,
        ]);

        // Knight on a1
        let current = Square::new(4, 2);
        let attacks = knight_attacks(&current, Color::White, &blocked);
        let mut expected = vec![
            Square::new(2, 1), // b3
            Square::new(2, 3), // b5
            Square::new(3, 0), // c2
            Square::new(3, 4), // c6
            Square::new(5, 0), // e2
            Square::new(5, 4), // e6
            Square::new(6, 1), // f3
            Square::new(6, 3), // f5
        ];
        expected.sort();
        assert_eq!(attacks, expected);
    
    }


}