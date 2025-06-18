use std::collections::HashMap;

use crate::{
    attacks::{
        bishop_attacks, collect_all_pseudo_legal_moves, is_attacked, king_attacks, king_castling_targets, knight_attacks, pawn_attacks, pawn_moves, queen_attacks, rook_attacks
    }, fen::generate_fen, types::{
        CastlingRights, Color, Move, Piece, PieceType, Promotion, Square
    }
};

use crate::constants::{NN, BR, BN, BB, BQ, BK, BP, WR, WN, WB, WQ, WK, WP};

/// Represents the state of a chessboard, including the pieces, turn, and other game state information.
/// The `BoardState` struct encapsulates the current state of a chess game, including:
/// 
/// - `pieces`: An array of optional `Piece` objects representing the pieces on the board.
/// - `turn`: The current player's turn, represented by the `Color` enum.
/// - `en_passant`: An optional tuple representing the square where en passant is possible.
/// - `castling_rights`: A `CastlingRights` struct indicating the castling rights for both players.
/// - `halfmove_clock`: A counter for the fifty-move rule, which counts halfmoves since the last pawn move or capture.
/// - `fullmove_number`: The current full move number in the game, starting from 1.
#[derive(Debug, Clone)]
pub struct BoardState {
    pub pieces: [Option<Piece>; 64], // 64 squares on the chessboard
    pub turn: Color,
    pub en_passant: Option<Square>, // (row, col) for en passant square
    pub castling_rights: CastlingRights,    // (white, black) castling rights
    pub halfmove_clock: u32,                // Halfmove clock for the fifty-move rule
    pub fullmove_number: u32,               // Fullmove number
    pub repetitions: HashMap<String, u64>,               // HashMap for n-fold repetition detection
}

impl Default for BoardState {
    fn default() -> Self {
        return BoardState {
            pieces: [
                BR, BN, BB, BQ, BK, BB, BN, BR,
                BP, BP, BP, BP, BP, BP, BP, BP,
                NN, NN, NN, NN, NN, NN, NN, NN,
                NN, NN, NN, NN, NN, NN, NN, NN,
                NN, NN, NN, NN, NN, NN, NN, NN,
                NN, NN, NN, NN, NN, NN, NN, NN,
                WP, WP, WP, WP, WP, WP, WP, WP,
                WR, WN, WB, WQ, WK, WB, WN, WR
            ],
            turn: Color::White,
            en_passant: None,
            castling_rights: CastlingRights::default(),
            halfmove_clock: 0,
            fullmove_number: 1,
            repetitions: HashMap::new(),
        }
    }
}

impl PartialEq for BoardState {
    fn eq(&self, other: &Self) -> bool {
        self.pieces == other.pieces &&
        self.turn == other.turn &&
        self.en_passant == other.en_passant &&
        self.castling_rights == other.castling_rights &&
        self.halfmove_clock == other.halfmove_clock &&
        self.fullmove_number == other.fullmove_number
    }
}

impl BoardState {

    /// Return a empty `BoardState` with no pieces but otherwise
    /// default game state.
    pub fn new() -> Self {
        Self {
            pieces: [
                NN, NN, NN, NN, NN, NN, NN, NN,
                NN, NN, NN, NN, NN, NN, NN, NN,
                NN, NN, NN, NN, NN, NN, NN, NN,
                NN, NN, NN, NN, NN, NN, NN, NN,
                NN, NN, NN, NN, NN, NN, NN, NN,
                NN, NN, NN, NN, NN, NN, NN, NN,
                NN, NN, NN, NN, NN, NN, NN, NN,
                NN, NN, NN, NN, NN, NN, NN, NN,
            ],
            turn: Color::White,
            en_passant: None,
            castling_rights: CastlingRights {
                white_k: true,
                white_q: true,
                black_k: true,
                black_q: true,
            },
            halfmove_clock: 0,
            fullmove_number: 1,
            repetitions: HashMap::new(),
        }
    }

    /// Register the current position in the repetitions map.
    pub fn register_position(&mut self) {
        let position = generate_fen(&self, true);
        let count = self.repetitions.entry(position).or_insert(0);
        *count += 1;
    }

    pub fn set_halfmove_clock(&mut self, clock: u32) {
        self.halfmove_clock = clock;
    }

    pub fn set_fullmove_number(&mut self, number: u32) {
        self.fullmove_number = number;
    }

    pub fn set_en_passant(&mut self, square: Option<Square>) {
        self.en_passant = square;
    }

    /// Set the turn to the specified color.
    /// 
    /// 
    pub fn set_turn(&mut self, color: Color) {
        self.turn = color;
    }

    /// Set the castling rights for the board state.
    /// as specified by the `CastlingRights` struct.
    /// 
    /// NOTE: should only be used for testing
    pub fn set_castling_rights(&mut self, rights: CastlingRights) {
        self.castling_rights = rights;
    }

    /// Update the turn and fullmove number.
    /// - If the current turn is Black, move to White and increment the fullmove number.
    /// - If the current turn is White, move to Black.
    /// Example:
    /// ```rust
    /// use brute_move::board::BoardState;
    /// use brute_move::types::{Color, Square};
    /// let mut board = BoardState::default();
    /// board.update_turn();
    /// assert_eq!(board.turn, Color::Black);
    /// board.update_turn();
    /// assert_eq!(board.turn, Color::White);
    /// assert_eq!(board.fullmove_number, 2);
    /// ```
    pub fn update_turn(&mut self) {
        self.turn = self.turn + 1;
        if self.turn == Color::White {
            self.fullmove_number += 1;
        }
    }

    /// Unset the piece at the specified square.
    /// In other words: the corresponding square in
    /// the `pieces` array will be set to `None`.
    /// 
    /// NOTE: This should only be used internally
    /// or for testing/mockup purposes.
    pub fn remove_piece(&mut self, square: Square) {
        self.pieces[self.index_from_square(&square)] = None;
    }

    /// Get the piece at the specified square.
    /// Returns an `Option<Piece>` where `None` indicates
    /// that the square is empty.
    /// 
    /// Example:
    /// ```rust
    /// use brute_move::board::BoardState;
    /// use brute_move::types::{Color, Square, PieceType};
    /// use brute_move::constants::WK;
    /// let board = BoardState::default();
    /// assert_eq!(board.get_piece(&Square::E1), WK.as_ref());
    /// assert_eq!(board.get_piece(&Square::E4), None);
    /// ```
    pub fn set_piece(&mut self, square: Square, piece: Piece) {
        self.pieces[self.index_from_square(&square)] = Some(piece);
    }

    /// In-place method to directly manipulate the pieces on the board.
    /// (Useful for testing or initializing the board state)
    pub fn with_pieces(&mut self, pieces: [Option<Piece>; 64]) -> &mut Self {
        self.pieces = pieces;
        self
    }

    /// In-place method to set the current castling rights.
    /// (Useful for testing or initializing the board state)
    pub fn with_castling_rights(
        &mut self,
        rights: CastlingRights,
    ) -> &mut Self {
        self.castling_rights = rights;
        self
    }

    /// Returns the `Square` that is occupied by the
    /// specified color's king.
    /// 
    /// Example:
    /// ```rust
    /// use brute_move::board::BoardState;
    /// use brute_move::types::{Color, Square};
    /// let board = BoardState::default();
    /// assert_eq!(board.get_king_square(Color::White), Some(Square::E1));
    /// assert_eq!(board.get_king_square(Color::Black), Some(Square::E8));
    /// ```
    pub fn get_king_square(&self, color: Color) -> Option<Square> {
        self.pieces.clone().into_iter().position(|p| {
            if let Some(piece) = p {
                piece.color == color && piece.piece_type == PieceType::King
            } else {
                false
            }
        }).map(|i| Square::from_index(i))
    }

    /// Checks whether a pawn move is valid. This function returns
    /// - move is valid?
    /// - move triggers en passant target square?
    /// - captured piece?
    /// - resulting castling rights (always unmodified in this case)
    /// - is castling move? (always false in this case)
    /// - en passant captured square (if applicable, otherwise None)
    /// 
    fn valid_pawn_move(
        &self, from: Square,
        to: Square,
        en_passant_target: Option<Square>,
        promotion: Option<Promotion>
    ) -> (bool, Option<Square>, Option<&Piece>, CastlingRights, bool, Option<Square>) {

        let pawn = self.get_piece(&from).unwrap(); // this is guaranteed to be a pawn

        // check en passant
        if let Some(en_passant) = en_passant_target {
            // if en_passant is set and we are moving to en passant target, check the conditions
            if en_passant == to  {
                
                // check if the pawn is on the 5th rank
                if (pawn.color == Color::White && from.r() != 4) || (pawn.color == Color::Black && from.r() != 3) {
                    return (false, None, None, self.castling_rights.clone(), false, None);
                }

                // check if the pawn comes from the correct file
                let left_of_en_passant = en_passant.f() as i8 - 1;
                let right_of_en_passant = en_passant.f() as i8 + 1;

                if left_of_en_passant < 0 {
                    // check only the right file
                    if from.f() != 1 {
                        return (false, None, None, self.castling_rights.clone(), false, None);
                    }
                } else if left_of_en_passant > 0 && right_of_en_passant < 8 {
                    // check both files
                    if from.f() != left_of_en_passant as usize && from.f() != right_of_en_passant as usize {
                        return (false, None, None, self.castling_rights.clone(), false, None);
                    }
                } else if right_of_en_passant >= 8 {
                    // check only the left file
                    if from.f() != 6 {
                        return (false, None, None, self.castling_rights.clone(), false, None);
                    }
                }

                let behind_square = match pawn.color {
                    Color::White => Square::new(en_passant.r() - 1, en_passant.f()),
                    Color::Black => Square::new(en_passant.r() + 1, en_passant.f()),
                };

                // check if the square behind en passant square has a piece of the opposite color
                let opponent_piece = self.get_piece(&behind_square);
                if let Some(opponent_piece) = opponent_piece {
                    if (opponent_piece.color == pawn.color) || (opponent_piece.piece_type != PieceType::Pawn) {
                        return (false, None, None, self.castling_rights.clone(), false, None);
                    } else {
                        // valid en passant move
                        return (true, None, Some(opponent_piece), self.castling_rights.clone(), false, Some(behind_square));
                    }
                } else {
                    return (false, None, None, self.castling_rights.clone(), false, None);
                }
                
            }
        }
        
        // then check the promotion target
        if let Some(promotion) = promotion {
            if !promotion.validate() {
                return (false, None, None, self.castling_rights.clone(), false, None);
            }
            // check if the pawn is on the last rank before the last rank
            if (pawn.color == Color::White && from.r() != 6) || (pawn.color == Color::Black && from.r() != 1) {
                return (false, None, None, self.castling_rights.clone(), false, None);
            }
            // check if the pawn moves to the last rank
            if (pawn.color == Color::White && to.r() != 7) || (pawn.color == Color::Black && to.r() != 0) {
                return (false, None, None, self.castling_rights.clone(), false, None);
            }
        } else {
            // if no promotion is set but to is in the last rank, it is invalid
            if (pawn.color == Color::White && to.r() == 7) || (pawn.color == Color::Black && to.r() == 0) {
                return (false, None, None, self.castling_rights.clone(), false, None);
            }
        }
        
        let valid_moves = pawn_moves(&from, pawn.color, &self);
        let attack_targets = pawn_attacks(&from, pawn.color);
        let mut valid_attacks = attack_targets.clone();
        
        for attack in attack_targets {
            // check if the attacked squere has a piece of the opposite color
            let opponent_piece =  self.get_piece(&attack);
            if opponent_piece.is_none() {
                valid_attacks.retain(|x| x != &attack);
                continue; // no piece to attack
            }
            let opponent_piece = opponent_piece.unwrap();
            if opponent_piece.color == pawn.color {
                valid_attacks.retain(|x| x != &attack);
            }
        }

        if valid_attacks.contains(&to) {
            return (true, None, self.get_piece(&to), self.castling_rights.clone(), false, None);
        }

        if valid_moves.contains(&to) {
            // move is double step -> set en passant target square
            if from.r() == 1 && to.r() == 3 && pawn.color == Color::White {
                return (true, Some(Square::new(2, from.f())), None, self.castling_rights.clone(), false, None);
            } else if from.r() == 6 && to.r() == 4 && pawn.color == Color::Black {
                return (true, Some(Square::new(5, from.f())), None, self.castling_rights.clone(), false, None);
            }
            return (true, None, None, self.castling_rights.clone(), false, None);
        }

        (false, None, None, self.castling_rights.clone(), false, None)

    }

    /// Check if a king move is valid. Return the following tuple:
    /// 
    /// - is_valid move?
    /// - set en_passant_target (always false in this case)
    /// - captured_piece
    /// - resulting castling_rights (always remove the moving color's castling rights)
    /// - is_castling move?
    ///
    fn valid_king_move(
        &self, from: Square, to: Square, castling_rights: &CastlingRights
    ) -> (bool, Option<Square>, Option<&Piece>, CastlingRights, bool, Option<Square>) {

        let king = self.get_piece(&from).unwrap(); // this is guaranteed to be a king
        let target_piece = self.get_piece(&to);

        let mut new_castling_rights = castling_rights.clone();
        match king.color {
            Color::White => {  new_castling_rights.white_k = false; new_castling_rights.white_q = false; },
            Color::Black => {  new_castling_rights.black_k = false; new_castling_rights.black_q = false; },
        };

        let attacks = king_attacks(&from, king.color, &self);
        if attacks.contains(&to) {
            return (true, None, target_piece, new_castling_rights, false, None);
        }

        let castling_targets = king_castling_targets(king.color, &self);
        if castling_targets.contains(&to) {
            return (true, None, None, new_castling_rights, true, None);
        }

        return (false, None, None, new_castling_rights, false, None);

    }

    /// Check if a queen move is valid. Return the following tuple:
    ///
    /// - is_valid move?
    /// - set en_passant_target (always false in this case)
    /// - captured_piece
    /// - resulting castling_rights (always unmodified in this case)
    /// - is_castling move? (always false in this case)
    /// - en passant captured square (always None in this case)
    /// 
    fn valid_queen_move(
        &self, from: Square, to: Square, castling_rights: &CastlingRights
    ) -> (bool, Option<Square>, Option<&Piece>, CastlingRights, bool, Option<Square>) {
        
        let queen = self.get_piece(&from).unwrap(); // this is guaranteed to be a queen
        let target_piece = self.get_piece(&to);
        let new_castling_rights = castling_rights.clone();

        let attacks = queen_attacks(&from, queen.color, &self, 8);
        if attacks.contains(&to) {
            return (true, None, target_piece, new_castling_rights, false, None);
        }

        // no valid attacks found
        (false, None, None, new_castling_rights, false, None)

    }

    /// Check if a rook move is valid. Return the following tuple:
    /// 
    /// - is_valid move?
    /// - set en_passant_target (always false in this case)
    /// - captured_piece
    /// - resulting castling_rights (always remove the moving color's castling rights at the correct side)
    /// - is_castling move? (always false in this case - can only be triggered by a king move)
    /// - en passant captured square (always None in this case)
    fn valid_rook_move(
        &self, from: Square, to: Square, castling_rights: &CastlingRights
    ) -> (bool, Option<Square>, Option<&Piece>, CastlingRights, bool, Option<Square>) {

        let rook = self.get_piece(&from).unwrap(); // this is guaranteed to be a rook
        let target_piece = self.get_piece(&to);

        let mut new_castling_rights = castling_rights.clone();
        let (kingside_move, queenside_move) = {
                if from.f() == 0 {
                    (false, true) // queenside rook
                } else if from.f() == 7 {
                    (true, false) // kingside rook
                } else {
                    (false, false) // not a rook in standard position
                }
        };
        match rook.color {
            Color::Black => {
                if kingside_move {
                    new_castling_rights.black_k = false;
                } else if queenside_move {
                    new_castling_rights.black_q = false;
                }
                // else: unmodified castling rights
            },
            Color::White => {
                if kingside_move {
                    new_castling_rights.white_k = false;
                } else if queenside_move {
                    new_castling_rights.white_q = false;
                }
                // else: unmodified castling rights
            },  
        };

        let attacks = rook_attacks(&from, rook.color, &self, 8);
        if attacks.contains(&to) {
            return (true, None, target_piece, new_castling_rights, false, None);
        }

        return (false, None, None, new_castling_rights, false, None);

    }

    /// Check if a bishop move is valid. Return the following tuple:
    /// 
    /// - is_valid move?
    /// - set en_passant_target (always false in this case)
    /// - captured_piece
    /// - resulting castling_rights (always unmodified in this case)
    /// - is_castling move? (always false in this case)
    /// - en passant captured square (always None in this case)
    /// 
    fn valid_bishop_move(
        &self, from: Square, to: Square, castling_rights: &CastlingRights
    ) -> (bool, Option<Square>, Option<&Piece>, CastlingRights, bool, Option<Square>) {
        
        let bishop = self.get_piece(&from).unwrap(); // this is guaranteed to be a bishop
        let target_piece = self.get_piece(&to);
        let new_castling_rights = castling_rights.clone();

        let attacks = bishop_attacks(&from, bishop.color, &self, 8);
        if attacks.contains(&to) {
            return (true, None, target_piece, new_castling_rights, false, None);
        }

        return (false, None, None, new_castling_rights, false, None);
    }

    /// Check if a knight move is valid. Return the following tuple:
    /// 
    /// - is_valid move?
    /// - set en_passant_target (always false in this case)
    /// - captured_piece
    /// - resulting castling_rights (always unmodified in this case)
    /// - is_castling move? (always false in this case)
    /// 
    fn valid_knight_move(
        &self, from: Square, to: Square, castling_rights: &CastlingRights
    ) -> (bool, Option<Square>, Option<&Piece>, CastlingRights, bool, Option<Square>) {
        
        let knight = self.get_piece(&from).unwrap(); // this is guaranteed to be a knight
        let target_piece = self.get_piece(&to);
        let new_castling_rights = castling_rights.clone();

        // Knight moves are not implemented yet
        let attacks = knight_attacks(&from, knight.color, &self);
        if attacks.contains(&to) {
            return (true, None, target_piece, new_castling_rights, false, None);
        }

        return (false, None, None, new_castling_rights, false, None);

    }

    pub fn is_check_mate(&self) -> bool {
        // 1. check if the king square is attacked
        let kings_square = self.get_king_square(self.turn);
        if kings_square.is_none() {
            // no king on the board, cannot be checkmate
            return false;
        }
        let attacked = is_attacked(kings_square.unwrap(), self.turn + 1, &self);
        if !attacked {
            // king is not in check, cannot be checkmate
            return false;
        }
        
        // 2. collect all pseudo legal moves for the current turn
        let mut pseudo_legal_moves = collect_all_pseudo_legal_moves(&self, self.turn);

        // 3. for each move, check if it is valid
        pseudo_legal_moves.retain(|mv| {
            let (valid, _, _, _, _, _) = self.valid_move(mv.clone());
            valid
        });

        if pseudo_legal_moves.is_empty() {
            // no legal moves available, checkmate
            return true;
        }

        return false;

    }

    pub fn is_stale_mate(&self) -> bool {
        // 1. check if the king square is _not_ attacked
        let kings_square = self.get_king_square(self.turn);
        if kings_square.is_none() {
            // no king on the board, cannot be checkmate
            return false;
        }
        let attacked = is_attacked(kings_square.unwrap(), self.turn + 1, &self);
        if attacked {
            // king is in check, cannot be stalemate
            return false;
        }

        // 2. collect all pseudo legal moves for the current turn
        let mut pseudo_legal_moves = collect_all_pseudo_legal_moves(&self, self.turn);

        // 3. for each move, check if it is valid
        pseudo_legal_moves.retain(|mv| {
            let (valid, _, _, _, _, _) = self.valid_move(mv.clone());
            valid
        });

        if pseudo_legal_moves.is_empty() {
            // no legal moves available, stalemate
            return true;
        }

        return false;

    }
    
    /// Check if a move is valid for a given piece.
    /// 
    /// This function checks if a move from one square to another is valid for the piece at the 'from' square.
    /// It returns a tuple containing:
    /// 
    /// - `bool`: Whether the move is valid.
    /// - `Option<Square>`: The en passant target square if applicable, otherwise `None`.
    /// - `Option<&Piece>`: The captured piece if applicable, otherwise `None`.
    /// - `CastlingRights`: The castling rights after the move.
    /// - `bool`: Whether the move is a castling move.
    /// - `Option<Square>`: The en passant capture square if applicable, otherwise `None`.
    pub fn valid_move(
        &self,
        mv: Move
    ) -> (bool, Option<Square>, Option<&Piece>, CastlingRights, bool, Option<Square>) {

        let from = mv.clone().from;
        let to = mv.clone().to;
        let promotion = mv.clone().promotion;
        
        // TODO: Implement the following rules:
        // -> 3-fold repetition but only if claimed
        // -> 50 rule - but only if claimed
        // -> 5-fold repetition

        // from and to are equal
        if from == to {
            return (false, None, None, self.castling_rights.clone(), false, None);
        }

        // Check if the from and to squares are within board limits
        if !from.validate() || !to.validate() {
            return (false, None, None, self.castling_rights.clone(), false, None);
        }

        // Retrieve the piece at the 'from' square
        let piece = self.get_piece(&from);

        // Check if there is a piece at the 'from' square
        if piece.is_none() {
            return (false, None, None, self.castling_rights.clone(), false, None);
        }

        let piece_moved = piece.unwrap();
        // Check if the piece belongs to the current turn
        if piece_moved.color != self.turn {
            return (false, None, None, self.castling_rights.clone(), false, None);
        }

        // now - for the piece type being moved, calculate the valid moves
        // using attacks and moves functions. all of these function return
        // a tuple with a set of triggers that are able to modify the game state.
        let triggers = match piece_moved.piece_type {
            PieceType::Pawn => self.valid_pawn_move(from, to, self.en_passant.clone(), promotion),
            PieceType::King => self.valid_king_move(from, to, &self.castling_rights),
            PieceType::Queen => self.valid_queen_move(from, to, &self.castling_rights),
            PieceType::Rook => self.valid_rook_move(from, to, &self.castling_rights),
            PieceType::Bishop => self.valid_bishop_move(from, to, &self.castling_rights),
            PieceType::Knight => self.valid_knight_move(from, to, &self.castling_rights),
        };

        // king should not be able to be captured - invalid game state
        // because at the beginning of the turn the enemy king cannot
        // be in check.
        if let Some(piece) = triggers.2 {
            if piece.piece_type == PieceType::King {
                panic!("Invalid game state: King cannot be captured during the turn.");
            }
        }

        // if at this point the move is valid, let's check if the king
        // is in check after the move. If it is, we cannot execute the move.
        if triggers.clone().0 {
            let new_board_state = self.simulate_move(
                mv.clone(),
                triggers.clone().4,
                triggers.clone().5,
                &triggers.clone().3
            );
            let kings_square = new_board_state.get_king_square(self.turn);
            let attacked = is_attacked(kings_square.unwrap(), self.turn + 1, &new_board_state);
            if attacked {
                return (false, None, None, self.castling_rights.clone(), triggers.clone().4, triggers.clone().5);
            }
        }
        
        return triggers;
    
    }

    /// Simulate a move
    /// - no validation of the move
    /// - only updating the board pieces (not the remaining state)
    /// This is basically a stripped down version of `execute_move` to be able to detect self-checks
    /// on an updated board state.
    pub fn simulate_move(
        &self,
        mv: Move,
        is_castling: bool,
        en_passant_captured: Option<Square>,
        castling_rights: &CastlingRights
    ) -> BoardState {
        let from = mv.clone().from;
        let to = mv.clone().to;

        // snapshot of the current board state
        let old_board_state = self.clone();
        let mut new_board_state = old_board_state.clone();

        if let Some(promotion) = mv.promotion {
            new_board_state.set_piece(mv.to.clone(), Piece {
                piece_type: promotion.0,
                color: self.turn,
            });
            new_board_state.remove_piece(from.clone());
            return new_board_state;
        }

        if is_castling {
            let (king_from, king_to) = if from.clone().f() < to.f() {
                // kingside castling
                (from.clone(), Square::new(from.r(), 6)) // move king to g1/g8
            } else {
                // queenside castling
                (from.clone(), Square::new(from.r(), 2)) // move king to c1/c8
            };
            
            let (rook_from, rook_to) = if from.f() < to.f() {
                // kingside castling
                (Square::new(from.r(), 7), Square::new(from.r(), 5)) // move rook to f1/f8
            } else {
                // queenside castling
                (Square::new(from.r(), 0), Square::new(from.r(), 3)) // move rook to d1/d8
            };

            // set king
            new_board_state.set_piece(king_to, Piece { piece_type: PieceType::King, color: self.turn });
            new_board_state.remove_piece(king_from.clone());

            // set rook
            new_board_state.set_piece(rook_to, Piece { piece_type: PieceType::Rook, color: self.turn});
            new_board_state.remove_piece(rook_from.clone());

            return new_board_state;
        }

        // handle en passant capture
        if let Some(en_passant_capture_square) = en_passant_captured {
            // remove the captured pawn from the board
            
            new_board_state.remove_piece(en_passant_capture_square.clone());
            new_board_state.remove_piece(from.clone());
            new_board_state.set_piece(to.clone(), Piece {
                piece_type: PieceType::Pawn,
                color: self.turn,
            });

            new_board_state.update_turn();
            new_board_state.en_passant = None; // en passant does not set en passant target
            new_board_state.castling_rights = castling_rights.clone(); // castling rights are unmodified
            new_board_state.halfmove_clock = 0; // was a pawn move, reset halfmove clock

            return new_board_state;
        }

        // handle normal piece moves including captures
        let piece = self.get_piece(&from).unwrap().clone();
        new_board_state.set_piece(to.clone(), piece.clone());
        new_board_state.remove_piece(from.clone());

        new_board_state

    }

    pub fn execute_move(
        &self,
        mv: Move
    ) -> Result<BoardState, BoardState> {

        let from = mv.clone().from;
        let to = mv.clone().to;

        // snapshot of the current board state
        let old_board_state = self.clone();
        let mut new_board_state = old_board_state.clone();
        
        // check if the move is valid
        let (
            valid,
            en_passant_target,
            captured_piece,
            castling_rights,
            is_castling,
            en_passant_captured
        ) = old_board_state.valid_move(mv.clone());

        if !valid {
            return Err(old_board_state);
        }

        // we have a promotion and the promotions was deemed
        // valid in conjunction with the to, from arguments 
        // (by the `valid_move` function). Only thing left
        // is to remove the pawn and replace it with the
        // promoted piece.
        if let Some(promotion) = mv.promotion {
            new_board_state.set_piece(mv.to.clone(), Piece {
                piece_type: promotion.0,
                color: self.turn,
            });
            new_board_state.remove_piece(from.clone());

            new_board_state.update_turn();
            new_board_state.en_passant = None; // promotion does not set en passant target
            new_board_state.castling_rights = castling_rights.clone(); // castling rights are unmodified
            new_board_state.halfmove_clock = 0; // was a pawn move, reset halfmove clock
            new_board_state.register_position();

            return Ok(new_board_state);
        }

        // this was a castling move. move the king and rook
        // accordingly and update the castling rights.
        if is_castling {
            let (king_from, king_to) = if from.clone().f() < to.f() {
                // kingside castling
                (from.clone(), Square::new(from.r(), 6)) // move king to g1/g8
            } else {
                // queenside castling
                (from.clone(), Square::new(from.r(), 2)) // move king to c1/c8
            };
            
            let (rook_from, rook_to) = if from.f() < to.f() {
                // kingside castling
                (Square::new(from.r(), 7), Square::new(from.r(), 5)) // move rook to f1/f8
            } else {
                // queenside castling
                (Square::new(from.r(), 0), Square::new(from.r(), 3)) // move rook to d1/d8
            };

            // set king
            new_board_state.set_piece(king_to, Piece { piece_type: PieceType::King, color: self.turn });
            new_board_state.remove_piece(king_from.clone());

            // set rook
            new_board_state.set_piece(rook_to, Piece { piece_type: PieceType::Rook, color: self.turn});
            new_board_state.remove_piece(rook_from.clone());

            new_board_state.update_turn();
            new_board_state.en_passant = None; // castling does not set en passant target
            new_board_state.castling_rights = castling_rights.clone();
            new_board_state.halfmove_clock += 1; // castling is a valid move, increment halfmove clock
            new_board_state.register_position();

            return Ok(new_board_state);
        }

        // handle en passant capture
        if let Some(en_passant_capture_square) = en_passant_captured {
            // remove the captured pawn from the board
            
            new_board_state.remove_piece(en_passant_capture_square.clone());
            new_board_state.remove_piece(from.clone());
            new_board_state.set_piece(to.clone(), Piece {
                piece_type: PieceType::Pawn,
                color: self.turn,
            });

            new_board_state.update_turn();
            new_board_state.en_passant = None; // en passant does not set en passant target
            new_board_state.castling_rights = castling_rights.clone(); // castling rights are unmodified
            new_board_state.halfmove_clock = 0; // was a pawn move, reset halfmove clock
            new_board_state.register_position();

            return Ok(new_board_state);
        }

        // handle normal piece moves including captures
        let piece = self.get_piece(&from).unwrap().clone();
        new_board_state.set_piece(to.clone(), piece.clone());
        new_board_state.remove_piece(from.clone());

        
        new_board_state.update_turn();
        new_board_state.en_passant = en_passant_target;
        new_board_state.castling_rights = castling_rights;

        // increment halfmove clock
        if piece.piece_type != PieceType::Pawn && captured_piece.is_none() {
            new_board_state.halfmove_clock += 1;
        } else {
            new_board_state.halfmove_clock = 0; // reset on pawn move or capture
        }

        new_board_state.register_position();

        return Ok(new_board_state);
    }

    pub fn get_piece(&self, square: &Square) -> Option<&Piece> {
        if square.validate() {
            self.pieces[self.index_from_square(square)].as_ref()
        } else {
            None
        }
    }

    fn index_from_square(&self, square: &Square) -> usize {
        (7 - square.r()) * 8 + square.f()
    }

}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{fen::{self, parse_fen}, types::Move};

    #[test]
    fn test_default_board_state() {
        let expected_board = parse_fen(fen::STARTING_POSITION).unwrap();
        let board = BoardState::default();
        assert_eq!(expected_board, board);
    }

    // GENERAL SANITY TESTS

    #[test]
    fn test_move_not_my_turn() {
        let board = BoardState::default();
        let mv = Move {
            from: Square::E7,
            to: Square::E6,
            promotion: None,
        };

        let (
            valid,
            _en_passant_target,
            _captured_piece,
            _castling_rights,
            _is_castling,
            _en_passant_captured
        ) = board.valid_move(mv.clone());

        assert!(!valid);

        let old_board = board.execute_move(mv.clone()).unwrap_err();
        assert_eq!(old_board, board);
    
    }

    #[test]
    fn test_move_piece_is_blocked() {
        let board = BoardState::default();
        let mv = Move {
            from: Square::D1,
            to: Square::D4,
            promotion: None,
        };

        let (
            valid,
            _en_passant_target,
            _captured_piece,
            _castling_rights,
            _is_castling,
            _en_passant_captured
        ) = board.valid_move(mv.clone());

        assert!(!valid);

        let old_board = board.execute_move(mv.clone()).unwrap_err();
        assert_eq!(old_board, board);
    
    }

    #[test]
    fn test_move_clears_en_passant() {
        let board = parse_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KkQq e3 0 1").unwrap();
        let mv = Move {
            from: Square::A7,
            to: Square::A6,
            promotion: None,
        };
        let expected_board = parse_fen("rnbqkbnr/1ppppppp/p7/8/4P3/8/PPPP1PPP/RNBQKBNR w KkQq - 0 2").unwrap();

        let (
            valid,
            en_passant_target,
            captured_piece,
            castling_rights,
            is_castling,
            en_passant_captured
        ) = board.valid_move(mv.clone());

        assert!(valid);
        assert_eq!(en_passant_target, None);
        assert!(captured_piece.is_none());
        assert_eq!(castling_rights, CastlingRights::default());
        assert!(!is_castling);
        assert!(en_passant_captured.is_none());

        // Execute the move
        let new_board = board.execute_move(mv.clone()).unwrap();
        assert_eq!(new_board, expected_board);

    }

    #[test]
    fn test_double_move_overrides_en_passant() {
        let board = parse_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KkQq e3 0 1").unwrap();
        let mv = Move {
            from: Square::A7,
            to: Square::A5,
            promotion: None,
        };
        let expected_board = parse_fen("rnbqkbnr/1ppppppp/8/p7/4P3/8/PPPP1PPP/RNBQKBNR w KkQq a6 0 2").unwrap();

        let (
            valid,
            en_passant_target,
            captured_piece,
            castling_rights,
            is_castling,
            en_passant_captured
        ) = board.valid_move(mv.clone());

        assert!(valid);
        assert_eq!(en_passant_target, Some(Square::A6));
        assert!(captured_piece.is_none());
        assert_eq!(castling_rights, CastlingRights::default());
        assert!(!is_castling);
        assert!(en_passant_captured.is_none());

        // Execute the move
        let new_board = board.execute_move(mv.clone()).unwrap();
        assert_eq!(new_board, expected_board);

    }

    #[test]
    fn test_not_using_en_passant_clears_it() {
        let board = parse_fen("rnbqkbnr/1ppppppp/8/p7/4P3/8/PPPP1PPP/RNBQKBNR w KkQq a6 0 2").unwrap();
        let mv = Move {
            from: Square::D1,
            to: Square::G4,
            promotion: None,
        };
        let expected_board = parse_fen("rnbqkbnr/1ppppppp/8/p7/4P1Q1/8/PPPP1PPP/RNB1KBNR b KQkq - 1 2").unwrap();

        let (
            valid,
            en_passant_target,
            captured_piece,
            castling_rights,
            is_castling,
            en_passant_captured
        ) = board.valid_move(mv.clone());

        assert!(valid);
        assert_eq!(en_passant_target, None);
        assert!(captured_piece.is_none());
        assert_eq!(castling_rights, CastlingRights::default());
        assert!(!is_castling);
        assert!(en_passant_captured.is_none());

        // Execute the move
        let new_board = board.execute_move(mv.clone()).unwrap();
        assert_eq!(new_board, expected_board);

    }

    #[test]
    fn capture_move_resets_halfclock_timer() {
        let board = parse_fen("rnbqk1nr/pppp1ppp/8/2b1p3/2B1P3/8/PPPP1PPP/RNBQK1NR w KQkq - 2 3").unwrap();
        let mv = Move {
            from: Square::C4,
            to: Square::F7,
            promotion: None,
        };
        let expected_board = parse_fen("rnbqk1nr/pppp1Bpp/8/2b1p3/4P3/8/PPPP1PPP/RNBQK1NR b KQkq - 0 3").unwrap();

        let (
            valid,
            en_passant_target,
            captured_piece,
            castling_rights,
            is_castling,
            en_passant_captured
        ) = board.valid_move(mv.clone());

        assert!(valid);
        assert_eq!(en_passant_target, None);
        assert!(captured_piece.is_some());
        assert_eq!(castling_rights, CastlingRights::default());
        assert!(!is_castling);
        assert!(en_passant_captured.is_none());

        // Execute the move
        let new_board = board.execute_move(mv.clone()).unwrap();
        assert_eq!(new_board, expected_board);

    }

    #[test]
    fn test_self_check_detection() {

        // king ist in check
        let board = parse_fen("rnbqk1nr/pppp2pp/6B1/2b1p3/4P3/8/PPPP1PPP/RNBQK1NR b KQkq - 0 1").unwrap();
        
        // this pawn move lets the black king remain in check
        let mv = Move { from: Square::A7, to: Square::A6, promotion: None };
        let (
            valid,
            _en_passant_target,
            _captured_piece,
            _castling_rights,
            _is_castling,
            _en_passant_captured
        ) = board.valid_move(mv.clone());

        assert!(!valid);
        let old_board = board.execute_move(mv.clone()).unwrap_err();
        assert_eq!(old_board, board);

        // king moves itself out of check - works
        let mv = Move { from: Square::E8, to: Square::E7, promotion: None };
        let (
            valid,
            en_passant_target,
            captured_piece,
            castling_rights,
            is_castling,
            en_passant_captured
        ) = board.valid_move(mv.clone());
        
        assert!(valid);
        assert_eq!(en_passant_target, None);
        assert!(captured_piece.is_none());
        assert_eq!(castling_rights, CastlingRights{ white_k: true, white_q: true, black_k: false, black_q: false });
        assert!(!is_castling);
        assert!(en_passant_captured.is_none());
        // Execute the move
        let new_board = board.execute_move(mv.clone()).unwrap();
        let expected_board = parse_fen("rnbq2nr/ppppk1pp/6B1/2b1p3/4P3/8/PPPP1PPP/RNBQK1NR w KQ - 1 2").unwrap();
        assert_eq!(new_board, expected_board);

        // black pawn captures white bishop and removes the check
        let mv = Move { from: Square::H7, to: Square::G6, promotion: None};
        let (
            valid,
            en_passant_target,
            captured_piece,
            castling_rights,
            is_castling,
            en_passant_captured
        ) = board.valid_move(mv.clone());
        assert!(valid);
        assert_eq!(en_passant_target, None);
        assert!(captured_piece.is_some());
        assert_eq!(castling_rights, CastlingRights::default());
        assert!(!is_castling);
        assert!(en_passant_captured.is_none());
        // Execute the move
        let new_board = board.execute_move(mv.clone()).unwrap();
        let expected_board = parse_fen("rnbqk1nr/pppp2p1/6p1/2b1p3/4P3/8/PPPP1PPP/RNBQK1NR w KQkq - 0 2").unwrap();
        assert_eq!(new_board, expected_board);

        // king is not in check - but moving the black pawn
        // to g5 would put the black king in check
        let board = parse_fen("rnbqk1nr/pppp3p/6p1/2b1p2B/4P3/8/PPPP1PPP/RNBQK1NR b KQkq - 0 1").unwrap();
        let mv = Move { from: Square::G6, to: Square::G5, promotion: None };
        let (
            valid,
            _en_passant_target,
            _captured_piece,
            _castling_rights,
            _is_castling,
            _en_passant_captured
        ) = board.valid_move(mv.clone());
        assert!(!valid);
        let old_board = board.execute_move(mv.clone()).unwrap_err();
        assert_eq!(old_board, board);

    }

    #[test]
    fn test_is_check_mate() {
        let board = parse_fen("K7/8/8/8/8/1r6/r7/8 w - - 0 1").unwrap();
        assert!(board.is_check_mate());

        let board = parse_fen("K7/8/8/8/8/2r5/r7/8 w - - 0 1").unwrap();
        assert!(!board.is_check_mate());

        let board = parse_fen("K7/1r6/8/8/8/8/r7/8 w - - 0 1").unwrap();
        assert!(!board.is_check_mate());

        let board = parse_fen("K7/8/8/5R2/8/1r6/r7/8 w - - 0 1").unwrap();
        assert!(!board.is_check_mate());

        let board = parse_fen("K7/2q5/3P4/8/8/8/r7/8 w - - 0 1").unwrap();
        assert!(board.is_check_mate());
    
        let board = parse_fen("K7/8/8/4q3/8/8/r5b1/8 w - - 0 1").unwrap();
        assert!(board.is_check_mate());

        // interesting case:
        // black king is seemingly in check mate,
        // but white can en passant capture
        // the black pawn and remove the check.
        let board = parse_fen("4K3/7r/8/b1pP4/q7/8/8/5r2 w - c6 0 1").unwrap();
        assert!(!board.is_check_mate());

        // this setup only depends on the
        // en passant right
        let board = parse_fen("4K3/7r/8/b1pP4/q7/8/8/5r2 w - - 0 1").unwrap();
        assert!(board.is_check_mate());

    }

    #[test]
    fn test_is_stale_mate() {

        let board = BoardState::default();
        assert!(!board.is_stale_mate());

        let board = parse_fen("4K3/q7/8/8/8/8/8/3r1r2 w - - 0 1").unwrap();
        assert!(board.is_stale_mate());

        let board = parse_fen("4K3/q7/1P6/8/8/8/8/3r1r2 w - - 0 1").unwrap();
        assert!(!board.is_stale_mate());

        // a single knight move available
        let board = parse_fen("4k3/8/8/6p1/6P1/4p1p1/4P1PN/5BRK w - - 0 1").unwrap();
        assert!(!board.is_stale_mate());

        // no moves available, but king is not in check
        let board = parse_fen("4k3/8/8/4p1p1/4P1P1/4pBp1/4P1PN/5BRK w - - 0 1").unwrap();
        assert!(board.is_stale_mate());

        let board = parse_fen("K7/8/8/8/8/8/5Q2/7k b - - 0 1").unwrap();
        assert!(board.is_stale_mate());

        let board = parse_fen("k7/7R/4p3/4P3/6p1/b5N1/8/RQ6 b - - 0 1").unwrap();
        assert!(board.is_stale_mate());

    }

    #[test]
    fn test_register_position() {

        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -";
        let fen2 = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3".to_string();
        let fen3 = "rnbqkbnr/1ppppppp/p7/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3".to_string();
        let mut board = BoardState::default();

        assert_eq!(board.repetitions.get(fen), None);
        
        board.register_position();
        assert_eq!(board.repetitions.get(fen).unwrap(), &1);

        // register the same position again
        board.register_position();
        assert_eq!(board.repetitions.get(fen).unwrap(), &2);

        // execute a move to change the position
        let mv = Move {
            from: Square::E2,
            to: Square::E4,
            promotion: None,
        };
        let new_board = board.execute_move(mv).unwrap();

        assert_eq!(new_board.repetitions.get(fen).unwrap(), &2);
        assert_eq!(new_board.repetitions.get(&fen2).unwrap(), &1);
        assert_eq!(new_board.repetitions.get(&fen3), None);

    }

    // TEST PAWN MOVES

    #[test]
    fn test_pawn_single_move_from_start() {
        let board = BoardState::default();
        let mv = Move {
            from: Square::E2,
            to: Square::E3,
            promotion: None,
        };
        let expected_board = parse_fen("rnbqkbnr/pppppppp/8/8/8/4P3/PPPP1PPP/RNBQKBNR b KkQq - 0 1").unwrap();

        let (
            valid,
            en_passant_target,
            captured_piece,
            castling_rights,
            is_castling,
            en_passant_captured
        ) = board.valid_move(mv.clone());

        assert!(valid);
        assert_eq!(en_passant_target, None);
        assert!(captured_piece.is_none());
        assert_eq!(castling_rights, CastlingRights::default());
        assert!(!is_castling);
        assert!(en_passant_captured.is_none());

        // Execute the move
        let new_board = board.execute_move(mv.clone()).unwrap();
        assert_eq!(new_board, expected_board);

    }

    #[test]
    fn test_pawn_double_move_from_start() {
        let board = BoardState::default();
        let mv = Move {
            from: Square::E2,
            to: Square::E4,
            promotion: None,
        };
        let expected_board = parse_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KkQq e3 0 1").unwrap();

        let (
            valid,
            en_passant_target,
            captured_piece,
            castling_rights,
            is_castling,
            en_passant_captured
        ) = board.valid_move(mv.clone());

        assert!(valid);
        assert_eq!(en_passant_target, Some(Square::E3));
        assert!(captured_piece.is_none());
        assert_eq!(castling_rights, CastlingRights::default());
        assert!(!is_castling);
        assert!(en_passant_captured.is_none());

        // Execute the move
        let new_board = board.execute_move(mv.clone()).unwrap();
        assert_eq!(new_board, expected_board);

    }

}