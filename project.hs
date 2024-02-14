type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location | B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])

setBoard :: Board
setBoard = (White, whitePieces, blackPieces)
  where
    whitePieces = [
      R ('h', 1), N ('g', 1), B ('f', 1), K ('e', 1),
      Q ('d', 1), B ('c', 1), N ('b', 1), R ('a', 1),
      P ('h', 2), P ('g', 2), P ('f', 2), P ('e', 2),
      P ('d', 2), P ('c', 2), P ('b', 2), P ('a', 2)
      ]
    blackPieces = [
      R ('h', 8), N ('g', 8), B ('f', 8), K ('e', 8),
      Q ('d', 8), B ('c', 8), N ('b', 8), R ('a', 8),
      P ('h', 7), P ('g', 7), P ('f', 7), P ('e', 7),
      P ('d', 7), P ('c', 7), P ('b', 7), P ('a', 7)
      ]

pieceSymbol :: Piece -> String
pieceSymbol (P _) = "P"
pieceSymbol (N _) = "N"
pieceSymbol (K _) = "K"
pieceSymbol (Q _) = "Q"
pieceSymbol (R _) = "R"
pieceSymbol (B _) = "B"

-- Helper function to get the color suffix for a piece
pieceColorSuffix :: Piece -> String
pieceColorSuffix (P loc) = if snd loc == 2 then "W" else "B"
pieceColorSuffix piece = if colorOfPiece piece == White then "W" else "B"

-- Helper function to get the piece at a given location on the board
lookupPiece :: Location -> [Piece] -> Maybe Piece
lookupPiece loc pieces = case filter (\piece -> pieceLocation piece == loc) pieces of
                           [] -> Nothing
                           (piece:_) -> Just piece
  where pieceLocation (P loc) = loc
        pieceLocation (N loc) = loc
	pieceLocation (K loc) = loc
        pieceLocation (Q loc) = loc
        pieceLocation (R loc) = loc
        pieceLocation (B loc) = loc

-- Helper function to get the color of a piece
colorOfPiece :: Piece -> Player
colorOfPiece (P loc) = if snd loc == 2 then White else Black
colorOfPiece piece = case playerPieces piece of
                       (_, White, _) -> White
                       (_, Black, _) -> Black
  where playerPieces (P loc) = (loc, White, [])
        playerPieces (N loc) = (loc, White, [])
        playerPieces (K loc) = (loc, White, [])
        playerPieces (Q loc) = (loc, White, [])
        playerPieces (R loc) = (loc, White, [])
        playerPieces (B loc) = (loc, White, [])

joinWithNewline :: [String] -> String
joinWithNewline = unlines

-- The main function to visualize the board
visualizeBoard :: Board -> String
visualizeBoard (currentPlayer, whitePieces, blackPieces) =
  let rows = [8, 7, 6, 5, 4, 3, 2, 1]
      cols = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']
      pieceAt loc = case lookupPiece loc (whitePieces ++ blackPieces) of
                      Just piece -> piece
                      Nothing -> error "Invalid board state: piece not found"
      cellContent loc =
        case lookupPiece loc whitePieces of
          Just piece -> pieceSymbol piece ++ "W"
          Nothing -> case lookupPiece loc blackPieces of
                       Just piece -> pieceSymbol piece ++ "B"
                       Nothing -> "  "
      rowString row =
        let cells = map (\col -> "| " ++ cellContent (col, row) ++ " ") cols
        in show row ++ " " ++ concat cells ++ "|"
      separator = replicate (length cols * 4 + 1) '-'
      boardLines = map rowString rows
  in joinWithNewline (separator : boardLines ++ [separator, "Turn: " ++ show currentPlayer])


isLegal :: Piece -> Board -> Location -> Bool
isLegal piece board destination = case (piece, board) of
  (_, (_, [], _)) -> False  -- Empty board, no legal moves
  (P _, (_, _, blackPieces)) -> not (elem destination (map getLocation blackPieces))  -- Black pawn cannot capture own pieces
  (P _, (_, whitePieces, _)) -> not (elem destination (map getLocation whitePieces))  -- White pawn cannot capture own pieces
  (_, (_, blackPieces, _)) -> elem destination (map getLocation blackPieces)  -- Check if destination is occupied by a black piece
  (_, (_, _, whitePieces)) -> elem destination (map getLocation whitePieces)  -- Check if destination is occupied by a white piece
  where
    getLocation (P loc) = loc
    getLocation (N loc) = loc
    getLocation (K loc) = loc
    getLocation (Q loc) = loc
    getLocation (R loc) = loc
    getLocation (B loc) = loc