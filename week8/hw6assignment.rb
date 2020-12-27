# University of Washington, Programming Languages, Homework 6, hw6assignment.rb

class MyPiece < Piece
  All_My_Pieces = All_Pieces + [
    rotations([[0, 0], [-1, 0], [1, 0], [0, -1], [-1,-1]]),
    [
      [[0, 0], [-1, 0], [1, 0], [2, 0], [-2, 0]],
      [[0, 0], [0, -1], [0, 1], [0, 2], [0, -2]]
    ],
    rotations([[0, 0], [1, 0], [0, 1]])
 ]

  Cheat_Piece = [[[0, 0]]]

  def self.next_piece(board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.next_cheat_piece(board)
    MyPiece.new(Cheat_Piece, board)
  end
end

class MyBoard < Board
  def initialize(game)
    super
    @current_block = MyPiece.next_piece(self)
    @cheat = false
  end

  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def next_piece
    if @cheat
      @current_block = MyPiece.next_cheat_piece(self)
      @cheat = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def store_current
    displacement = @current_block.position
    @current_block.current_rotation.each_with_index do |location, i| 
      x = location[0] + displacement[0]
      y = location[1] + displacement[1]
      @grid[y][x] = @current_pos[i]
    end
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def activate_cheat
    if !@cheat && score >= 100
      @score -= 100
      @cheat = true
    end
  end
end

class MyTetris < Tetris
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc { @board.rotate_180 })
    @root.bind('c', proc { @board.activate_cheat })
  end
end
