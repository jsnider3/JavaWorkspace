import java.util.ArrayList;

public class TicTacToeBoard{
	String[][] state = new String[3][3];
	short score;
	short recentMove;
	boolean XJustMoved;//X is max. O is min.//If it's Xs Turn then the next piece put on the board will be an O and all children will have XsTurn=false.
	
	public TicTacToeBoard(String[][] array, boolean b){
		state[0] = array[0].clone();
		state[1] = array[1].clone();
		state[2] = array[2].clone();
		XJustMoved = b;
		tidyUpState();
		score = score();
	}

	public TicTacToeBoard(TicTacToeBoard t, short s){
		state[0] = t.state[0].clone();
		state[1] = t.state[1].clone();
		state[2] = t.state[2].clone();
		XJustMoved = !t.XJustMoved;
		recentMove = s;
		if (XJustMoved) {
			state[s / 3][s % 3] = "X";
		}
		else {
			state[s / 3][s % 3] = "O";
		}
		tidyUpState();
		score = score();
	}

	private final void tidyUpState(){
		for (String[] ar : state) {
			for (String str : ar) {
				if (str == null) {
					str = " ";
				}
				else if (!(str.equals("X")||str.equals("O"))) {
					str = " ";
				}
			}
		}
	}

	public final static TicTacToeBoard max(ArrayList<TicTacToeBoard> input){//Provides best move for X.
		short maxIndex = 0;
		short max = input.get(maxIndex).score;
		for (short x = 0; x < input.size(); x++) {
			if (input.get(x).score == 2) {
				return input.get(x);
			}
			else if (input.get(x).score > max) { 
				max = input.get(x).score;
				maxIndex = x;
			}
		}
		return input.get(maxIndex);
	}

	//TODO Add some randomness to these two.
	public final static TicTacToeBoard min(ArrayList<TicTacToeBoard> input){//Provides best move for O.
		short minIndex = 0;
		short min = input.get(minIndex).score;
		for (short x = 0; x < input.size(); x++) {
			if (input.get(x).score == -2) {
				return input.get(x);
			}
			else if (input.get(x).score<min) {
				min = input.get(x).score;
				minIndex = x;
			}
		}
		return input.get(minIndex);
	}

	public final ArrayList<TicTacToeBoard> children(){
		ArrayList<TicTacToeBoard> children = new ArrayList<TicTacToeBoard>();
		if (hasWon("X") || hasWon("O") || isDraw()) {
			children.add(this);
		}
		else {
			ArrayList<Short> empties = getEmptySpots();
			for (short empty : empties) {
				children.add(new TicTacToeBoard(this, empty));
			}
		}
		return children;
	}

	public final void Move(short s){
		if (s >= 0 && s <= 8){
			state[s / 3][s % 3] = (XJustMoved ? "O" : "X");
			XJustMoved = !XJustMoved;
		}
	}

	public final boolean hasWon(String playerChar){
		//Possible Winning Moves in Tic Tac Toe
		//3 Rows
		//3 Columns
		//2 Diagonals

		for(int x=0;x<3;x++){//Check if there's a winning column.
			if(state[x][0].equals(playerChar)&&state[x][0].equals(state[x][1])&&state[x][1].equals(state[x][2])){
				return true;
			}
		}
		for(int x=0;x<3;x++){//Check if there's a winnning row.
		 	if(state[0][x].equals(playerChar)&&state[0][x].equals(state[1][x])&&state[1][x].equals(state[2][x])){
				return true;
			}
		}
		if(state[1][1].equals(playerChar)){
			if(state[0][0].equals(playerChar)&&state[1][1].equals(state[2][2])){
		 		return true;//Top-left to bottom-right.
		 	}
		 	if(state[2][0].equals(playerChar)&&state[1][1].equals(state[0][2])){
			 	return true;//Top right to bottom left
		 	}
		}
		return false;
	}

	public final boolean isDraw(){
		if(hasWon("X")||hasWon("O")){
			return false;
		}
		return getEmptySpots().isEmpty();
	}

	public final boolean isOver(){
		return isDraw()||hasWon("X")||hasWon("O");
	}

	public final ArrayList<Short> getEmptySpots(){
		ArrayList<Short> empties = new ArrayList<Short>();
		for(short x=0;x<9;x++){
			if(state[x/3][x%3]==""||state[x/3][x%3]==" "){
				empties.add(x);
			}
		}
		return empties;
	}

	public final short score(){
		if (hasWon("X")) {
			return 2;
		}
		else if (hasWon("O")) {
			return -2;
		}
		else if (isDraw()) {
      return 0;
    }
		if(!XJustMoved){//X's turn
			short highest = -1;
			for (TicTacToeBoard child : children()) {
				if (child.score > highest) {
					highest = child.score;
				}
			}
			return highest == 2 ? 1 : highest;
		}
		else {//Not X's turn.
			short lowest = 1;
			for (TicTacToeBoard child : children()) {
			 	if (child.score<lowest) {
			 		lowest = child.score;
				}
			}
			return lowest == -2 ? -1 : lowest;
		}
	}

	public final String toString(){
		String str = "";
		str += state[0][0] + "|" + state[0][1] + "|" + state[0][2] + "\n";
		str += "-----" + "\n";
		str += state[1][0] + "|" + state[1][1] + "|" + state[1][2] + "\n";
		str += "-----" + "\n";
		str += state[2][0] + "|" + state[2][1] + "|" + state[2][2] + "\n";
		return str;
	}

	public final void printVerbose(){
		System.out.println(this);
		System.out.println(XJustMoved? "X just went":"O just went");
		score=score();
		if (score == -2) {
			System.out.println("O has won.");
		}
		else if (score == -1) {
			System.out.println("O will win.");
		}
		else if (score == 0) {
			if (isDraw()) {
				System.out.println("Draw");
			}
			else {
				System.out.println("Eventual Draw");
			}
		}
		else if (score == 1) {
			System.out.println("X will win.");
		}
		else if (score == 2) {
			System.out.println("X has won.");
		}
		else {
			System.out.println("Error!Error!");
		}
		System.out.println("");
	}

	public void compMove() {
		if (XJustMoved) {
			short move = min(children()).recentMove;
			Move(move);
		}
		else {
			short move = max(children()).recentMove;
			Move(move);
		}
	}
}
