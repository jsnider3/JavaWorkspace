import java.util.ArrayList;

//TODO Stores the state of the game.
public class CheckersBoard{
	String[][] state = new String[8][8];
	short score;
	short recentMove;
	boolean XJustMoved;

	public CheckersBoard(String[][] array, boolean b){
		for(int x=0;x<8;x++){
			state[x]=array[x].clone();
		}
		XJustMoved=b;
		tidyUpState();
		score=score();
	}
	
	public CheckersBoard(CheckersBoard t, short s){
		//TODO
	}
	
	private final void tidyUpState(){
		//TODO
	}
	
	public final CheckersBoard max(ArrayList<CheckersBoard> input){//Provides best move for X.
		//TODO
		return this;
	}
	
	public final CheckersBoard min(ArrayList<CheckersBoard> input){//Provides best move for O.
		//TODO
		return this;
	}
	
	public final ArrayList<CheckersBoard> children(){
		//TODO
		return null;
	}
	
	public final void Move(short s){
		//TODO
	}

	public final boolean hasWon(String playerColor){
		//TODO
		return false;
	}
	
	public final boolean isDraw(){
		//TODO
		return false;
	}
	
	public final boolean isOver(){
		//TODO
		return hasWon("Red")||hasWon("Black");
	}

	public final ArrayList<Short> getEmptySpots(){
		//TODO
		return null;
	}
	
	public final short score(){
		//TODO
		return 0;
	}
	
	public final String toString(){
		//TODO
		return "toString() not yet finished.";
	}

	public final void printVerbose(){
		System.out.println(this);
		//TODO
	}

	public void compMove() {
		//TODO
	}
}
