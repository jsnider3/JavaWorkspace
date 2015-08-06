import java.util.Scanner;
//This is a min-max implementation.
public class TicTacToeGame {
	public static final String[][] BLANK_ARRAY={{" "," "," "},{" "," "," "},{" "," "," "}};
	public static final TicTacToeBoard DEFAULT_BOARD = new TicTacToeBoard(BLANK_ARRAY, false);
	public static Scanner sc = new Scanner(System.in);
	/* 5/1/2012 Bug Report:
	 * The computer doesn't make perfect moves in CLI mode.
	 * The computer doesn't make perfect moves in GUI mode.
	 * The computer sometimes reports results incorrectly .
	 */

	/* 5/18/2012 Bug Report:
	 * The computer doesn't make perfect moves in GUI mode.
	 */

	/*7:48 pm 5/18/2012 All Bugs removed
	 * Suggestions for improvements:
	 * Multiple draw/loss messages.
	 * Keep track of losses in row.
	 */

	public static void main(String[] args){
		GUI();
		//CLI();
	}

	private static void GUI(){
		TicTacToeGUI.start();
	}

	private static void CLI() {
		System.out.println("Do you want to go first? Yes/No");
		boolean playerFirst = sc.next().equals("Yes");
    //If the user types in Yes.
		System.out.println("0|1|2");
		System.out.println("-----");
		System.out.println("3|4|5");
		System.out.println("-----");
		System.out.println("6|7|8");
		if(!playerFirst){
			DEFAULT_BOARD.compMove();
		}
		while (!DEFAULT_BOARD.isOver()) {
      //While the game is not over
			DEFAULT_BOARD.Move(getMove());
			if (!DEFAULT_BOARD.isOver()) {
				DEFAULT_BOARD.compMove();
			}
			System.out.print(DEFAULT_BOARD);
		}
		if (DEFAULT_BOARD.hasWon("X")) {
			System.out.print("X wins.");
		}
		else if (DEFAULT_BOARD.hasWon("O")) {
			System.out.print("O wins.");
		}
		else if (DEFAULT_BOARD.isDraw()) {
			System.out.print("Draw.");
		}
	}

	private static short getMove(){
		System.out.println("Enter a number for your move.");
		short move = sc.nextShort();
		while (move > 8 || move < 0 ||
           !DEFAULT_BOARD.getEmptySpots().contains(move)) {
			System.out.println("That spot's taken.");
			System.out.println("Enter a new one");
			move = sc.nextShort();
		}
		return move;
	}
}
