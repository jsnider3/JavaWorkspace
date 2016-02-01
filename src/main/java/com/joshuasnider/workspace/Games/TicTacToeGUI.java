import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

//Optimal play for Tic Tac Toe can be found in the 832nd XKCD strip.

public class TicTacToeGUI {

  private JFrame frame;
  private JPanel panel;
  private boolean PLAYER_X;//True means player is X, false means player is O.
  private String USER_CHAR;
  private String COMP_CHAR;
  private JButton[] buttonList;
  private TicTacToeBoard board;

  public TicTacToeGUI(TicTacToeBoard brd, boolean playerX) {
    board = brd;
    PLAYER_X = playerX;
    USER_CHAR = PLAYER_X?"X":"O";//If the player is x, their character is x, else it's o.
    COMP_CHAR = PLAYER_X?"O":"X";
    setUpGraphics();
    if (!PLAYER_X) {//If the player isn't X, the computer goes first.
      compMove();
    }
    frame.setVisible(true);
  }

  public void setUpGraphics(){
    frame = new JFrame("Tic Tac Toe");
    panel = new JPanel();
    buttonList = new JButton[9];
    for(short x=0;x<9;x++){
      buttonList[x]=new JButton();//Create 9 buttons.
      panel.add(buttonList[x]);//Add them to the screen.
      buttonList[x].addActionListener(new TicTacToeListener(x));//Give them Listeners.
    }
    panel.setLayout(new GridLayout(3, 3, 0 ,0));
    frame.add(panel);
    frame.setSize(300,300);
    //Showtime.
  }

  protected class TicTacToeListener implements ActionListener{
    private short num;

    public TicTacToeListener(short n){
      num = n;
    }

    public void actionPerformed(ActionEvent arg0) {
      playerMove(num);
    }
  }

  private void playerMove(short s){
    if (buttonList[s].getText().equals("") && !board.isOver()) {
      //If the button is blank and the game isn't over yet.
      buttonList[s].setText(USER_CHAR);//Set the button's text equal to USER_CHAR.
      board.Move(s);
      if (!board.isOver()) {
        compMove();
      }
      if (board.hasWon(USER_CHAR)) {
        JOptionPane.showMessageDialog(null, "You won, since TicTacToe is a solved game, that means there's a bug in my game.");
        /*if(t==JOptionPane.YES_OPTION){
          start();
        }*/
      }
      else if (board.isDraw()) {
        JOptionPane.showMessageDialog(null, "It's a draw as expected.");
        /*if(t==JOptionPane.YES_OPTION){
          start();
        }*/
      }
      else if (board.hasWon(COMP_CHAR)) {
        JOptionPane.showMessageDialog(null, "The computer won. You must have made a bad move.");
        /*if(t==JOptionPane.YES_OPTION){
          start();
        }*/
      }
      board.printVerbose();
    }

  }

  public void compMove() {
    short optimalMove;
    if (PLAYER_X) {
      optimalMove = TicTacToeBoard.min(board.children()).getRecentMove();
    } else {
      optimalMove = TicTacToeBoard.max(board.children()).getRecentMove();
    }
    buttonList[optimalMove].setText(COMP_CHAR);
    board.Move(optimalMove);
  }

  public static void start(){
    int t = JOptionPane.showConfirmDialog(null, "Do you want to go first?");
    if (t == JOptionPane.YES_OPTION) {
      new TicTacToeGUI(TicTacToeGame.DEFAULT_BOARD, true);
    }
    else if (t == JOptionPane.NO_OPTION) {
      new TicTacToeGUI(TicTacToeGame.DEFAULT_BOARD, false);
    }
    else {
      System.exit(0);
    }
  }

  public static void main(String[] args){
    start();
  }
}
