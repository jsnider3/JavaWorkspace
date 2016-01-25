import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;

public class DiceRollerImproved {
  private JFrame _GUI;
  private JPanel _Panel;
  private JLabel _DiceLabel;
  private JTextField _DiceText;
  private JLabel _ResultsLabel;
  private JTextField _ResultsText;
  private JLabel _Again;
  private JTextField _AgainText;
  private JMenuBar _MenuBar;
  private JMenu _HelpMenu;
  private JMenuItem _HelpMenuButton;
  private JButton _RollButton;
  private JButton _CalculateButton;
  private JTextArea _HelpText;

  public DiceRollerImproved() {
    _GUI = new JFrame("Josh Snider's New and Improved Dice Roller for the New World of Darkness");
    _GUI.setSize(500,400);
    _Panel = new JPanel();
    makeButtonsAndLabels();
    makeHelpMenu();
    makeLayout();
  }

  private void makeButtonsAndLabels() {
    _DiceLabel = new JLabel("Dice:");
    _DiceText = new JTextField(10);
    _Again = new JLabel("Roll again on:");
    _AgainText = new JTextField(10);
    _ResultsLabel = new JLabel("Results:");
    _ResultsText = new JTextField(10);
    _RollButton = new JButton("Roll");
    _RollButton.addActionListener(new RollListener());
    _CalculateButton = new JButton("Calculate");
    _CalculateButton.addActionListener(new CalculateListener());
  }

  private void makeHelpMenu() {
    _MenuBar = new JMenuBar();
    _HelpMenu = new JMenu();
    _MenuBar.add(_HelpMenu);
    _HelpMenuButton = new JMenuItem("Help");
    _HelpMenuButton.addActionListener(new helpListener());
    _MenuBar.add(_HelpMenuButton);
    _GUI.setJMenuBar(_MenuBar);
    _HelpText = new JTextArea();
  }

  private void makeLayout() {
    _Panel.setLayout(new GridLayout(4, 2, 0, 0));
    _Panel.add(_DiceLabel);
    _Panel.add(_DiceText);
    _Panel.add(_Again);
    _Panel.add(_AgainText);
    _Panel.add(_ResultsLabel);
    _Panel.add(_ResultsText);
    _Panel.add(_RollButton);
    _Panel.add(_CalculateButton);

    _GUI.setContentPane(_Panel);
    _GUI.pack();
    _GUI.setVisible(true);
  }

  public int getDice(){
    int tDice;
    try {
      tDice = Integer.parseInt(_DiceText.getText());
      if (tDice < 0) {
        tDice = 0;
      }
    } catch (Exception e) {
      tDice = 0;
    }
    return tDice;
  }

  /**
   * Get the cutoff for rerolls, default is 10's only.
   */
  public int getAgain() {
    int tAgain;
    try {
      tAgain = Integer.parseInt(_AgainText.getText());
      if (tAgain < 8 || tAgain > 10) {
        tAgain = 10;
      }
    } catch (NumberFormatException e) {
      tAgain = 10;
    }
    return tAgain;
  }

  class CalculateListener implements ActionListener {
    public void actionPerformed(ActionEvent arg0) {
      //TODO Rote.
      int tAgain = getAgain();
      int tDice = getDice();
      OddsCalculator tOdds = new OddsCalculator(tAgain);
      int tExpected = (int)(tDice * 0.3 + 0.3 * (11 - tAgain));
      int tStart = tExpected - 5;
      if (tStart < 0) {
        tStart = 0;
      }

      StringBuilder tBuilder = new StringBuilder();
      tBuilder.append("Dice=" + tDice + " Again=" + tAgain + "\n");
      for (int x = 0; x < 10; x++) {
        double tResult = tOdds.calculateEndAt(tStart + x, tDice);
        tBuilder.append((tStart + x) + " Successes: " + tResult + "\n");
      }

      _HelpText.setText(tBuilder.toString());
      _GUI.setTitle("Calculated values...");
      toSecondView();

    }
  }

  /**
   * Roll the dice with the given settings and set
   * the display accordingly.
   */
  public void rollDice() {
    String tOutput = "";
    int tDice = getDice();
    int tAgain = getAgain();
    boolean tRote = false;
    int tReroll = 0;
    int tSuccesses = 0;
    if (tDice > 0) {
      D10 die = new D10();
      for (int tIndex = tDice; tIndex > 0; tIndex--) {
        die.roll();
        if (die.isSuccess()) {
          tSuccesses++;
          if (die.getValue() >= tAgain) {
            tReroll++;
          }
        }
        if (!die.isSuccess() && tRote) {
          tReroll++;
        }
        tOutput += die.getValue() + " ";
      }
      for (int tIndex = tReroll; tIndex > 0; tIndex--) {
        die.roll();
        if (die.isSuccess()) {
          tSuccesses++;
          if (die.getValue() >= tAgain) {
            tIndex++;
          }
        }
        tOutput += die.getValue() + " ";
      }

      tOutput += ":" + tSuccesses + " successes";
      _ResultsText.setText(tOutput);
    }
  }

  class RollListener implements ActionListener {
    public void actionPerformed(ActionEvent arg0) {
      rollDice();
    }
  }

  /**
   * Switch to displaying the help screen.
   */
  private void toSecondView() {
    _Panel.remove(_DiceLabel);
    _Panel.remove(_DiceText);
    _Panel.remove(_ResultsLabel);
    _Panel.remove(_ResultsText);
    _Panel.remove(_Again);
    _Panel.remove(_AgainText);
    _Panel.remove(_RollButton);
    _Panel.remove(_CalculateButton);
    _Panel.add(_HelpText);
    _HelpMenuButton.setText("Go Back");
    _Panel.setLayout(new GridLayout(1, 1, 0, 0));
    _GUI.pack();
  }

  class helpListener implements ActionListener {
    public void actionPerformed(ActionEvent arg0) {
      if (_HelpMenuButton.getText().equals("Help")) {
        _HelpText.setText(
             "Type the number of dice you want to roll into the text field" + "\n"+
             " labeled \"Number of Dice to Roll:\" and press the button \"" + "\n"+
             "Roll\". In the future this will be a complete nWOD dice app." + "\n");
          _GUI.setTitle("Josh Snider's New and Improved Help Screen");
        toSecondView();
      } else {
        _Panel.remove(_HelpText);
        _HelpMenuButton.setText("Help");
        makeLayout();
      }
    }
  }

  private static void runGUI() {
    JFrame.setDefaultLookAndFeelDecorated(true);
    new DiceRollerImproved();
  }

  public static void main(String[] args) {
    javax.swing.SwingUtilities.invokeLater(new Runnable() {
      public void run() {
        runGUI();
      }
    });
  }
}
