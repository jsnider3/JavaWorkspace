/**
 * This is a joke Rock Paper Scissors game. It has multiple difficulties,
 *  a story mode, and ASCII art graphics.
 *
 * This was actually one of the first programs I ever wrote. I originally
 *  did it at a summer camp during High School and since then I've refactored
 *  it a couple times.
 *
 * @author Josh Snider
 */

import javax.swing.*;

import java.awt.*;
import java.awt.event.*;
import java.util.Random;

public class RockPaperScissors {
  private JButton button;
  private JButton button2;
  private JButton button3;
  private JTextArea maintextarea;
  private JTextArea score;
  private Difficulty diff;
  private int cwins;
  private int uwins;
  private int ties;
  private GameStage gamestage;

  private String[] moves = {"rock", "paper", "scissors"};

  private Random generator;

  public RockPaperScissors(){
    generator = new Random();
    cwins = 0;
    uwins = 0;
    ties = 0;
    gamestage = GameStage.MODE_SELECT;
    JFrame frame = new JFrame ("Rock, Paper, Scissors by Josh Snider");
    JPanel contentPanel = new JPanel();
    contentPanel.setLayout(new GridLayout(3, 1, 0 ,0));
    JPanel buttonPanel = new JPanel();
    button = new JButton("Story Mode");
    button.addActionListener(new buttonListener());
    button2 = new JButton("");
    button2.addActionListener(new button2Listener());
    button3 = new JButton("Free Play");
    button3.addActionListener(new button3Listener());
    buttonPanel.add(button);
    buttonPanel.add(button2);
    buttonPanel.add(button3);
    contentPanel.add(buttonPanel);

    maintextarea = new JTextArea("Let's play rock, paper, scissors.");
    contentPanel.add(maintextarea);
    score = new JTextArea("");
    contentPanel.add(score);
    frame.setContentPane(contentPanel);
    frame.pack();
    frame.setVisible(true);
    button.setBackground(Color.WHITE);
    button2.setBackground(Color.WHITE);
  }

  /**
   * Adjust the scoreboard given each person's moves.
   */
  private void adjustScore(int userPick, int compPick) {
    if (userPick == compPick - 1 || userPick == compPick + 2) {
      uwins++;
      maintextarea.setText("You picked " + moves[userPick] +".\n" +
        "Kim Jong-il picks " + moves[compPick] +"\nYou win.");
    }
    else if (userPick == compPick) {
      ties++;
      maintextarea.setText("You picked " + moves[userPick] +".\n" +
        "Kim Jong-il picks " + moves[compPick] +"\nIt's a tie.");
    }
    else if (compPick == userPick - 1 || compPick == userPick + 2) {
      cwins++;
      maintextarea.setText("You picked " + moves[userPick] +".\n" +
        "Kim Jong-il picks " + moves[compPick] +"\nKim Jong-il wins.");
    }
    score.setText("The score is " + cwins +" for Kim Jong-il, " + uwins +" for you, and " + ties +" ties.");
  }

  /**
   * Prompt the user to choose a difficulty.
   */
  private void askDifficulty() {
    button.setText("Easy");
    button2.setText("Normal");
    button3.setText("Hard");
    maintextarea.setText("How hard should this be? It can be easy, normal, or hard.");
  }

  /**
   * Get the computer's response to a user's move.
   */
  private int getCompPick(int userPick) {
    int res = generator.nextInt(4);
    if (res == 3) {
      if (diff == Difficulty.EASY) {
        res = (userPick + 1) % 3;
      } else if (diff == Difficulty.HARD) {
        res = (userPick + 2) % 3;
      }
    }
    return res;
  }

  private void handleUserMove(int userPick) {
    int compPick = getCompPick(userPick);
    adjustScore(userPick, compPick);
    if (cwins == 3 && uwins < 3 && gamestage == GameStage.PLAYING_STORY) {
      maintextarea.setText("No! This...\n This cannot be!\n You've failed!\n You've failed everyone!\n                             ____\n               ____  , -- -        ---   -.\n            (((   ((  ///   //   '  \\\\-\\ \\  )) ))\n        ///    ///  (( _        _   -- \\\\--     \\\\\\ \\)\n     ((( ==  ((  -- ((             ))  )- ) __   ))  ))) \n     ((  (( -=   ((  ---  (          _ ) ---  ))   ))  \n        (( __ ((    ()(((  \\  / ///     )) __ ))) \n              \\_ (( __  |     | __  ) _ ))  \n                        ,|  |  |\n                       `-._____,-'   \n                       `--.___,--'    \n                         |     |    \n                         |    || \n                         | ||  |     \n                 ,    _,   |   | | \n        (  ((  ((((  /,| __|     |  ))))  )))  )  ))\n      (()))       __/ ||(    ,,     ((//\\     )     ))))\n---((( ///_.___ _/    ||,,_____,_,,, (|\\ \\___.....__..  ))--");
      gamestage = GameStage.STORY_OVER;
    }
    else if (uwins == 3 && cwins < 3 && gamestage == GameStage.PLAYING_STORY) {
      maintextarea.setText("I can't believe it!\n You've won!\n You've saved everyone!\n You're a hero!");
      gamestage = GameStage.STORY_OVER;
    }
  }

  private void setDifficulty(Difficulty diff) {
    this.diff = diff;
    maintextarea.setText("Which do you pick?\nRock, Paper, Scissors");
    button.setText("Rock");
    button2.setText("Paper");
    button3.setText("Scissors");
    if (gamestage == GameStage.STORY_DIFFICULTY) {
      gamestage = GameStage.PLAYING_STORY;
    } else {
      gamestage = GameStage.PLAYING_FREELY;
    }
  }

  class buttonListener implements ActionListener {

    public void actionPerformed(ActionEvent event) {
      if (gamestage == GameStage.MODE_SELECT) {
        //This code is executed when you press "Story Mode"
        gamestage = GameStage.MISSION_OFFER;
        button.setText("Yes");
        button2.setText("");
        button3.setText("No");
        maintextarea.setText("The President has chosen you for a special mission.\n Your mission if you choose to accept it is to defeat Kim Jong-il in rock, paper, scissors.\n If you succeed Kim Jong-il will halt his nuclear program.\n If you fail...\n everyone dies.\n The winner is the first to 3 points. \n Do you accept the mission?");
      }
      else if (gamestage == GameStage.MISSION_OFFER) {
        //This code is executed when you press "Yes"
        gamestage = GameStage.STORY_DIFFICULTY;
        askDifficulty();
      }
      else if (gamestage == GameStage.STORY_DIFFICULTY ||
          gamestage == GameStage.FREE_DIFFICULTY) {
        setDifficulty(Difficulty.EASY);
      }
      else if (gamestage == GameStage.PLAYING_FREELY ||
          gamestage == GameStage.PLAYING_STORY) {
        handleUserMove(0);
      }
    }

  }

  class button2Listener implements ActionListener {
    public void actionPerformed(ActionEvent event) {
      if (gamestage == GameStage.STORY_DIFFICULTY ||
          gamestage == GameStage.FREE_DIFFICULTY) {
        setDifficulty(Difficulty.NORMAL);
      }
      else if (gamestage == GameStage.PLAYING_FREELY ||
          gamestage == GameStage.PLAYING_STORY) {
        handleUserMove(1);
      }
    }
  }

  class button3Listener implements ActionListener {
    public void actionPerformed(ActionEvent event) {
      if (gamestage == GameStage.MISSION_OFFER) {
        maintextarea.setText("After you turned down the mission the U.N. dispatched General Specific.\n After 4 rounds both were tied with 2 points...\n The next round would determine the fate of the world.\n In this moment of truth, General Specific failed. Kim's rock crushed not only the General' scissors\n but also the hopes and dreams of the human race.\n Thus, Kim Jong-il was free to start the first and the last global thermonuclear war.\n                              ____\n                ____  , -- -        ---   -.\n             (((   ((  ///   //   '  \\\\-\\ \\  )) ))\n         ///    ///  (( _        _   -- \\\\--     \\\\\\ \\)\n      ((( ==  ((  -- ((             ))  )- ) __   ))  ))) \n      ((  (( -=   ((  ---  (          _ ) ---  ))   ))  \n         (( __ ((    ()(((  \\  / ///     )) __ ))) \n               \\_ (( __  |     | __  ) _ ))  \n                         ,|  |  |\n                        `-._____,-'\n                        `--.___,--'\n                            |     |\n                            |    ||\n                            | ||  |\n                            | ||  |\n         (  ((  ((((  /, | __|     |  ))))  )))  )  ))\n       (()))       __/ ||(    ,,     ((//\\     )     ))))\n ---((( ///_.___ _/    ||,,_____,_,,, (|\\ \\___.....__..  ))--\n");
        score.setText("You lose.");
      }
      if (gamestage == GameStage.MODE_SELECT) {
        gamestage = GameStage.FREE_DIFFICULTY;
        askDifficulty();
      }
      else if (gamestage == GameStage.STORY_DIFFICULTY ||
          gamestage == GameStage.FREE_DIFFICULTY) {
        setDifficulty(Difficulty.HARD);
      }
      else if (gamestage == GameStage.PLAYING_FREELY ||
          gamestage == GameStage.PLAYING_STORY) {
        handleUserMove(2);
      }
    }
  }

  private static void runGUI() {
    JFrame.setDefaultLookAndFeelDecorated(true);

    new RockPaperScissors();
  }

  public static void main(String[] args) {
    javax.swing.SwingUtilities.invokeLater(new Runnable() {
      public void run() {
        runGUI();
      }
    });
  }
}
