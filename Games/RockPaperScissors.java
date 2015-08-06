import javax.swing.*;

import java.awt.*;
import java.awt.event.*;
import java.util.Random;
import java.util.Scanner;
public class RockPaperScissors {
	JFrame frame;
	JPanel buttonPanel;
	JPanel contentPanel;
	JButton button;
	JButton button2;
	JButton button3;
	JTextArea maintextarea;
	JTextArea score;
	int gameover;
	int diff;
	int cwins;
	int uwins;
	int ties;
	int x;
	int y;
	int storymode;
	int missionoffer;
	int freeplay;
	int subif;

	String compPick;
	String userPick;
	Random generator;
	Scanner input;



	public RockPaperScissors(){
		generator = new Random();
		input = new Scanner(System.in);
		gameover = 0;
		diff = 1;
		cwins = 0;
		uwins = 0;
		ties = 0;
		x = 0;
		y = 0;
		storymode = 0;
		missionoffer = 0;
		freeplay = 0;
		subif = 0;
		frame = new JFrame ("Rock, Paper, Scissors by Josh Snider");
		contentPanel = new JPanel();
		contentPanel.setLayout(new GridLayout(3, 1, 0 ,0));
		buttonPanel = new JPanel();
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

	class buttonListener implements ActionListener{
		public void actionPerformed(ActionEvent event){
			if(storymode == 0){
				//This code is executed when you press "Story Mode"
				missionoffer = 1;
				button.setText("Yes");
				button2.setText("");
				button3.setText("No");
				maintextarea.setText("The President has chosen you for a special mission.\n Your mission if you choose to accept it is to defeat Kim Jong-il in rock, paper, scissors.\n If you succeed Kim Jong-il will halt his nuclear program.\n If you fail...\n everyone dies.\n The winner is the first to 3 points. \n Do you accept the mission?");
				storymode = 1;
			}
			else if (storymode == 1){
				//This code is executed when you press "Yes"
				missionoffer = 0;
				button.setText("Easy");
				button2.setText("Normal");
				button3.setText("Hard");
				maintextarea.setText("How hard should this be? It can be easy, normal, or hard.");
				storymode = 2;
			}
			else if(storymode == 2){
				//This code is executed if the user chooses "easy"
				diff = 0;
				System.err.println("diff" +diff);
				maintextarea.setText("Which do you pick?\nRock, Paper, Scissors");
				button.setText("Rock");
				button2.setText("Paper");
				button3.setText("Scissors");
				storymode = 3;

			}
			else if(storymode == 3 ){
				//pressing rock
				//System.err.println("something");
				y = 0;
				userPick = "rock";
				x = generator.nextInt(4);
				System.err.println(x);
				if(x == 0 && subif == 0){//This is where the computer picks its move.
					compPick = "rock";
					ties ++;
					maintextarea.setText("You picked " +userPick +".\n" +"Kim Jong-il picks " + "rock" +"\nIt's a tie.");
					score.setText("The score is " + cwins +" for Kim Jong-il, " + uwins +" for you, and " + ties +" ties.");
				}
					else if(x == 1){
						compPick = "paper";
						cwins ++;
						maintextarea.setText("You picked " +userPick +".\n" +"Kim Jong-il picks " + "paper" +"\nKim Jong-il wins.");
						score.setText("The score is " + cwins +" for Kim Jong-il, " + uwins +" for you, and " + ties +" ties.");
						}
					else if (x == 2){
						compPick = "scissors";
						uwins ++;
						maintextarea.setText("You picked " +userPick +".\n" +"Kim Jong-il picks " + "scissors" +"\nYou win.");
						score.setText("The score is " + cwins +" for Kim Jong-il, " + uwins +" for you, and " + ties +" ties.");
						}
				if(x == 3 && diff == 0){
						compPick = "scissors";
						uwins ++;
						maintextarea.setText("You picked " +userPick +".\n" +"Kim Jong-il picks " + "scissors" +"\nYou win.");
						score.setText("The score is " + cwins +" for Kim Jong-il, " + uwins +" for you, and " + ties +" ties.");
						x = 2;
						subif = 1;
						}
					else if(x == 3 && diff == 1){
						compPick = "rock";
						ties ++;
						maintextarea.setText("You picked " +userPick +".\n" +"Kim Jong-il picks " + "rock" +"\nIt's a tie.");
						score.setText("The score is " + cwins +" for Kim Jong-il, " + uwins +" for you, and " + ties +" ties.");
						x = 0;
						subif = 1;
						}
					else if(x == 3 && diff ==2){
						compPick = "paper";
						cwins ++;
						maintextarea.setText("You picked " +userPick +".\n" +"Kim Jong-il picks " + "paper" +"\nKim Jong-il wins.");
						score.setText("The score is " + cwins +" for Kim Jong-il, " + uwins +" for you, and " + ties +" ties.");
						x = 1;
						subif = 1;
					}
				if(cwins == 3 && storymode == 3 && uwins < 3 && freeplay == 0){
					maintextarea.setText("No! This...\n This cannot be!\n You've failed!\n You've failed everyone!\n                             ____\n               ____  , -- -        ---   -.\n            (((   ((  ///   //   '  \\\\-\\ \\  )) ))\n        ///    ///  (( _        _   -- \\\\--     \\\\\\ \\)\n     ((( ==  ((  -- ((             ))  )- ) __   ))  ))) \n     ((  (( -=   ((  ---  (          _ ) ---  ))   ))  \n        (( __ ((    ()(((  \\  / ///     )) __ ))) \n              \\_ (( __  |     | __  ) _ ))  \n                        ,|  |  |\n                       `-._____,-'   \n                       `--.___,--'    \n                         |     |    \n                         |    || \n                         | ||  |     \n                 ,    _,   |   | | \n        (  ((  ((((  /,| __|     |  ))))  )))  )  ))\n      (()))       __/ ||(    ,,     ((//\\     )     ))))\n---((( ///_.___ _/    ||,,_____,_,,, (|\\ \\___.....__..  ))--");
					gameover = 1;
				}
					else if(uwins == 3 && storymode == 3 && cwins < 3 && freeplay == 0){
						maintextarea.setText("I can't believe it!\n You've won!\n You've saved everyone!\n You're a hero!");
						gameover = 1;
					}


			}
		}


	}

	class button2Listener implements ActionListener{
		public void actionPerformed(ActionEvent event){
			if(storymode == 2){
				//This code executes when you press normal
				diff = 1;
				maintextarea.setText("Which do you pick?\nRock, Paper, Scissors");
				button.setText("Rock");
				button2.setText("Paper");
				button3.setText("Scissors");
				storymode = 3;
				//maintextarea.setText(storymode +"");
			}
			else if(storymode == 3){
				//pressing paper
				y = 1;
				userPick = "paper";
				x = generator.nextInt(4);
				if(x == 0 && subif == 0){ //This is where the computer picks its move.
					compPick = "rock";
					uwins ++;
					maintextarea.setText("You picked " +userPick +".\n" +"Kim Jong-il picks " + compPick +"\nYou win.");
					score.setText("The score is " + cwins +" for Kim Jong-il, " + uwins +" for you, and " + ties +" ties.");
					}
					else if(x == 1){
						compPick = "paper";
						ties ++;
						maintextarea.setText("You picked " +userPick +".\n" +"Kim Jong-il picks " + compPick +"\nIt's a tie.");
						score.setText("The score is " + cwins +" for Kim Jong-il, " + uwins +" for you, and " + ties +" ties.");
						}
					else if (x == 2){
						compPick = "scissors";
						cwins ++;
						maintextarea.setText("You picked " +userPick +".\n" +"Kim Jong-il picks " + compPick +"\nKim Jong-il wins.");
						score.setText("The score is " + cwins +" for Kim Jong-il, " + uwins +" for you, and " + ties +" ties.");
						}
				if(x == 3 && diff == 0){
					compPick = "rock";
					uwins ++;
					maintextarea.setText("You picked " +userPick +".\n" +"Kim Jong-il picks " + compPick +"\nYou win.");
					score.setText("The score is " + cwins +" for Kim Jong-il, " + uwins +" for you, and " + ties +" ties.");
					x = 0;
					subif = 1;
					}
					else if(x == 3 && diff == 1){
						compPick = "paper";
						ties ++;
						maintextarea.setText("You picked " +userPick +".\n" +"Kim Jong-il picks " + compPick +"\nIt's a tie.");
						score.setText("The score is " + cwins +" for Kim Jong-il, " + uwins +" for you, and " + ties +" ties.");
						x = 1;
						subif = 1;
					}
					else if(x == 3 && diff ==2){
						compPick = "scissors";
						cwins ++;
						maintextarea.setText("You picked " +userPick +".\n" +"Kim Jong-il picks " + compPick +"\nKim Jong-il wins.");
						score.setText("The score is " + cwins +" for Kim Jong-il, " + uwins +" for you, and " + ties +" ties.");
						x = 2;
						subif = 1;
					}
				if(cwins == 3 && storymode == 3 && uwins < 3 && freeplay == 0){
					maintextarea.setText("No! This...\n This cannot be!\n You've failed!\n You've failed everyone!\n                             ____\n               ____  , -- -        ---   -.\n            (((   ((  ///   //   '  \\\\-\\ \\  )) ))\n        ///    ///  (( _        _   -- \\\\--     \\\\\\ \\)\n     ((( ==  ((  -- ((             ))  )- ) __   ))  ))) \n     ((  (( -=   ((  ---  (          _ ) ---  ))   ))  \n        (( __ ((    ()(((  \\  / ///     )) __ ))) \n              \\_ (( __  |     | __  ) _ ))  \n                        ,|  |  |\n                       `-._____,-'   \n                       `--.___,--'    \n                         |     |    \n                         |    || \n                         | ||  |     \n                 ,    _,   |   | | \n        (  ((  ((((  /,| __|     |  ))))  )))  )  ))\n      (()))       __/ ||(    ,,     ((//\\     )     ))))\n---((( ///_.___ _/    ||,,_____,_,,, (|\\ \\___.....__..  ))--");
					gameover = 1;
				}
				else if(uwins == 3 && storymode == 3 && cwins < 3 && freeplay == 0){
					maintextarea.setText("I can't believe it!\n You've won!\n You've saved everyone!\n You're a hero!");
					gameover = 1;
				}
			}
		}
	}

	class button3Listener implements ActionListener{
		public void actionPerformed(ActionEvent event){
			if(missionoffer == 1){
				maintextarea.setText("After you turned down the mission the U.N. dispatched General Specific.\n After 4 rounds both were tied with 2 points...\n The next round would determine the fate of the world.\n In this moment of truth, General Specific failed. Kim's rock crushed not only the General' scissors\n but also the hopes and dreams of the human race.\n Thus, Kim Jong-il was free to start the first and the last global thermonuclear war.\n                              ____\n                ____  , -- -        ---   -.\n             (((   ((  ///   //   '  \\\\-\\ \\  )) ))\n         ///    ///  (( _        _   -- \\\\--     \\\\\\ \\)\n      ((( ==  ((  -- ((             ))  )- ) __   ))  ))) \n      ((  (( -=   ((  ---  (          _ ) ---  ))   ))  \n         (( __ ((    ()(((  \\  / ///     )) __ ))) \n               \\_ (( __  |     | __  ) _ ))  \n                         ,|  |  |\n                        `-._____,-'\n                        `--.___,--'\n                            |     |\n                            |    ||\n                            | ||  |\n                            | ||  |\n         (  ((  ((((  /, | __|     |  ))))  )))  )  ))\n       (()))       __/ ||(    ,,     ((//\\     )     ))))\n ---((( ///_.___ _/    ||,,_____,_,,, (|\\ \\___.....__..  ))--\n");
				score.setText("You lose.");
			}
			if(storymode == 0){
				//This code executes if you select free play
				freeplay = 1;
				storymode = 1;
			}
			if(freeplay == 1 && storymode == 1){
				//This code executes if you're in free play
				missionoffer = 0;
				button.setText("Easy");
				button2.setText("Normal");
				button3.setText("Hard");
				maintextarea.setText("How hard should this be? It can be easy, normal, or hard.");
				storymode = 2;
			}
			else if(storymode == 2){
				//This code executes if you click hard
				diff = 2;
				maintextarea.setText("Which do you pick?\nRock, Paper, Scissors");
				button.setText("Rock");
				button2.setText("Paper");
				button3.setText("Scissors");
				storymode = 3;
			}
			else if(storymode == 3){
				//pressing scissors
				y = 2;
				userPick = "scissors";
				x = generator.nextInt(4);
				if(x == 0 && subif == 0){ //This is where the computer picks its move.
					compPick = "rock";
					cwins ++;
					maintextarea.setText("You picked " +userPick +".\n" +"Kim Jong-il picks " + compPick +"\nKim Jong-il wins.");
					score.setText("The score is " + cwins +" for Kim Jong-il, " + uwins +" for you, and " + ties +" ties.");
					}
					else if(x == 1){
						compPick = "paper";
						uwins ++;
						maintextarea.setText("You picked " +userPick +".\n" +"Kim Jong-il picks " + compPick +"\nYou win.");
						score.setText("The score is " + cwins +" for Kim Jong-il, " + uwins +" for you, and " + ties +" ties.");
						}
					else if (x == 2){
						compPick = "scissors";
						ties ++;
						maintextarea.setText("You picked " +userPick +".\n" +"Kim Jong-il picks " + compPick +"\nIt's a tie.");
						score.setText("The score is " + cwins +" for Kim Jong-il, " + uwins +" for you, and " + ties +" ties.");
						}
				if(x == 3 && diff == 0){
					compPick = "paper";
					uwins ++;
					maintextarea.setText("You picked " +userPick +".\n" +"Kim Jong-il picks " + compPick +"\nYou win.");
					score.setText("The score is " + cwins +" for Kim Jong-il, " + uwins +" for you, and " + ties +" ties.");
					x = 1;
					subif = 1;
					}
					else if(x == 3 && diff == 1){
						compPick = "scissors";
						ties ++;
						maintextarea.setText("You picked " +userPick +".\n" +"Kim Jong-il picks " + compPick +"\nIt's a tie.");
						score.setText("The score is " + cwins +" for Kim Jong-il, " + uwins +" for you, and " + ties +" ties.");
						x = 2;
						subif = 1;
					}
					else if(x == 3 && diff ==2){
						compPick = "rock";
						cwins ++;
						maintextarea.setText("You picked " +userPick +".\n" +"Kim Jong-il picks " + compPick +"\nKim Jong-il wins.");
						score.setText("The score is " + cwins +" for Kim Jong-il, " + uwins +" for you, and " + ties +" ties.");
						x = 0;
						subif = 1;
					}
				//System.err.println(f);
				if(cwins == 3 && storymode == 3 && uwins < 3 && freeplay == 0){
					maintextarea.setText("No! This...\n This cannot be!\n You've failed!\n You've failed everyone!\n                             ____\n               ____  , -- -        ---   -.\n            (((   ((  ///   //   '  \\\\-\\ \\  )) ))\n        ///    ///  (( _        _   -- \\\\--     \\\\\\ \\)\n     ((( ==  ((  -- ((             ))  )- ) __   ))  ))) \n     ((  (( -=   ((  ---  (          _ ) ---  ))   ))  \n        (( __ ((    ()(((  \\  / ///     )) __ ))) \n              \\_ (( __  |     | __  ) _ ))  \n                        ,|  |  |\n                       `-._____,-'   \n                       `--.___,--'    \n                         |     |    \n                         |    || \n                         | ||  |     \n                 ,    _,   |   | | \n        (  ((  ((((  /,| __|     |  ))))  )))  )  ))\n      (()))       __/ ||(    ,,     ((//\\     )     ))))\n---((( ///_.___ _/    ||,,_____,_,,, (|\\ \\___.....__..  ))--");
					gameover = 1;
				}
				else if(uwins == 3 && storymode == 3 && cwins < 3 && freeplay == 0){
					maintextarea.setText("I can't believe it!\n You've won!\n You've saved everyone!\n You're a hero!");
					gameover = 1;
				}
			}
		}
	}
	private static void runGUI(){
		JFrame.setDefaultLookAndFeelDecorated(true);

		RockPaperScissors sw = new RockPaperScissors();
	}

	public static void main(String[] args){
		javax.swing.SwingUtilities.invokeLater(new Runnable(){
			public void run() {
				runGUI();
			}
		});
	}
}
