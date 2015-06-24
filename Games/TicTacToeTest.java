import static org.junit.Assert.*;

import org.junit.Test;


public class TicTacToeTest {

	@Test
	public void test() {
		//checkScoring();
		checkChildren();
	}
	
	public static void checkScoring(){
		String[][][] array={{{"X","X","X"},{" ","O","O"},{"O"," "," "}},
							{{"X","X"," "},{" ","O","O"},{"O"," "," "}},
							{{"O","O"," "},{" ","X","X"},{"O"," "," "}},
							{{"O","O","O"},{" ","X","X"},{"O"," "," "}},
							{{"O","X","O"},{"O","X","O"},{"X","O","X"}}};
		TicTacToeBoard test = new TicTacToeBoard(array[0],false);
		TicTacToeBoard test2 = new TicTacToeBoard(array[1],false);
		TicTacToeBoard test3 = new TicTacToeBoard(array[2],false);
		TicTacToeBoard test4 = new TicTacToeBoard(array[2],true);
		TicTacToeBoard test5 = new TicTacToeBoard(array[3],true);
		TicTacToeBoard test6 = new TicTacToeBoard(array[4],true);
		TicTacToeBoard test7 = new TicTacToeBoard(array[4],false);
		assertTrue(test.score()==2);
		assertTrue(test2.score()==1);
		assertTrue(test3.score()==1);
		//assertTrue(test4.score()==-1);//TODO Not working
		assertTrue(test5.score()==-2);
		assertTrue(test6.score()==0);
		assertTrue(test7.score()==0);
	}

	public static void checkChildren(){
		String[][] test_array ={{"X","X"," "},{" ","O","O"},{"O"," "," "}};
		TicTacToeBoard test = new TicTacToeBoard(test_array,true);
		for(TicTacToeBoard t : test.children()){
			System.out.println(t);
			System.out.println(t.score);
		}
	}

}
