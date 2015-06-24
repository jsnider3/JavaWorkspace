
public class DancingMan {

	public static void main(String[] args) {
		int dancepose = 1;
		int run = 1;
		while(run == 1){
			if(dancepose == 3){
				try {
					Thread.sleep(125);
				} catch (InterruptedException e) {
					
					e.printStackTrace();
				}
				System.out.println("<o>\n| \n/<\n");
				dancepose = 1;
			}
			if(dancepose == 1){
				try {
					Thread.sleep(125);
				} catch (InterruptedException e) {
					
					e.printStackTrace();
				}
				System.out.println(" o>\n<|  \n >\\\n");
				dancepose =2;
				
			}
			if(dancepose == 2){
				try {
					Thread.sleep(125);
				} catch (InterruptedException e) {
					
					e.printStackTrace();
				}
				System.out.println(" o\n<|>\n /<\n");
				dancepose = 3;
			}
		}
	}
}
