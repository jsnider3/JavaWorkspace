public class OddsCalculator {
	int again;
	Double[][] memo;
	public OddsCalculator(int aAgain){
		again = aAgain;
		if(again < 8 || again >10){
			again = 10;
		}
		memo = new Double[10][10];
	}
	
	public double calculateEndAt(int goal, int dice){
		//TODO This is only valid for non-rote.
		if(goal==0 && dice==0){
			return 1;
		}
		if(goal==0){
			return Math.pow(getFailProportion(), dice);
		}
		if(dice==0){
			return 0;
		}
		if(goal < 11 && dice < 11){
			if(memo[goal-1][dice-1] != null){
				return memo[goal - 1][dice - 1];
			}
		}
		double result = getFailProportion() * calculateEndAt(goal, dice - 1) +
			   getSimpleSuccessProportion() * calculateEndAt(goal - 1, dice - 1) +
			   getRerollProportion() * calculateEndAt(goal - 1, dice);
		if(goal < 11 && dice < 11){
			memo[goal - 1][dice - 1] = result;
		}
		return result;
	}
	
	public double getFailProportion(){
		return .7;
	}
	
	public double getSimpleSuccessProportion(){
		return 0.3 - getRerollProportion();
	}
	
	public double getRerollProportion(){
		return (10 + 1 - again)/10.0;
	}
	
	public double calculateAtLeast(int goal, int dice){
		double result = 1;
		for(int x = 0; x<goal; x++){
			result -= calculateEndAt(x, dice);
		}
		return result;
	}
	
	public double calculateLessThan(int goal, int dice){
		double result = 0;
		for(int x = 0; x<goal; x++){
			result += calculateEndAt(x, dice);
		}
		return result;
	}

	public static void main(String[] args){
		int x=0;
		double total = 0;
		OddsCalculator calc = new OddsCalculator(10);
		while(true){
			double result = calc.calculateEndAt(10, x);
			total += result;
			System.out.println(x + " Result: " + result);
			/*if(x > 8){
				break;
			}*/
			x++;
		}
	}
	
}
