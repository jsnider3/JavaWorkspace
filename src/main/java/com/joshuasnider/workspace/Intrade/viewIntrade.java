package com.joshuasnider.workspace.intrade;

import java.io.IOException;
import java.net.URL;
import java.util.List;
import com.joshuasnider.workspace.internetio.Lexer;

public class viewIntrade {

  public static void main(String[] args) throws IOException {
    Lexer lex = new Lexer();
    URL markets = new URL("http://www.intrade.com/v4/markets/");
    Lexer.print(markets);
    List<String> page = lex.getWebpage(markets);
  }
}
