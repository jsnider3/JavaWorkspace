import java.io.FileOutputStream;
import java.io.IOException;
import java.net.*;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import javax.swing.*;
  
public class Anima {
    public static void main(String[] args) throws MalformedURLException {
 
        URL url = new URL("http://pinkie.ponychan.net/chan/files/src/132287935236.gif");
        ImageIcon icon = new ImageIcon(url);
        JLabel label = new JLabel(icon);
        icon.getImage();
        System.out.println(icon.toString());
        try {
		    //File outputfile = new File("saved.gif");
		    //ImageIO.write(ImageIO.read(url), "gif", outputfile);
        	URL source = new URL("http://pinkie.ponychan.net/chan/files/src/132287935236.gif");
        	ReadableByteChannel in = Channels.newChannel(source.openStream());
        	FileOutputStream out = new FileOutputStream("saved.gif");
        	out.getChannel().transferFrom(in, 0, 1 << 24);
        	out.close();
		    //outputfile.write(icon.toString());
		    /*This returns the error message.
		     * Exception in thread "main" java.lang.ClassCastException: sun.awt.image.ToolkitImage cannot be cast to java.awt.image.RenderedImage
			 *		at Anima.main(Anima.java:25)
		     */
		} catch (IOException e) {
			e.printStackTrace();
		}
        JFrame f = new JFrame("Animation");
        f.getContentPane().add(label);
        f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        f.pack();
        f.setLocationRelativeTo(null);
        f.setVisible(true);
    }
}