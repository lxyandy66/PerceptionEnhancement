import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JTabbedPane;

public class LayoutMain extends JFrame implements ActionListener {
	private JButton btExit = new JButton("EXIT");
	private JMenuBar menuBar = new JMenuBar();
	private JMenu menuAbout = new JMenu("About");
	private JMenuItem itemOwner = new JMenuItem("Copyright");
	
	private JTabbedPane tabPane = new JTabbedPane();
	private Container layoutContainer;
	private BorderLayout layoutMain = new BorderLayout(10, 10);
	
	private PanelServer pnTcpMain=new PanelServer();
	

	public LayoutMain() {
		super();
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		// TODO Auto-generated method stub
		Object event = e.getSource();
		if (event.equals(btExit)) {
			System.out.println("Exit Button pressed ,System exit");
			System.exit(0);
		} else if (event.equals(itemOwner)) {
			System.out.println("Menu select.");
			JOptionPane.showMessageDialog(null, "Powered by L'", "About",
					JOptionPane.INFORMATION_MESSAGE);
		}
	}

	public LayoutMain(int posX, int posY, int width, int high) {
		setTitle("Control Server");
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setLocation(posX, posY);
		setSize(width, high);
		setJMenuBar(menuBar);
		menuAbout.add(itemOwner);
		menuBar.add(menuAbout);
		tabPane.addTab("Serial Monitor", null, pnTcpMain, "TCP Tab");


		layoutContainer = getContentPane();
		layoutContainer.setLayout(layoutMain);
		layoutContainer.add(tabPane, BorderLayout.CENTER);
		layoutContainer.add(btExit, BorderLayout.SOUTH);// 这里还能搞得好看点

		btExit.addActionListener(this);
		itemOwner.addActionListener(this);
	}
	
}
