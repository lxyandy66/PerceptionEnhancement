import java.awt.GridBagConstraints;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.IOException;
import java.math.BigInteger;
import java.net.Socket;
import java.util.Date;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import tool.data.processor.DatabaseImporter;
import tool.layout.AbstractGridBagPanel;
import tool.mcu.SerialManager;
import tool.mcu.ThreadSerial;
import tool.network.SocketProcessor;
import tool.network.ThreadServer;

public class PanelServer extends AbstractGridBagPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private boolean isBroadcast = true;
	/**
	 * 单点发送时设置的socket选择器
	 * 
	 */
	private String serialSelector = "";

	private String[] baudRate = { "9600", "115200" };

	private int baudSelector = Integer.valueOf(baudRate[0]);

	private JTextField tfTestId = new JTextField();// 测试ID输入框
	private JTextField tfDataLabel = new JTextField();// 数据标签输入框
	
	private JButton btSend = new JButton("Send");// 发送命令按钮
	private JButton btShutdown = new JButton("Shutdown");// 关闭按钮，关闭服务器并结束所有连接
	private JButton btStart = new JButton("Start");// 启动服务器按钮
	private JButton btInfo = new JButton("States");// 获取按钮

	private JTextArea taConsole = new JTextArea();// 交互信息显示框
	private JScrollPane panelConsole = new JScrollPane(taConsole);// 交互信息显示的滚动容器

	private JCheckBox cbBroadcast = new JCheckBox("Broadcast", isBroadcast);
	private JLabel textHint = new JLabel(" ");

	private EntityManagerFactory emf;
	
	private int msgCount=0;



	private SocketProcessor serialProc = new SocketProcessor() {

		@Override
		public void processIncomeSocket(Socket s) {
			// TODO Auto-generated method stub
		}

		@Override
		public void processIncomeMsg(String str) {
			// TODO Auto-generated method stub

			new Thread(new Runnable() {
				@Override
				public void run() {
					// TODO Auto-generated method stub
					if (str.trim().equals("") || str == null)
						return;
					
					try {
						SerialLog msgBuffer=SerialLogProcessor.msgToObject(str.trim(),tfDataLabel.getText().trim(),
								tfTestId.getText().trim(),new Date(),msgCount++);
//						AgentMsg msgBuffer = AgentMsgProcessor.msgToObject(str.trim(),
//								BigInteger.valueOf(System.currentTimeMillis()));
//						msgBuffer.setResndTime(BigInteger.valueOf(System.currentTimeMillis()));
						DatabaseImporter.simpleImport(emf, msgBuffer);
						printConsole("已持久化串口消息: " );
						// 例如单片机中为"rq"，在实体化后该字段为"reqId"
						// 这里应该在控制台输出实体化过的JSON对象，注意部分成员变量的命名：
					} catch (IllegalArgumentException e) {
						printConsole("Msg: " + str + " 指令不合法! " + e.getMessage());
					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
						printConsole("Msg: " + str + " failed resend! " + e.getMessage());
					}
				}
			}).start();

		}

		@Override
		public void printConsole(String str) {
			// TODO Auto-generated method stub
			printText(str);
		}

		@Override
		public void initalProcess(Object ts) {
			// TODO Auto-generated method stub

		}
	};


	private Thread trSerial;

	private ThreadSerial tsSerial;

	private JComboBox<String> serialComboList = new JComboBox<String>(SerialManager.getAllComPort());

	/**
	 * 目标下拉框的监听器
	 */
	private ItemListener serialCombListen = new ItemListener() {

		@Override
		public void itemStateChanged(ItemEvent e) {
			// TODO Auto-generated method stub
			if (e.getStateChange() == ItemEvent.SELECTED) {
				serialSelector = SerialManager.getAllComPort()[serialComboList.getSelectedIndex()];
				textHint.setText("Set target as: " + serialSelector);
			}
		}
	};

	private JComboBox<String> baudComboList = new JComboBox<>(baudRate);
	private ItemListener cbBaudListen = new ItemListener() {

		@Override
		public void itemStateChanged(ItemEvent e) {
			// TODO Auto-generated method stub
			if (e.getStateChange() == ItemEvent.SELECTED) {
				baudSelector = Integer.valueOf(baudRate[baudComboList.getSelectedIndex()]);
				textHint.setText("Set baud as " + baudSelector);
			}
		}
	};

	/**
	 * 广播单选框的监听器 设置是否广播并对下拉框的可编辑性进行联动
	 */
	private ItemListener cbListen = new ItemListener() {

		@Override
		public void itemStateChanged(ItemEvent e) {
			// TODO Auto-generated method stub
			isBroadcast = cbBroadcast.isSelected();
			textHint.setText("Set as " + (isBroadcast ? "Broadcast" : "Singal") + " mode");
			serialComboList.setEnabled(!isBroadcast);
		}
	};

	/**
	 * 直接与TextArea交互
	 * 
	 * @param str
	 */
	private void printText(String str) {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				taConsole.append("\n" + str);
			}
		});
	}

	/**
	 * 设置hint的消息内容
	 * 
	 * @param str
	 */
	private void printHint(String str) {
		textHint.setText(str);
	}

	public PanelServer() {
		super();
		constraints.fill = GridBagConstraints.BOTH;
		constraints.anchor = GridBagConstraints.WEST;
		addComponent(new JLabel("Test ID:"), 0, 0, 1, 1);
		addComponent(tfTestId, 0, 1, 2, 1);
		addComponent(new JLabel("   Data label:"), 0, 3, 1, 1);
		addComponent(tfDataLabel, 0, 4, 1, 1);
		
		addComponent(btSend, 1, 4, 1, 1);
		
		addComponent(new JLabel("Target:"), 1, 0, 1, 1);
		addComponent(serialComboList, 1, 1, 2, 1);
		constraints.anchor = GridBagConstraints.CENTER;
		addComponent(baudComboList, 1, 3, 1, 1);
		addComponent(btShutdown, 6, 1, 1, 1);
		addComponent(btInfo, 6, 2, 1, 1);
		addComponent(btStart, 6, 3, 1, 1);
		addComponent(textHint, 7, 0, 4, 1);
		constraints.ipady = 150;
		addComponent(panelConsole, 2, 0, 5, 4);

		// 注册监听器
		btInfo.addActionListener(this);
		btSend.addActionListener(this);
		btShutdown.addActionListener(this);
		btStart.addActionListener(this);
		serialComboList.addActionListener(this);
		serialComboList.addItemListener(serialCombListen);
		baudComboList.addActionListener(this);
		baudComboList.addItemListener(cbBaudListen);
		cbBroadcast.addItemListener(cbListen);

	}

	@Override
	public void actionPerformed(ActionEvent e) {
		// TODO Auto-generated method stub
		if (e.getSource().equals(btStart)) {
			emf = Persistence.createEntityManagerFactory("PU_PerceptionEnhancement");// 理论来说在这里就已经open了
			/*注意这里应该再优化一下，包括刚开启串口时，有部分数据仍存在*/
			EntityManager tempEM = emf.createEntityManager();
			System.out.println("持久化工厂" + (tempEM.isOpen() ? "已启动" : "未启动"));
			while (!tempEM.isOpen()) {
				;
			}
			System.out.println("持久化工厂" + (tempEM.isOpen() ? "已启动" : "未启动"));
			printText("持久化程序已启动!");

			try {
				if (serialSelector.trim().equals("")) {
					serialSelector = SerialManager.getAllComPort()[0];
				}
				tsSerial = new ThreadSerial(serialSelector, baudSelector, serialProc);// 直接赋值并初始化串口
				trSerial = new Thread(tsSerial);
				trSerial.start();// 执行串口线程
			} catch (Exception e1) {
				// TODO Auto-generated catch block
				serialProc.printConsole(e1.getMessage());
				tsSerial = null;
				e1.printStackTrace();
			}
			printText("串口已启动!");

		} else if (e.getSource().equals(btSend)) {
			try {
					tsSerial.sendMessage(tfTestId.getText().trim());
			} catch (Exception e2) {
				// TODO: handle exception
				printText("Error in Send Message: "+e2.getMessage());
			}
		} 
	}

}
