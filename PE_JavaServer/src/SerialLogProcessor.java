import java.util.Date;

/**
 * @author Mr_Li
 * 将串口收到的信息处理为可持久化的对象
 */
public class SerialLogProcessor {
	
	/**
	 * @param msg
	 * @return
	 */
	public static SerialLog msgToObject(String msg,String dataLabel,String testId,Date logTime,int msgId) {
		SerialLog sl=new SerialLog();
		sl.setMsgContent(msg);
		sl.setTestId(testId);
		sl.setMsgId(msgId);
		sl.setDataLabel(dataLabel);
		sl.setRecTime(logTime);
		return sl;
	}

}
