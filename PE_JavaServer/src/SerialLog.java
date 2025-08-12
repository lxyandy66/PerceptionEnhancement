import java.io.Serializable;
import javax.persistence.*;

import java.util.Date;


/**
 * The persistent class for the serialacquisition database table.
 * 
 */
@Entity
@Table(name="serialacquisition")
public class SerialLog implements Serializable {
	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy=GenerationType.IDENTITY)
	@Column(name="log_id")
	private int logId;

	@Column(name="data_label")
	private String dataLabel;

	@Column(name="msg_content")
	private String msgContent;

	@Column(name="msg_id")
	private int msgId;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="rec_time")
	private Date recTime;

	@Column(name="test_id")
	private String testId;

	public SerialLog() {
	}
	
	public SerialLog(String msg,String dataLabel,String testId,Date logTime,int msgId) {
		this.msgContent=msg;
		this.testId=testId;
		this.msgId=msgId;
		this.dataLabel=dataLabel;
		this.recTime=logTime;
	}

	public int getLogId() {
		return this.logId;
	}

	public void setLogId(int logId) {
		this.logId = logId;
	}

	public String getDataLabel() {
		return this.dataLabel;
	}

	public void setDataLabel(String dataLabel) {
		this.dataLabel = dataLabel;
	}

	public String getMsgContent() {
		return this.msgContent;
	}

	public void setMsgContent(String msgContent) {
		this.msgContent = msgContent;
	}

	public int getMsgId() {
		return this.msgId;
	}

	public void setMsgId(int msgId) {
		this.msgId = msgId;
	}

	public Date getRecTime() {
		return this.recTime;
	}

	public void setRecTime(Date recTime) {
		this.recTime = recTime;
	}

	public String getTestId() {
		return this.testId;
	}

	public void setTestId(String testId) {
		this.testId = testId;
	}
	
	@Override
	public SerialLog clone() throws CloneNotSupportedException {
		// TODO Auto-generated method stub
		return (SerialLog)super.clone();
	}

}