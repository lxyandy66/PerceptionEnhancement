// 通过热电偶+MAX6675模块测温度
//MAX31855这个芯片是真的没用成过

#include <Adafruit_MAX31855.h>
#include <max6675.h>
#include <IOIndex.h>
#include <CtrlBoardIndex.h>

#define ANALOG_RESOLUTION 16         // 模拟测量的分辨位数
#define ANALOG_RESOLUTION_MAX 65535  // 模拟测量分辨率的最大值
#define TIME_STARTUP 100             // 使能后至采样的等待时间

#define VNAME_R_ITO "R_ITO"
#define VNAME_T_IN "T_IN"
#define VNAME_T_OUT "T_OUT"

#define PIN_MEASUREMENT_ITO A1
#define PIN_ENABLE D13

#define PIN_CS_TIN D6
#define PIN_CS_TOUT D9
#define PIN_CS_TENV D11

float temperature = 0;
long loopCount = 0;
StaticJsonDocument<1024> jsonDoc;
PackedMAX6675 tcIn("TC_in", VNAME_T_IN,(new MAX6675(D2, D6, D10)));
PackedMAX6675 tcOut("TC_out", VNAME_T_OUT,(new MAX6675(D2, D9, D10)));

AnalogReader windowSensorReader("ITO",VNAME_R_ITO,PIN_MEASUREMENT_ITO,ANALOG_RESOLUTION,3);


String temp = "";

void setup() {
  // dudu
  Serial.begin(115200);
  pinMode(D6,OUTPUT);
  pinMode(D9, OUTPUT);
  digitalWrite(D6, HIGH);
  digitalWrite(D9, HIGH);
  //   if (!thermalCouple2.begin()) {
  //     Serial.println("ERROR.");
  //     while (1)
  //       delay(10);
  //   }
  Serial.println("Setup finished!");
}

void loop() {
  jsonDoc.clear();
  jsonDoc[AgentProtocol::REQ_ID_FROM_JSON] = loopCount++;
  // sensor.outputStatus(&jsonDoc, true);
  tcIn.updateMeasurement();
  tcOut.updateMeasurement();

  tcIn.outputStatus(&jsonDoc, true,true);
  tcOut.outputStatus(&jsonDoc, true,true);

  windowSensorReader.outputStatus(&jsonDoc, true, true);


  serializeJson(jsonDoc, temp);
  Serial.println(temp);
  temp = "";
  // Serial.print("MAX31855: ");
  // Serial.println(thermalCouple2.readCelsius(), 3);

  // For the MAX6675 to update, you must delay AT LEAST 250ms between reads!
  delay(1000);
}