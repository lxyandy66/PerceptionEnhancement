// 通过热电偶+MAX6675模块测温度
//MAX31855这个芯片是真的没用成过

#include <Adafruit_MAX31855.h>
#include <max6675.h>
#include <IOIndex.h>
#include <CtrlBoardIndex.h>

#define ANALOG_RESOLUTION 16         // 模拟测量的分辨位数
#define ANALOG_RESOLUTION_MAX 65535  // 模拟测量分辨率的最大值
#define TIME_STARTUP 100             // 使能后至采样的等待时间

// MAX6675 thermalCouple(D2, D3, D4);             // SCLK,CS,MISO
// Adafruit_MAX31855 thermalCouple2(D2, D5, D4);  // SCLK,CS,MISO
float temperature = 0;
StaticJsonDocument<1024> jsonDoc;
PackedMAX6675 tcIn("TC_in", "t_in",(new MAX6675(D2, D3, D4)));
PackedMAX6675 tcOut("TC_out", "t_out",(new MAX6675(D2, D5, D4)));

String temp = "";

void setup() {
  // dudu
  Serial.begin(115200);
//   if (!thermalCouple2.begin()) {
//     Serial.println("ERROR.");
//     while (1)
//       delay(10);
//   }
  Serial.println("Setup finished!");
}

void loop() {
  // dd
  //  Serial.println("in loop");
  //  temperature = thermalCouple.readCelsius();
//    Serial.print("MAX6675: ");
//    Serial.println(thermalCouple.readCelsius(), 3);

  jsonDoc.clear();
  // sensor.outputStatus(&jsonDoc, true);
  tcIn.updateMeasurement();
  tcOut.updateMeasurement();

  tcIn.outputStatus(&jsonDoc, true,true);
  tcOut.outputStatus(&jsonDoc, true,true);

  serializeJson(jsonDoc, temp);
  Serial.println(temp);
  temp = "";
  // Serial.print("MAX31855: ");
  // Serial.println(thermalCouple2.readCelsius(), 3);

  // For the MAX6675 to update, you must delay AT LEAST 250ms between reads!
  delay(1000);
}