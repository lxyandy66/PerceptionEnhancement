// 测量并打包输出的脚本
#define HAL_DAC_MODULE_ENABLED
#define HAL_ADC_MODULE_ENABLED

#include <Arduino.h>
#include <IOIndex.h>
#include <AgentProtocol.h>
#include <ArduinoJson.h>
#include "IoTCtrlBoardManager.h"
#include "SimplePacketOutput.h"
#include "ResistanceMapper.h"
#include <max6675.h>

#define PIN_MEASUREMENT A1
#define PIN_ENABLE D10
#define SWITCH_ENABLE LOW    // 低电平使能
#define SWITCH_DISABLE HIGH  // 低电平使能

#define ANALOG_RESOLUTION 16         // 模拟测量的分辨位数
#define ANALOG_RESOLUTION_MAX 65535  // 模拟测量分辨率的最大值

const int SAMPLING_INTERVAL = 1000;

StaticJsonDocument<1024> jsonDoc; //负责格式化输出

AnalogReader windowSensorReader(PIN_MEASUREMENT, ANALOG_RESOLUTION, 3);

ResistanceMapper rMapper(0,"M_ITO");
MAX6675 thermalCouple(D2,D3,D4); //SCLK,CS,MISO

double temperture = -999;

long loopCount = 0;  // 循环计数用
long readData = 0;
String temp="";

void setup() {
  Serial.begin(115200);
  windowSensorReader.setMapper(&rMapper);
  rMapper.setRefResistance(10000);
  rMapper.setRefVolt(ANALOG_RESOLUTION_MAX);
}

// 试一试 实际为2k ohm，ref 10k ohm

void loop() {
  delay(1000);
  temp = "";
  jsonDoc.clear();

  readData = analogRead(PIN_MEASUREMENT);
  jsonDoc[AgentProtocol::DEV_ID_FROM_JSON] = rMapper.getAcId();
  jsonDoc[AgentProtocol::REQ_ID_FROM_JSON] = loopCount;
  // jsonDoc["odt"] = readData;
  jsonDoc[AgentProtocol::DATA_FROM_JSON] = readData;// rMapper.mapping(readData);//直接输出，后期计算
  jsonDoc["temp_in"] = thermalCouple.readCelsius();

  serializeJson(jsonDoc, temp);
  Serial.println(temp);
  loopCount++;
}