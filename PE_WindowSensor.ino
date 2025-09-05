// 测量并打包输出的脚本
#define HAL_DAC_MODULE_ENABLED
#define HAL_ADC_MODULE_ENABLED

#include <Arduino.h>
#include <IOIndex.h>
#include <CtrlBoardIndex.h>
#include <AgentProtocol.h>
#include <ArduinoJson.h>
#include "IoTCtrlBoardManager.h"
#include "SimplePacketOutput.h"
#include "ResistanceMapper.h"
#include <max6675.h>
#include<Chrono.h>
#include <vector>

/****电路定义****/
#define PIN_MEASUREMENT_ITO A1
#define PIN_MEASUREMENT_AgNW A2
#define PIN_MEASUREMENT_PTIN A3
#define PIN_MEASUREMENT_PTOUT A4
#define PIN_ENABLE D13
#define SWITCH_ENABLE LOW    // 低电平使能
#define SWITCH_DISABLE HIGH  // 低电平使能

#define ANALOG_RESOLUTION 16         // 模拟测量的分辨位数
#define ANALOG_RESOLUTION_MAX 65535  // 模拟测量分辨率的最大值
#define TIME_STARTUP 100             // 使能后至采样的等待时间

#define VNAME_R_ITO "R_ITO"
#define VNAME_R_AgNW "R_AgNW"
#define VNAME_L_IN "L_IN"
#define VNAME_L_OUT "L_OUT"
#define VNAME_T_IN "T_IN"
#define VNAME_T_OUT "T_OUT"


#define SAMPLING_INTERVAL 1000

StaticJsonDocument<1024> jsonDoc; //负责格式化输出
std::vector<CtrlAccessory*> ctrlAccContainer;

PackedMAX6675 tcIn("TC_in", "T_IN",(new MAX6675(D2, D3, D4)));
PackedMAX6675 tcOut("TC_out", "T_OUT",(new MAX6675(D2, D5, D4)));
AnalogReader srITO("ITO",VNAME_R_ITO,PIN_MEASUREMENT_ITO,ANALOG_RESOLUTION,3);
AnalogReader srAgNW("AgNW",VNAME_R_AgNW,PIN_MEASUREMENT_AgNW,ANALOG_RESOLUTION,3);
AnalogReader slIn("PT_IN",VNAME_L_IN,PIN_MEASUREMENT_PTIN,ANALOG_RESOLUTION,3);
AnalogReader slOut("PT_OUT",VNAME_L_OUT,PIN_MEASUREMENT_PTOUT,ANALOG_RESOLUTION,3);

long loopCount = 0;  // 循环计数用

String jsonOutString = "";

Chrono sampleChrono;

void setup() {
  Serial.begin(115200);
  Serial.println("In Setup...");
  analogReadResolution(ANALOG_RESOLUTION);
  analogWriteResolution(ANALOG_RESOLUTION);

  //传感器压入vector
  ctrlAccContainer.push_back(&tcIn);
  ctrlAccContainer.push_back(&tcOut);
  ctrlAccContainer.push_back(&srITO);
  ctrlAccContainer.push_back(&srAgNW);
  ctrlAccContainer.push_back(&slIn);
  ctrlAccContainer.push_back(&slOut);

  pinMode(PIN_ENABLE, OUTPUT);

  Serial.println("Setup finished!");
}

void loop() {
  if (sampleChrono.hasPassed(SAMPLING_INTERVAL)) {

    /****输出内容重置****/
    sampleChrono.restart();
    jsonDoc.clear();
    jsonOutString = "";
    jsonDoc[AgentProtocol::REQ_ID_FROM_JSON] = loopCount;
    /**** 使能读取 ****/
    digitalWrite(PIN_ENABLE, SWITCH_ENABLE);  // 串联继电器连通 使能
    delay(TIME_STARTUP);
    // 读取部分
    for (int i = 0; i < ctrlAccContainer.size();i++){
      ctrlAccContainer[i]->outputStatus(&jsonDoc, true, true);
    }
    digitalWrite(PIN_ENABLE, SWITCH_DISABLE);
    /**** 使能读取结束 ****/

    
    /**** 打包输出 ****/
    serializeJson(jsonDoc, jsonOutString);
    Serial.println(jsonOutString);
    loopCount++;
  }
}