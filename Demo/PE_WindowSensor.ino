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
#include<Chrono.h>

/****电路定义****/
#define PIN_MEASUREMENT_ITO A1
#define PIN_MEASUREMENT_AgNW A2
#define PIN_MEASUREMENT_PTIN A3
#define PIN_MEASUREMENT_PTOUT A4
#define PIN_ENABLE D10
#define SWITCH_ENABLE LOW    // 低电平使能
#define SWITCH_DISABLE HIGH  // 低电平使能

#define ANALOG_RESOLUTION 16         // 模拟测量的分辨位数
#define ANALOG_RESOLUTION_MAX 65535  // 模拟测量分辨率的最大值
#define TIME_STARTUP 100             // 使能后至采样的等待时间

#define SAMPLING_INTERVAL 1000

StaticJsonDocument<1024> jsonDoc; //负责格式化输出

ResistanceMapper rMapper(0,"M_ITO");
MAX6675 thermalCouple_in(D2,D3,D4); //SCLK,CS,MISO
MAX6675 thermalCouple_out(D2,D3,D4); //SCLK,CS,MISO

double temperture = -999;

long loopCount = 0;  // 循环计数用
long readWinSensor_ITO = 0;
long readWinSensor_AgNW = 0;
long readPtSensor_in = 0;
long readPtSensor_out = 0;
String temp = "";

Chrono sampleChrono;

void setup() {
  Serial.begin(115200);
  Serial.println("In Setup...");
  analogReadResolution(ANALOG_RESOLUTION);
  analogWriteResolution(ANALOG_RESOLUTION);

  pinMode(PIN_ENABLE, OUTPUT);

  Serial.println("Setup finished!");
}

void loop() {

  
  if (sampleChrono.hasPassed(SAMPLING_INTERVAL)) {

    /****输出内容重置****/
    sampleChrono.restart();
    jsonDoc.clear();
    temp = "";

    /**** 使能读取 ****/
    digitalWrite(PIN_ENABLE, SWITCH_ENABLE);  // 串联继电器连通 使能
    delay(TIME_STARTUP);
    // 读取
    readWinSensor_ITO = analogRead(PIN_MEASUREMENT_ITO);
    readWinSensor_AgNW = analogRead(PIN_MEASUREMENT_AgNW);
    // 读取部分
    digitalWrite(PIN_ENABLE, SWITCH_DISABLE);
    /**** 使能读取结束 ****/
    
    /**** 模拟量读取 ****/
    readPtSensor_in = analogRead(PIN_MEASUREMENT_PTIN);
    readPtSensor_out = analogRead(PIN_MEASUREMENT_PTOUT);
    
    /**** 温度读取 ****/
    /**** 打包输出 ****/
    jsonDoc[AgentProtocol::DEV_ID_FROM_JSON] = "win"; //整个的窗户名
    jsonDoc[AgentProtocol::REQ_ID_FROM_JSON] = loopCount;

    jsonDoc["R_ITO"] = readWinSensor_ITO;
    jsonDoc["R_AgNW"] = readWinSensor_AgNW;
    jsonDoc["L_IN"] = readPtSensor_in;
    jsonDoc["L_OUT"] = readPtSensor_out;
    jsonDoc["T_IN"] = thermalCouple_in.readCelsius();
    jsonDoc["T_OUT"] = thermalCouple_out.readCelsius();

    delay(1000);

    serializeJson(jsonDoc, temp);
    Serial.println(temp);
    loopCount++;
  }
}