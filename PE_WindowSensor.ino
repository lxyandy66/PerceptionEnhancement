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
#include "Adafruit_VEML7700.h"
#include <Wire.h>
#include <PackedVEML7700.h>

/**** 电路定义 ****/
// 测量引脚
#define PIN_MEASUREMENT_ITO A1
#define PIN_MEASUREMENT_AgNW A2
#define PIN_MEASUREMENT_PTIN A3
#define PIN_MEASUREMENT_PTOUT A4
// 电阻间断测量引脚
#define PIN_ENABLE D13
// 热电偶相关引脚
#define PIN_SPI_CLK D2
#define PIN_SPI_SO D10
#define PIN_CS_TIN D6
#define PIN_CS_TOUT D9
#define PIN_CS_TENV D11
// 加热器引脚
#define PIN_ENABLE_HEATING D13
// 额外I2C总线定义
#define PIN_I2C2_SDA D12
#define PIN_I2C2_SCL A6

/**** 采样定义 ****/
#define ANALOG_RESOLUTION 16         // 模拟测量的分辨位数
#define ANALOG_RESOLUTION_MAX 65535  // 模拟测量分辨率的最大值
#define TIME_STARTUP 100             // 使能后至采样的等待时间
#define SAMPLING_INTERVAL 1000
// 电阻测量使能定义
#define SWITCH_ENABLE LOW    // 低电平使能
#define SWITCH_DISABLE HIGH  // 低电平使能

/**** 变量名称定义 ****/
#define VNAME_R_ITO "R_ITO"
#define VNAME_R_AgNW "R_AgNW"
#define VNAME_L_IN "L_IN"
#define VNAME_L_OUT "L_OUT"
#define VNAME_T_IN "T_IN"
#define VNAME_T_OUT "T_OUT"
#define VNAME_T_ENV "T_ENV"



StaticJsonDocument<1024> jsonDoc; //负责格式化输出
std::vector<CtrlAccessory*> ctrlAccContainer;


TwoWire Wire2(PIN_I2C2_SDA,PIN_I2C2_SCL);

PackedMAX6675 tcIn("TC_in", VNAME_T_IN,(new MAX6675(PIN_SPI_CLK, PIN_CS_TIN, PIN_SPI_SO)));
PackedMAX6675 tcOut("TC_out", VNAME_T_OUT,(new MAX6675(PIN_SPI_CLK, PIN_CS_TOUT, PIN_SPI_SO)));
PackedMAX6675 tcEnv("TC_env", VNAME_T_ENV,(new MAX6675(PIN_SPI_CLK, PIN_CS_TENV, PIN_SPI_SO)));
AnalogReader srITO("ITO",VNAME_R_ITO,PIN_MEASUREMENT_ITO,ANALOG_RESOLUTION,3);
// AnalogReader srAgNW("AgNW",VNAME_R_AgNW,PIN_MEASUREMENT_AgNW,ANALOG_RESOLUTION,3);
// AnalogReader slIn("PT_IN",VNAME_L_IN,PIN_MEASUREMENT_PTIN,ANALOG_RESOLUTION,3);
// AnalogReader slOut("PT_OUT",VNAME_L_OUT,PIN_MEASUREMENT_PTOUT,ANALOG_RESOLUTION,3);
PackedVEML7700 slIn("PT_in", VNAME_L_IN, (new Adafruit_VEML7700()));
PackedVEML7700 slOut("PT_out", VNAME_L_OUT, (new Adafruit_VEML7700()));  // 封装测试可用

long loopCount = 0;  // 循环计数用

String jsonOutString = "";

Chrono sampleChrono;

void setup() {
  Serial.begin(115200);
  Wire2.begin();
  Serial.println("In Setup...");
  analogReadResolution(ANALOG_RESOLUTION);
  analogWriteResolution(ANALOG_RESOLUTION);

  pinMode(PIN_CS_TIN, OUTPUT);
  pinMode(PIN_CS_TOUT, OUTPUT);
  pinMode(PIN_CS_TENV, OUTPUT);
  pinMode(PIN_ENABLE, OUTPUT);

  // 初始化MAX6675热电偶模块，CS线置高
  digitalWrite(PIN_CS_TIN, HIGH);
  digitalWrite(PIN_CS_TOUT, HIGH);
  digitalWrite(PIN_CS_TENV, HIGH);

  // 光线传感器VEML7700启动
  slIn.begin();
  slOut.begin(&Wire2);

  //传感器压入vector
  ctrlAccContainer.push_back(&tcIn);
  ctrlAccContainer.push_back(&tcOut);
  ctrlAccContainer.push_back(&tcEnv);
  ctrlAccContainer.push_back(&srITO);
  // ctrlAccContainer.push_back(&srAgNW);
  // ctrlAccContainer.push_back(&slIn);
  // ctrlAccContainer.push_back(&slOut);

  digitalWrite(PIN_ENABLE, SWITCH_ENABLE);//不使能读取
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
    // digitalWrite(PIN_ENABLE, SWITCH_ENABLE);  // 串联继电器连通 使能
    // delay(TIME_STARTUP);
    // 读取部分
    for (int i = 0; i < ctrlAccContainer.size();i++){
      ctrlAccContainer[i]->outputStatus(&jsonDoc, true, true);
      delay(100);
    }
    // digitalWrite(PIN_ENABLE, SWITCH_DISABLE);
    /**** 使能读取结束 ****/

    
    /**** 打包输出 ****/
    serializeJson(jsonDoc, jsonOutString);
    Serial.println(jsonOutString);
    loopCount++;
  }
}