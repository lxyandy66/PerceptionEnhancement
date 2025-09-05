// 本脚本用于测试电流换向的分压法测电阻，包括
// Vcc与GND均通过模拟输出引脚实现，并在loop函数中实现换向
#pragma once

#include <Chrono.h>
#include<max6675.h>

MAX6675 thermalCouple(D2,D3,D4); //SCLK,CS,MISO

#define PIN_MEASUREMENT A1
#define PIN_ENABLE D10
#define SWITCH_ENABLE LOW    // 低电平使能
#define SWITCH_DISABLE HIGH  // 低电平使能

#define ANALOG_RESOLUTION 16         // 模拟测量的分辨位数
#define ANALOG_RESOLUTION_MAX 65535  // 模拟测量分辨率的最大值

#define TIME_STARTUP 250  // 采样使能后到采样的时间间隔

Chrono sampleChrono;
long readValue = 0;

const int SAMPLING_INTERVAL = 1000;



void setup() {
  Serial.begin(115200);
  Serial.println("In Setup...");
  analogReadResolution(ANALOG_RESOLUTION);
  analogWriteResolution(ANALOG_RESOLUTION);
  Serial.println("Setup finished!");
}

void loop() {
  // 确定输入输出的方向
  pinMode(A4, OUTPUT);
  pinMode(A5, OUTPUT);

  pinMode(PIN_ENABLE, OUTPUT);

  if (sampleChrono.hasPassed(SAMPLING_INTERVAL)) {
    sampleChrono.restart();
    digitalWrite(PIN_ENABLE, SWITCH_ENABLE);  // 串联继电器连通 使能
    delay(TIME_STARTUP);
    // 读取
    readValue = analogRead(PIN_MEASUREMENT);
    // 读取部分

    digitalWrite(PIN_ENABLE, SWITCH_DISABLE);

    Serial.println(readValue);
  }
}