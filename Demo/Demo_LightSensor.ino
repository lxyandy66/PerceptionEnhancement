#include "Adafruit_VEML7700.h"
#include <Wire.h>
#include <PackedVEML7700.h>
TwoWire Wire2(D12,A6);//部分时候可能不可用，则尝试 TwoWire Wire2(PB4,PA7);

Adafruit_VEML7700 veml = Adafruit_VEML7700();
Adafruit_VEML7700 veml2 = Adafruit_VEML7700();

PackedVEML7700 lightSensor("ls1", "lUp", &veml2); //封装测试可用
long i = 0;
String jsonOutString = "";
StaticJsonDocument<1024> jsonDoc; //负责格式化输出

void setup() {
  Wire2.begin();
  Serial.begin(115200);
  while (!Serial) { delay(10); }
  Serial.println("Adafruit VEML7700 Test");

  if (!veml.begin()) {
    Serial.println("Sensor1 not found");
    // while (1); 
  }
  if (!veml2.begin(&Wire2)) {
    Serial.println("Sensor2 not found");
    // while (1);
  }
  Serial.println("Sensor found");

}

void loop() {

  jsonDoc.clear();
  jsonOutString = "";
  jsonDoc[AgentProtocol::REQ_ID_FROM_JSON] = i++;

  // jsonDoc["ALS1"] = veml.readALS();
  jsonDoc["LUX1"] = veml.readLux();
  // jsonDoc["ALS2"] = veml2.readALS();
  jsonDoc["LUX2"] = veml2.readLux();


  serializeJson(jsonDoc, jsonOutString);
  Serial.println(jsonOutString);

  delay(1000);
}