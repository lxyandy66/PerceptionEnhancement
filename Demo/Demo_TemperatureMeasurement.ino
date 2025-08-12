// 通过热电偶+MAX6675模块测温度
//MAX31855这个芯片是真的没用成过

#include <Adafruit_MAX31855.h>
#include <max6675.h>

// MAX6675 thermalCouple(D2, D3, D4);             // SCLK,CS,MISO
Adafruit_MAX31855 thermalCouple2(D2, D5, D4);  // SCLK,CS,MISO
float temperature = 0;
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
  delay(500);
  Serial.print("MAX31855: ");
  Serial.println(thermalCouple2.readCelsius(), 3);

  // For the MAX6675 to update, you must delay AT LEAST 250ms between reads!
  delay(1000);
}