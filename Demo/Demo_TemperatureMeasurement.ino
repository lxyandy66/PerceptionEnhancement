// 通过热电偶+MAX6675模块测温度

#include<max6675.h>

MAX6675 thermalCouple(D2,D3,D4); //SCLK,CS,MISO
float temperature = 0;
void setup() {
    //dudu
    Serial.begin(115200);
    Serial.println("Setup finished!");
}

void loop(){
    //dd
    // Serial.println("in loop");
    temperature = thermalCouple.readCelsius();
    Serial.println(temperature, 3);

    // For the MAX6675 to update, you must delay AT LEAST 250ms between reads!
    delay(1000);
}