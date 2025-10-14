#define PIN_ENABLE D10
#define SWITCH_ENABLE LOW    // 低电平使能
#define SWITCH_DISABLE HIGH  // 低电平使能

void setup(){
    Serial.begin(115200);
    Serial.println("In Setup...");
    pinMode(PIN_ENABLE, OUTPUT);
    
  analogReadResolution(ANALOG_RESOLUTION);
  analogWriteResolution(ANALOG_RESOLUTION);

  Serial.println("Setup finished!");
}

void loop(){

}