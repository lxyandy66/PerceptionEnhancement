// 本脚本用于测试电流换向的分压法测电阻
// Vcc与GND均通过模拟输出引脚实现，并在loop函数中实现换向

boolean isForward = true;

long readValue = 0; 

void setup(){
    Serial.begin(115200);
    Serial.println("In Setup...");
    analogReadResolution(16);
    analogWriteResolution(6);
    Serial.println("Setup finished!");
}

void loop(){
    Serial.println("in loop");
    // 确定输入输出的方向
    pinMode(A3, OUTPUT);
    pinMode(A4, OUTPUT);

    analogWrite(isForward ? A3 : A4, 65535);
    analogWrite(isForward ? A4 : A3, 0);

    Serial.print("isForward: ");
    Serial.println(isForward);

    // readValue = analogRead(isForward ? A3 : A4);
    // Serial.println(readValue);

    isForward = !isForward;

    delay(2000);
}