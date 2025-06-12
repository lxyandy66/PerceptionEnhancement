// 本脚本用于测试电流换向的分压法测电阻
// Vcc与GND均通过模拟输出引脚实现，并在loop函数中实现换向

boolean isForward = true;

long readValue = 0; 

void setup(){
    Serial.begin(115200);
    Serial.println("In Setup...");
    analogReadResolution(16);
    analogWriteResolution(16);
    Serial.println("Setup finished!");
}

void loop(){
    Serial.println("in loop");
    // 确定输入输出的方向
    pinMode(A4, OUTPUT);
    pinMode(A5, OUTPUT);

    analogWrite(isForward ? A4 : A5, 65535);
    analogWrite(isForward ? A5 : A4, 0);

    Serial.print("isForward: ");
    Serial.println(isForward);

    // readValue = analogRead(isForward ? A4 : A5);
    // Serial.println(readValue);

    isForward = !isForward;

    delay(2000);
}