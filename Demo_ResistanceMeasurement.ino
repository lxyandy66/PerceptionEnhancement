// 测量电阻的demo
// 通过分压法测TCW电阻

// 基本设置参数
// int SET_BaudRate = 115200;

double REF_Resistance = 1538; // 已知的参考电阻，此处用1k+330+220 //连线后1338 ohms unknow 887 ohms
double REF_Volt = 3.3; //已知的总电压


double measurementResist(long in,double refR,double refV){
    if(in>0)
        return refR * in /(65536-in);
    else
        return -1;
}

long readValue = 0;
// double estResistance = -1;

void setup(){
    Serial.println("Setup finished!");
    Serial.begin(115200);
    pinMode(A1, INPUT);
    analogReadResolution(16);
    Serial.println("Setup finished!");
    delay(500);
}

void loop(){
    // if(Seari)
    // Serial.println("in loop");
    readValue = analogRead(A1);
    // Serial.printf("Current read: %d\n",readValue);
    // estResistance = ;
    Serial.print("Est. resistance (ohm):");
    Serial.println(measurementResist(readValue, REF_Resistance, REF_Volt));
    delay(1000);
}

