// 测量电阻的demo
// 通过分压法测TCW电阻

// 基本设置参数
// int SET_BaudRate = 115200;

#include<max6675.h>

double REF_Resistance = 2000; // 已知的参考电阻，此处用1k+330+220 //连线后1338 ohms unknow 887 ohms
double REF_Volt = 3.3;        // 已知的总电压
int dirCounter = 2;

const static long RESOLUTION_MAX = 65535;
const static long RESOLUTION_BIT = 16;


MAX6675 thermalCouple(D2,D3,D4); //SCLK,CS,MISO
float temperature1 = 0;
float temperature2 = 0;

double measurementResist(long in,double refR,double refV){
    if(in>0)
        return refR * in /(RESOLUTION_MAX-in);
    else
        return -1;
}

long readValuePos = 0;
// double estResistance = -1;

void setup(){
    Serial.println("Setup finished!");
    Serial.begin(115200);
    pinMode(A1, INPUT);
    pinMode(A0, INPUT);
    analogReadResolution(RESOLUTION_BIT);
    Serial.println("Setup finished!");
    delay(500);
}

void loop(){
    // if(Seari)
    // Serial.println("in loop");
    temperature1 = thermalCouple.readCelsius();
    delay(500);
    readValuePos = analogRead(A0);
    temperature2 = thermalCouple.readCelsius();
    Serial.printf("%d\t",readValuePos);
    Serial.println((temperature1 + temperature2)/2.0, 2);
    // estResistance = ;
    // Serial.print("Est. resistance (ohm):");
    Serial.println(measurementResist(readValuePos, REF_Resistance, REF_Volt));
    delay(500);
}

