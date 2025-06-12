// 测量电阻的demo
// 通过分压法测TCW电阻

// 基本设置参数
// int SET_BaudRate = 115200;

double REF_Resistance = 440; // 已知的参考电阻，此处用1k+330+220 //连线后1338 ohms unknow 887 ohms
double REF_Volt = 3.3;        // 已知的总电压
int dirCounter = 2;

const static long RESOLUTION_MAX = 65535;
const static long RESOLUTION_BIT = 16;

double measurementResist(long in, double refR, long refV)
{
    if (in > 0)
        return refR * in / (refV - in); //refV为总压降
    else
        return -1;
}

long readValueA0 = 0;
long readValueA1 = 0;
long readValueA2 = 0;
long readValueA3 = 0;
// double estResistance = -1;

void setup()
{
    Serial.println("Setup finished!");
    Serial.begin(115200);

    pinMode(A1, INPUT);
    // A4 A4设为可切换的电源正负极
    pinMode(A4, OUTPUT);
    pinMode(A5, OUTPUT);

    analogReadResolution(RESOLUTION_BIT);
    analogWriteResolution(RESOLUTION_BIT);

    Serial.println("Setup finished!");
    delay(500);
}

void loop()
{
    // if(Seari)
    Serial.println(dirCounter);

    // 电源正负极切换
    switch (dirCounter)
    {
    case 2:
    case 4:
        /* code */
        pinMode(A4, INPUT);
        pinMode(A5, INPUT);
        // analogWrite(A4, 0);
        // analogWrite(A5, 0);
        if (dirCounter == 4)
            dirCounter = 1;
        else
            dirCounter++;
        delay(3000);
        break;
    default:
        /* code */
        pinMode(A4, OUTPUT);
        pinMode(A5, OUTPUT);
        analogWrite(dirCounter == 1 ? A4 : A5, 0);
        analogWrite(dirCounter == 1 ? A5 : A4, RESOLUTION_MAX);

        delay(1000);

        // 切记 此处电流切换方向之后，压降是有先后顺序的
        // A0-A2总压降，A1-A3待测电阻的压降
        readValueA0 = analogRead(A0);
        readValueA1 = analogRead(A1);
        readValueA2 = analogRead(A2);
        readValueA3 = analogRead(A3);
        Serial.printf("Current read A0: %d\tA1: %d\tA2: %d\n",readValueA0,readValueA1,readValueA2);
        Serial.printf("Current read: %d\n",abs(readValueA1-readValueA3));
        // estResistance = ;
        // Serial.print("Est.R: ");
        Serial.println(measurementResist((abs(readValueA1-readValueA3)),
                                         REF_Resistance, abs(readValueA0-readValueA2)));

        Serial.print("Current state: ");
        Serial.println(dirCounter); // 显示电源方向

        pinMode(A4, INPUT);
        pinMode(A5, INPUT);
        dirCounter++;
        break;
    }

    delay(1000);
}
