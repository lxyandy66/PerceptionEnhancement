// 用于简单的JSON输出

#pragma once

#include <ArduinoJson.h>

class SimplePacketOutput {
    private:
        StaticJsonDocument<512> jsonDoc;
    public:
};