public static void writeOnOffState(boolean onOff) {
    if (onOff)
        es.carlosrolindez.kbfinder.SelectBtActivity.sendSppMessage("STB ON\r", true);
    else
        es.carlosrolindez.kbfinder.SelectBtActivity.sendSppMessage("STB OFF\r", true);
    
}