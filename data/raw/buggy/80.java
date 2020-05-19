public void actionPerformed(java.awt.event.ActionEvent ae) {
    try {
        SerialPortReader.serialPort.closePort();
    } catch (jssc.SerialPortException e) {
        e.printStackTrace();
    }
    SerialPortReader.view.clear();
}