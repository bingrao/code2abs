public void createPDF(GUI.GUISETTINGSPDF erster, java.lang.String headline, int num) {
    GUI.PDFGEN pdfGenerator = new GUI.PDFGEN();
    pdfGenerator.PDFAB(headline, num, erster, false);
}