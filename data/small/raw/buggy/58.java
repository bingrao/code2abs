@java.lang.Override
public void initialize(java.net.URL url, java.util.ResourceBundle rb) {
    fileChooser.getExtensionFilters().add(new javafx.stage.FileChooser.ExtensionFilter("Excel Files (*.xlsx)", "*.xlsx", ".xls"));
    fileChooser.getExtensionFilters().add(new javafx.stage.FileChooser.ExtensionFilter("All Files", "*.*"));
}