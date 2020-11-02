public void onClick(android.view.View v) {
        android.widget.CheckBox cb = ((android.widget.CheckBox) (v));
        if (cb.isChecked()) {
        cb.setText("Avklarat?");
        }else {
        cb.setText("Avklarat!");
        }
        }