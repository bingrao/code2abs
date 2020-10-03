public void clearValues() {
        if (null == (propertyTable))
        throw new java.lang.IllegalStateException("You need to call asWidget() before clearing the values");

        propertyProvider.setList(new java.util.ArrayList<org.jboss.as.console.client.shared.properties.PropertyRecord>());
        setEnabled(false);
        }