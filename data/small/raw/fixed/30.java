private com.google.gwt.maps.client.base.Point castToGooglPoint(com.google.gwt.maps.client.base.org.wwarn.mapcore.client.components.customwidgets.map.Point p) {
    if (p == null) {
        return null;
    }
    return com.google.gwt.maps.client.base.Point.newInstance(p.getX(), p.getY());
}