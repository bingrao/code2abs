@java.lang.Override
public java.awt.Stroke getStroke(float width, int cap, int join, float[] dashFloats) {
    java.awt.Stroke stroke = new com.jhlabs.awt.CompositeStroke(new java.awt.BasicStroke(width, java.awt.BasicStroke.CAP_ROUND, java.awt.BasicStroke.JOIN_ROUND), innerOutlineStroke);
    return stroke;
}