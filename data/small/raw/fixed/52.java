public void resetLevelColors(final boolean enabled) {
    synchronized(buffer) {
        if (enabled) {
            resetLevelColors();
        }else {
            colors = null;
            colorLevels = null;
            colorSequences = null;
        }
    }
}