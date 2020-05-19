public synchronized void resetLevelColors(final boolean enabled) {
    if (enabled) {
        resetLevelColors();
    }else {
        colors = null;
        colorLevels = null;
        colorSequences = null;
    }
}