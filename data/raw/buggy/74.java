public void toggleMute() {
    if (!(project.equationinvasion.Audio.muted)) {
        project.equationinvasion.Audio.muted = true;
        stopMusic();
    }else {
        project.equationinvasion.Audio.muted = false;
        menuBGM();
    }
}