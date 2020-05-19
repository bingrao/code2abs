public void toggleMute() {
    if (!(project.equationinvasion.Audio.muted)) {
        stopMusic();
        project.equationinvasion.Audio.muted = true;
    }else {
        project.equationinvasion.Audio.muted = false;
        menuBGM();
    }
}