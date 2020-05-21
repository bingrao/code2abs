public float timeMidRender() {
    return ((timeTick) + (stratos.game.common.PlayLoop.frameTime())) / (stratos.game.common.Stage.UPDATES_PER_SECOND);
}