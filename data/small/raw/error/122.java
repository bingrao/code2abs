public void reveal(@android.support.annotation.Size(value = 2) @android.support.annotation.NonNull final int[] from) {
    if (changeState(de.wackernagel.android.sidekick.widgets.CircularRevealView.STATE_REVEAL_STARTED))
    {
        animationState = de.wackernagel.android.sidekick.widgets.CircularRevealView.ANIMATION_STARTING;
        circleX = from[0];
        circleY = from[1];
        android.support.v4.view.ViewCompat.postInvalidateOnAnimation(this);
    }
}