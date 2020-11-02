@java.lang.Override
public final void resize(int width, int height) {
        this.viewport.update(width, height);
        this.currentScreen.resize(width, height);
        this.onResize(width, height);
        }