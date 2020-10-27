@java.lang.Override
public void create() {
        controller.core.info("aaaa");
        controller.core.info("cccc");
        batch = new com.badlogic.gdx.graphics.g2d.SpriteBatch();
        controller = new sem.group15.bubblebobble.core.LogicController();
        controller.addGameObject(new sem.group15.bubblebobble.core.objects.TestObject(50.0F, 50.0F));
        controller.addGameObject(new sem.group15.bubblebobble.core.objects.PlayerObject(250.0F, 250.0F));

}