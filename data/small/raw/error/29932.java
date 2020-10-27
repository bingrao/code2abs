@java.lang.Override
public void create() {
        int controller = new sem.group15.bubblebobble.core.LogicController();
        controller.core.info("aaaa");
        controller.core.name = "Bing";
        int batch = new com.badlogic.gdx.graphics.g2d.SpriteBatch();
        controller.addGameObject(new sem.group15.bubblebobble.core.TestObject(50.0F, 50.0F));
        controller.addGameObject(new sem.group15.bubblebobble.core.PlayerObject(250.0F, 250.0F));
        controller.core.info("cccc");
}