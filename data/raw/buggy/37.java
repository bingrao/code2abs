public void meleeAttack(java.util.ArrayList<scrublords.entities.enemies.Enemy> enemies) {
    for (scrublords.entities.enemies.Enemy enemy : enemies) {
        if (character.attacking) {
            if (facingRight) {
                attackRightEnemy(enemy);
            }
            attackLeftEnemy(enemy);
        }
    }
}