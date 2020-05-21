public void meleeAttack(scrublords.entities.enemies.Enemy enemy) {
    if (character.attacking) {
        if (facingRight) {
            attackRightEnemy(enemy);
        }
        attackLeftEnemy(enemy);
    }
}