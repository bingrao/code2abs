public static bl.battlefield.Move GetInstance(bl.battlefield.Pokemon pokemon, int i) {
    if (bl.move.MoveFactory.firstUse) {
        bl.move.MoveFactory.listMove.add(new bl.move.Tackle(pokemon));
        bl.move.MoveFactory.firstUse = !(bl.move.MoveFactory.firstUse);
    }
    return bl.move.MoveFactory.listMove.get(i);
}