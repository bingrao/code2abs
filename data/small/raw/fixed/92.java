public static bl.battlefield.Move GetInstance(bl.battlefield.Pokemon pokemon, int i) {
    if (bl.move.MoveFactory.firstUse) {
        bl.move.MoveFactory.firstUse = !(bl.move.MoveFactory.firstUse);
    }
    return new bl.move.Tackle(pokemon);
}