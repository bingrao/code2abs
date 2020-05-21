public boolean setStone(com.peterverzijl.softwaresystems.qwirkle.Node node) {
    boolean set = setPlacedBlock(node);
    if (set)
        setFrontier(mSetBlocks.get(((mSetBlocks.size()) - 1)));
    
    return set;
}