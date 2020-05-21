public void setStone(com.peterverzijl.softwaresystems.qwirkle.Node node) {
    setPlacedBlock(node);
    setFrontier(mSetBlocks.get(((mSetBlocks.size()) - 1)));
}