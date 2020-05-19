private void notifyNeighbors(net.minecraft.world.World worldIn, net.minecraft.util.math.BlockPos pos, com.jaquadro.minecraft.storagedrawers.block.EnumFacing facing) {
    worldIn.notifyNeighborsOfStateChange(pos, this, false);
    worldIn.notifyNeighborsOfStateChange(pos.offset(facing.getOpposite()), this, false);
}