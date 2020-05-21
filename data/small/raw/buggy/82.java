private void MoveUp() {
    int ix = mProperty.x;
    int iy = mProperty.y;
    if (!(isEdge(ix, iy, CRobot.NORTH)));
    {
        iy++;
    }
    Place(ix, iy, CRobot.NORTH);
}