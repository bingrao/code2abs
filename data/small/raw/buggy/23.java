@java.lang.Override
public void actionPerformed(java.awt.event.ActionEvent e) {
    editDraftBeer(draftBeerMasterSortedList.get(bottledBeerMasterListJTable.getSelectedRow()));
    draftBeerMasterSortedList.remove(bottledBeerMasterListJTable.getSelectedRow());
}