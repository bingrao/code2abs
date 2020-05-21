private void jBtnCardRightActionPerformed(java.awt.event.ActionEvent evt) {
    int count = m_durakClient.getM_cards().size();
    if (((m_currCardInd) + 1) < (count - 6)) {
        (m_currCardInd)++;
        drawUserCards();
    }
}