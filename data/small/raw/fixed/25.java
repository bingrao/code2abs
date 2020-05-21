@java.lang.Override
public void onSucces(com.evernote.edam.type.Note result) {
    recycleview.scrollToPosition(0);
    arrayNotes.add(0, result);
    adapter.animateTo(arrayNotes);
}