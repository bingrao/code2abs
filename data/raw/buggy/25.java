@java.lang.Override
public void onSucces(com.evernote.edam.type.Note result) {
    android.util.Log.e("NOTECREATED", result.toString());
    recycleview.scrollToPosition(0);
    arrayNotes.add(0, result);
    adapter.animateTo(arrayNotes);
}