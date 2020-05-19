@java.lang.Override
public void onItemClick(java.lang.String id) {
    android.content.Intent i = new android.content.Intent();
    i.setClass(getActivity(), edu.mobapde.bloodnet.MyPledgeActivity.class);
    i.putExtra("id", id);
    startActivity(i);
}