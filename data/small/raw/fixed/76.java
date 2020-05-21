public void setDescription(java.lang.String description) {
    this.description = description;
    sharedPreference.saveString(mContext, SharedPreference.KEY_DESC, description);
}