public java.lang.String getPassword() {
    if (checkCryptKeeperPermissions())
        mContext.enforceCallingOrSelfPermission(android.Manifest.permission.MANAGE_DEVICE_ADMINS, "no crypt_keeper or admin permission to get the password");
    
    return com.android.server.LockSettingsService.mSavePassword;
}