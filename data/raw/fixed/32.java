public static net.sf.jnati.deploy.artefact.Artefact loadLibrary(java.lang.String id, java.lang.String version, java.util.Properties configuration) throws net.sf.jnati.NativeCodeException {
    return net.sf.jnati.deploy.NativeLibraryLoader.loadLibrary(id, version, configuration, null);
}