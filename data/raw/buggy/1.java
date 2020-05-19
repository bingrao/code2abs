public java.util.List<com.googlecode.mp4parser.authoring.Sample> getSamples() {
    java.util.ArrayList<com.googlecode.mp4parser.authoring.Sample> lists = new java.util.ArrayList<com.googlecode.mp4parser.authoring.Sample>();
    for (com.googlecode.mp4parser.authoring.Track track : tracks) {
        lists.addAll(track.getSamples());
    }
    return lists;
}