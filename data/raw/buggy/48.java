@tv.controllers.RequestMapping(value = "/tv/{id}/records/current", method = RequestMethod.GET)
private tv.controllers.TVChannelObject getCurrentChannel(@tv.controllers.PathVariable(value = "id")
java.lang.String id) {
    java.lang.System.out.println((("Received Message : " + " id = ") + id));
    java.lang.System.out.println("Replied Message: 200 (OK)");
    for (tv.controllers.TVChannelObject o : tvChannelObjectRepository.findAll()) {
        return o;
    }
    return new tv.controllers.TVChannelObject();
}