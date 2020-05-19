@tv.controllers.RequestMapping(value = "/tv/{id}/records/current", method = RequestMethod.GET)
private tv.controllers.TVChannelObject getCurrentChannel(@tv.controllers.PathVariable(value = "id")
java.lang.String id) {
    for (tv.controllers.TVChannelObject o : tvChannelObjectRepository.findAll()) {
        return o;
    }
    return new tv.controllers.TVChannelObject();
}