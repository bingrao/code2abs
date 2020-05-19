public gr.cite.earthserver.wcs.core.WCSRequest build() {
    return new gr.cite.earthserver.wcs.core.WCSRequest(this.webTarget.queryParam("query", org.glassfish.jersey.uri.UriComponent.encode(query, Type.QUERY_PARAM_SPACE_ENCODED)));
}