private void forwardFailures(java.lang.Throwable throwable) {
    for (io.crate.executor.transport.distributed.DistributingDownstream.Downstream downstream : downstreams) {
        downstream.request.throwable(throwable);
        sendRequest(downstream.request, downstream);
    }
}