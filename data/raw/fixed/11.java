private void forwardFailures(java.lang.Throwable throwable) {
    for (io.crate.executor.transport.distributed.DistributingDownstream.Downstream downstream : downstreams) {
        downstream.sendRequest(throwable);
    }
}