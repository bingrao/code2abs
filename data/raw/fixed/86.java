@java.lang.Override
public void setMuleContext(org.mule.api.MuleContext context) {
    for (org.mule.api.processor.MessageProcessor processor : processors) {
        if (processor instanceof org.mule.api.context.MuleContextAware) {
            ((org.mule.api.context.MuleContextAware) (processor)).setMuleContext(context);
        }
    }
}