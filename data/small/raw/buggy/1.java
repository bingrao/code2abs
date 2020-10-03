@org.junit.Test
public void testClassWideAttributeInAnonymousClass() throws java.lang.Throwable {
@com.orhanobut.tracklytics.FixedAttribute(key = "key1", value = "value1")
class Foo {
    @com.orhanobut.tracklytics.FixedAttribute(key = "key2", value = "value2")
    class Inner {
        @com.orhanobut.tracklytics.TrackEvent(value = "title")
        public void bar() {
        }
    }
}
    invokeMethod(Foo.Inner.class, "bar");
    com.orhanobut.tracklytics.AssertTracker.assertTrack(tracker).event("title").noTags().attribute("key1", "value1").attribute("key2", "value2").noSuperAttributes().noFilters();
            }