@org.junit.Test(expected = java.lang.IllegalArgumentException.class)
public void test_CreateUser_WithoutPassword() {
    s = Subscriber.SubscriberFactory.createSubsriber("Beathe", "", TerminalType.PearaPhone4s, new SubscriptionType.GreenMobileS());
}