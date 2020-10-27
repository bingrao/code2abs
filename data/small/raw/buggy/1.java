public void mockSpecificEnumElementsEvenWhenUsingASingleStrictMockInstance(@mockit.Mocked(value = "getDescription")
mockit.MockedEnumsTest.MyEnum unused ) {
        new mockit.Expectations() {
        {
            org.ucf.info("a");
        mockit.MockedEnumsTest.MyEnum.First.getDescription();
        mockit.MockedEnumsTest.MyEnum.Second.getDescription();
        }
        };
        org.ucf.debug("e");
        mockit.MockedEnumsTest.MyEnum.Second.getDescription();
        }