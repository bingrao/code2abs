@org.springframework.context.annotation.Bean
public org.springframework.batch.core.Step orderStep() {
    return stepBuilderFactory.get("processStep").<java.util.List<com.chargeback.batch.vo.ChargeBackUsage>, java.util.List<com.chargeback.batch.vo.ChargeBackUsage>>chunk(1).reader(reader()).writer(writer()).build();
}