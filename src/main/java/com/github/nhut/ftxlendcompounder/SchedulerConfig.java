package com.github.nhut.ftxlendcompounder;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

import javax.annotation.PostConstruct;

@Configuration
@EnableScheduling
@ConditionalOnProperty(name = "scheduler.enabled", matchIfMissing = true)
public class SchedulerConfig {

    private final CompoundInterestService compoundInterestService;

    @Autowired
    public SchedulerConfig(final CompoundInterestService compoundInterestService) {
        this.compoundInterestService = compoundInterestService;
    }

    @PostConstruct
    public void runAfterCreated() {
        compoundLendingInterestScheduler();
    }

    @Scheduled(cron = "0 5 * ? * *") //Gets trigger at every hour and 5 minute time.
    public void compoundLendingInterestScheduler() {
        compoundInterestService.reLendAgainWithEarnedInterests();
    }
}
