package com.github.nhut.ftxlendcompounder;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.util.StringUtils;

import javax.annotation.PostConstruct;

@Configuration
@EnableScheduling
@ConditionalOnProperty(name = "scheduler.enabled", matchIfMissing = true)
public class SchedulerConfig {

    private boolean callCompoundInterestSchedulerAtStartup = false;

    private final CompoundInterestService compoundInterestService;

    @Autowired
    public SchedulerConfig(final CompoundInterestService compoundInterestService, @Value("${app.ftx.startup.call-compound-interest}") String callCompoundInterestSchedulerAtStartupS) {
        this.compoundInterestService = compoundInterestService;
        this.callCompoundInterestSchedulerAtStartup =
                StringUtils.hasLength(callCompoundInterestSchedulerAtStartupS) && Boolean.parseBoolean(callCompoundInterestSchedulerAtStartupS);
    }

    @PostConstruct
    public void postConstruct() {
        if (callCompoundInterestSchedulerAtStartup) {
            compoundLendingInterestScheduler();
        }
    }

    @Scheduled(cron = "0 5 * ? * *") //Gets trigger at every hour and 5 minute time.
    public void compoundLendingInterestScheduler() {
        compoundInterestService.reLendAgainWithEarnedInterests();
    }
}
