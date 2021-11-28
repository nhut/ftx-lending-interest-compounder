package com.github.nhut.ftxlendcompounder;

import io.contek.invoker.ftx.api.ApiFactory;
import io.contek.invoker.ftx.api.rest.spotmargin.GetLendingInfo;
import io.contek.invoker.ftx.api.rest.spotmargin.LendingInfo;
import io.contek.invoker.ftx.api.rest.spotmargin.LendingOffer;
import io.contek.invoker.ftx.api.rest.spotmargin.PostLendingOffer;
import io.contek.invoker.ftx.api.rest.spotmargin.SpotMarginApi;
import io.contek.invoker.security.ApiKey;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import java.math.BigDecimal;

@Slf4j
@Service
public class CompoundInterestServiceImpl implements CompoundInterestService {

    private final SpotMarginApi spotMarginApi;

    public CompoundInterestServiceImpl(@Value("${app.ftx.api.key}") final String apiKey,
                                       @Value("${app.ftx.api.secret}") final String apiSecret) {
        if (!StringUtils.hasLength(apiKey)) {
            throw new IllegalArgumentException("Required app.ftx.api.key value is missing!");
        }
        if (!StringUtils.hasLength(apiSecret)) {
            throw new IllegalArgumentException("Required app.ftx.api.secret value is missing!");
        }
        final ApiKey apiKeyData = ApiKey.newBuilder().setId(apiKey).setSecret(apiSecret).build();
        this.spotMarginApi = ApiFactory.getMainNet().rest().spotMargin(apiKeyData);
    }

    public void reLendAgainWithEarnedInterests() {
        log.debug("Check current lending...");
        final GetLendingInfo lendingInfo = spotMarginApi.getLendingInfo();
        final GetLendingInfo.Response lendingInfoResponse = lendingInfo.submit();
        log.debug("Current lending:\n{}", lendingInfoResponse);

        for (LendingInfo currentLendingInfo : lendingInfoResponse.result) {
            if (currentLendingInfo.lendable < 0.000001) {
                log.debug("Skipping {}... {} is too low for lending.", currentLendingInfo.coin, currentLendingInfo.lendable);
                return;
            }

            final LendingOffer newLendingOffer = new LendingOffer();
            newLendingOffer.coin = currentLendingInfo.coin;
            newLendingOffer.rate = currentLendingInfo.minRate;
            newLendingOffer.size = BigDecimal.valueOf(currentLendingInfo.lendable).setScale(5, BigDecimal.ROUND_DOWN).doubleValue();
            final PostLendingOffer postLendingOffer = spotMarginApi.postLendingOffer(newLendingOffer);
            try {
                final PostLendingOffer.Response submit = postLendingOffer.submit();
                if (Boolean.TRUE.equals(submit.success)) {
                    log.info("Lending {} success. Result: {}", newLendingOffer.coin, submit.result);
                } else {
                    log.error("Lending {} FAILED. Result: {}", newLendingOffer.coin, submit.result);
                }

            } catch (Exception e) {
                log.error("Fetch to lend {} with size of {}.", newLendingOffer.coin, newLendingOffer.size, e);
                throw e;
            }
        }
    }
}
