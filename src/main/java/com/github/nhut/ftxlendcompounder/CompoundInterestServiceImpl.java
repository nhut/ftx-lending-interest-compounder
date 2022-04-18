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
import java.math.RoundingMode;
import java.util.Collections;
import java.util.List;

@Slf4j
@Service
public class CompoundInterestServiceImpl implements CompoundInterestService {

    private static final int FTX_OFFERED_LENDABLE_VALUE_DECIMALS = 5;

    private final SpotMarginApi spotMarginApi;
    private final List<String> coinsToExclude;

    public CompoundInterestServiceImpl(@Value("${app.ftx.api.key}") final String apiKey,
                                       @Value("${app.ftx.api.secret}") final String apiSecret,
                                       @Value("${app.ftx.lending.exclude.coins}") final List<String> coinsToExclude) {
        if (!StringUtils.hasLength(apiKey)) {
            throw new IllegalArgumentException("Required app.ftx.api.key value is missing!");
        }
        if (!StringUtils.hasLength(apiSecret)) {
            throw new IllegalArgumentException("Required app.ftx.api.secret value is missing!");
        }
        final ApiKey apiKeyData = ApiKey.newBuilder().setId(apiKey).setSecret(apiSecret).build();
        this.spotMarginApi = ApiFactory.getMainNet().rest().spotMargin(apiKeyData);
        this.coinsToExclude = coinsToExclude != null ? coinsToExclude : Collections.emptyList();
    }

    public void reLendAgainWithEarnedInterests() {
        log.debug("Checking current lending...");
        final GetLendingInfo.Response lendingInfoResponse = spotMarginApi.getLendingInfo().submit();
        if (Boolean.FALSE.equals(lendingInfoResponse.success)) {
            log.error("Failed to get lending info. {}", lendingInfoResponse.result);
            return;
        }

        for (LendingInfo currentLendingInfo : lendingInfoResponse.result) {
            if (coinsToExclude.contains(currentLendingInfo.coin) || currentLendingInfo.offered <= 0) {
                continue;
            }
            final BigDecimal offered = BigDecimal.valueOf(currentLendingInfo.offered).setScale(FTX_OFFERED_LENDABLE_VALUE_DECIMALS, RoundingMode.FLOOR);
            final BigDecimal lendable = BigDecimal.valueOf(currentLendingInfo.lendable).setScale(FTX_OFFERED_LENDABLE_VALUE_DECIMALS, RoundingMode.FLOOR);
            final boolean isLendingAmountIsUnchanged = offered.compareTo(lendable) == 0;
            if (isLendingAmountIsUnchanged) {
                continue;
            }
            sendNewLendingOfferToFtx(currentLendingInfo);
        }
    }

    private void sendNewLendingOfferToFtx(final LendingInfo currentLendingInfo) {
        final LendingOffer newLendingOffer = new LendingOffer();
        newLendingOffer.coin = currentLendingInfo.coin;
        newLendingOffer.rate = currentLendingInfo.minRate;
        newLendingOffer.size = BigDecimal.valueOf(currentLendingInfo.lendable)
                .setScale(FTX_OFFERED_LENDABLE_VALUE_DECIMALS, RoundingMode.FLOOR).doubleValue();
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
