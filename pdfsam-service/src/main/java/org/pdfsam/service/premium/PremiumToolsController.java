/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 23 nov 2016
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as 
 * published by the Free Software Foundation, either version 3 of the 
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.service.premium;

import jakarta.inject.Inject;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.injector.Auto;
import org.pdfsam.model.premium.FetchPremiumModulesRequest;
import org.pdfsam.model.premium.PremiumToolsResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.CompletableFuture;

import static java.util.Objects.nonNull;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * @author Andrea Vacondio
 */
@Auto
public class PremiumToolsController {
    private static final Logger LOG = LoggerFactory.getLogger(PremiumToolsController.class);

    private final PremiumToolsService service;

    @Inject
    PremiumToolsController(PremiumToolsService service) {
        this.service = service;
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void fetchPremium(FetchPremiumModulesRequest event) {
        LOG.debug(i18n().tr("Fetching premium modules"));
        CompletableFuture.supplyAsync(service::getPremiumTools).thenAcceptAsync(premiumModules -> {
            if (nonNull(premiumModules) && !premiumModules.isEmpty()) {
                eventStudio().broadcast(new PremiumToolsResponse(premiumModules));
            }
        }).whenComplete((r, e) -> {
            if (nonNull(e)) {
                LOG.warn(i18n().tr("Unable to retrieve premium modules"), e);
            }
        });
    }

}
