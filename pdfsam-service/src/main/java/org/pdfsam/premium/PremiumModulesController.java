/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 23 nov 2016
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.premium;

import static java.util.Objects.nonNull;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.concurrent.CompletableFuture;

import javax.inject.Inject;

import org.pdfsam.i18n.DefaultI18nContext;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.injector.Auto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Andrea Vacondio
 */
@Auto
public class PremiumModulesController {
    private static final Logger LOG = LoggerFactory.getLogger(PremiumModulesController.class);

    private PremiumModulesService service;

    @Inject
    PremiumModulesController(PremiumModulesService service) {
        this.service = service;
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void fetchPremium(FetchPremiumModulesRequest event) {
        LOG.debug(DefaultI18nContext.getInstance().i18n("Fetching premium modules"));
        CompletableFuture.supplyAsync(service::getPremiumModules).thenAcceptAsync(premiumModules -> {
            if (nonNull(premiumModules) && !premiumModules.isEmpty()) {
                eventStudio().broadcast(new PremiumModulesEvent(premiumModules));
            }
        }).whenComplete((r, e) -> {
            if (nonNull(e)) {
                LOG.warn(DefaultI18nContext.getInstance().i18n("Unable to retrieve premium modules"), e);
            }
        });
    }

}
