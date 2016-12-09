/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 24 ott 2015
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
package org.pdfsam.news;

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
 * Controller listening for latest news related requests
 * 
 * @author Andrea Vacondio
 */
@Auto
public class LatestNewsController {
    private static final Logger LOG = LoggerFactory.getLogger(LatestNewsController.class);

    private NewsService service;
    private int currentLatest = -1;

    @Inject
    LatestNewsController(NewsService service) {
        this.service = service;
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void fetchLatestNews(FetchLatestNewsRequest event) {
        LOG.debug(DefaultI18nContext.getInstance().i18n("Fetching latest news"));
        CompletableFuture.supplyAsync(service::getLatestNews).thenAcceptAsync(news -> {
            if (nonNull(news) && !news.isEmpty()) {
                currentLatest = news.get(0).getId();
                eventStudio().broadcast(new LatestNewsEvent(news, service.getLatestNewsSeen() >= currentLatest));
                news.stream().filter(n -> n.isImportant()).findFirst()
                        .filter(n -> service.getLatestImportantNewsSeen() < n.getId()).ifPresent(n -> {
                            service.setLatestImportantNewsSeen(n.getId());
                            eventStudio().broadcast(new NewImportantNewsEvent(n));
                        });
            }
        }).whenComplete((r, e) -> {
            if (nonNull(e)) {
                LOG.warn(DefaultI18nContext.getInstance().i18n("Unable to retrieve the latest news"), e);
            }
        });
    }

    @EventListener
    public void onShowNewsPanel(ShowNewsPanelRequest req) {
        if (service.getLatestNewsSeen() < currentLatest) {
            service.setLatestNewsSeen(currentLatest);
        }
    }
}
