/*
 * This file is part of the PDF Split And Merge source code
 * Created on 24 ott 2015
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.service.news;

import jakarta.inject.Inject;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.injector.Auto;
import org.pdfsam.model.lifecycle.CleanupRequest;
import org.pdfsam.model.news.FetchLatestNewsRequest;
import org.pdfsam.model.news.LatestNewsResponse;
import org.pdfsam.model.news.NewImportantNewsEvent;
import org.pdfsam.model.news.NewsData;
import org.pdfsam.model.news.ToggleNewsPanelRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Optional;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Controller listening for latest news related requests
 *
 * @author Andrea Vacondio
 */
@Auto
public class NewsController {
    private static final Logger LOG = LoggerFactory.getLogger(NewsController.class);

    private final NewsService service;
    private int currentLatest = -1;

    @Inject
    NewsController(NewsService service) {
        this.service = service;
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void fetchLatestNews(FetchLatestNewsRequest event) {
        Thread.ofVirtual().name("news-checker-thread").start(() -> {
            LOG.debug(i18n().tr("Fetching latest news"));
            try {
                Optional.ofNullable(service.getLatestNews()).filter(l -> !l.isEmpty()).ifPresent(news -> {
                    currentLatest = news.get(0).id();
                    eventStudio().broadcast(new LatestNewsResponse(news, service.getLatestNewsSeen() >= currentLatest));
                    news.stream().filter(NewsData::important).findFirst()
                            .filter(n -> service.getLatestImportantNewsSeen() < n.id()).ifPresent(n -> {
                                service.setLatestImportantNewsSeen(n.id());
                                eventStudio().broadcast(new NewImportantNewsEvent(n));
                            });
                });
            } catch (Exception e) {
                LOG.warn(i18n().tr("Unable to retrieve the latest news"), e);
            }
        });
    }

    @EventListener
    public void onShowNewsPanel(ToggleNewsPanelRequest req) {
        if (service.getLatestNewsSeen() < currentLatest) {
            service.setLatestNewsSeen(currentLatest);
        }
    }

    @EventListener
    public void onCleanupRequest(CleanupRequest req) {
        service.clear();
    }
}
