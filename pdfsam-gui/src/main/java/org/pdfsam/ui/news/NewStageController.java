/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 20/ott/2014
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
package org.pdfsam.ui.news;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.UserContext;
import org.pdfsam.ui.NewsPolicy;
import org.pdfsam.ui.StageService;
import org.pdfsam.ui.commons.ShowStageRequest;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.eventstudio.annotation.EventStation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;

/**
 * @author Andrea Vacondio
 *
 */
@Named
public class NewStageController {
    private static final Logger LOG = LoggerFactory.getLogger(NewStageController.class);
    @EventStation
    public static final String NEWSSTAGE_EVENTSTATION = "NewsStage";

    @Inject
    private ApplicationContext applicationContext;

    private StageService service;
    private UserContext userContext;

    @Inject
    NewStageController(StageService service, UserContext userContext) {
        this.service = service;
        this.userContext = userContext;
        eventStudio().addAnnotatedListeners(this);
    }

    @SuppressWarnings("unused")
    @EventListener
    void requestShow(ShowStageRequest event) {
        if (NewsPolicy.valueOf(userContext.getNewsPolicy()).isTimeToShow(
                this.service.getLatestNewsStageDisplayInstant())) {
            LOG.debug("Requesting to display latest PDFsam news");
            NewsStage stage = applicationContext.getBean(NewsStage.class);
            stage.loadAndShow(showing -> service.newsStageDisplayed());
        }
    }

}
