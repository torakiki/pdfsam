/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 11/set/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Andrea Vacondio (info@pdfsam.org).
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
package org.pdfsam.ui.banner;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import javax.inject.Inject;

import org.pdfsam.news.LatestNewsEvent;
import org.sejda.eventstudio.ReferenceStrength;

import javafx.application.Platform;
import javafx.scene.layout.HBox;

/**
 * Container for the banner buttons
 * 
 * @author Andrea Vacondio
 *
 */
class BannerButtons extends HBox {

    @Inject
    BannerButtons(LogButton logButton, DashboardButton dashboardButton, NewsButton newsButton, MenuButton menuButton) {
        getStyleClass().addAll("pdfsam-container", "pdfsam-banner-buttons");
        newsButton.setDisable(true);
        getChildren().addAll(dashboardButton, logButton, newsButton, menuButton);
        eventStudio().add(LatestNewsEvent.class, (e) -> {
            if (!e.latestNews.isEmpty()) {
                Platform.runLater(() -> {
                    newsButton.setDisable(false);
                    newsButton.setUpToDate(e.isUpToDate);
                });
            }
        } , Integer.MAX_VALUE, ReferenceStrength.STRONG);
    }
}
