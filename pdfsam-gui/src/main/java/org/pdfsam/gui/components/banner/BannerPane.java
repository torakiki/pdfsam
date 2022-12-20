/*
 * This file is part of the PDF Split And Merge source code
 * Created on 01/nov/2013
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
package org.pdfsam.gui.components.banner;

import jakarta.inject.Inject;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * Panel showed on the top part of the application. It displays branding images and a toolbar
 *
 * @author Andrea Vacondio
 */
public class BannerPane extends HBox {

    @Inject
    public BannerPane(BannerButtons buttons, ImageView payoff) {
        getStyleClass().add("pdfsam-banner");
        HBox.setHgrow(buttons, Priority.ALWAYS);
        HBox logoView = new HBox();
        logoView.getStyleClass().add("pdfsam-logo");
        logoView.getChildren().addAll(payoff);
        getChildren().addAll(logoView, buttons);
        eventStudio().addAnnotatedListeners(this);
    }

}
