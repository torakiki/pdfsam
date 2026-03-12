/*
 * This file is part of the PDF Split And Merge source code
 * Created on 01/nov/2013
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
package org.pdfsam.gui.components.info;

import jakarta.inject.Inject;
import jakarta.inject.Named;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;
import org.pdfsam.ui.components.commons.ClosePane;
import org.pdfsam.ui.components.commons.HideOnEscapeHandler;
import org.pdfsam.ui.components.support.Style;

import java.util.List;

import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Stage for the info panel
 * 
 * @author Andrea Vacondio
 * 
 */
class InfoStage extends Stage {

    @Inject
    public InfoStage(InfoPane infoPane, @Named("icons") List<Image> logos) {
        BorderPane containerPane = new BorderPane();
        containerPane.getStyleClass().addAll(Style.CONTAINER.css());
        containerPane.setCenter(infoPane);
        containerPane.setBottom(new ClosePane());
        Scene scene = new Scene(containerPane);
        scene.setOnKeyReleased(new HideOnEscapeHandler(this));
        setScene(scene);
        app().registerScene(scene);
        setTitle(i18n().tr("Document details"));
        getIcons().addAll(logos);
        setMaximized(true);
        setOnShown(e -> infoPane.requestFocus());
    }

}
