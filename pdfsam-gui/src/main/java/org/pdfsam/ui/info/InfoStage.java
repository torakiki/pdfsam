/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 01/nov/2013
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
package org.pdfsam.ui.info;

import java.util.List;

import javax.inject.Inject;

import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.ui.commons.ClosePane;
import org.pdfsam.ui.commons.HideOnEscapeHandler;
import org.pdfsam.ui.support.Style;

import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;

/**
 * Stage for the info panel
 * 
 * @author Andrea Vacondio
 * 
 */
class InfoStage extends Stage {

    @Inject
    public InfoStage(InfoPane infoPane, List<Image> logos, StylesConfig styles) {
        BorderPane containerPane = new BorderPane();
        containerPane.getStyleClass().addAll(Style.CONTAINER.css());
        containerPane.setCenter(infoPane);
        containerPane.setBottom(new ClosePane());
        Scene scene = new Scene(containerPane);
        scene.getStylesheets().addAll(styles.styles());
        scene.setOnKeyReleased(new HideOnEscapeHandler(this));
        setScene(scene);
        setTitle(DefaultI18nContext.getInstance().i18n("Document details"));
        getIcons().addAll(logos);
        setMaximized(true);
    }

}
