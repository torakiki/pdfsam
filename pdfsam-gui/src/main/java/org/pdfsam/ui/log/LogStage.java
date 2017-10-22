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
package org.pdfsam.ui.log;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.List;

import javax.inject.Inject;

import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.ui.commons.ClosePane;
import org.pdfsam.ui.commons.HideOnEscapeHandler;
import org.pdfsam.ui.commons.ShowStageRequest;
import org.pdfsam.ui.support.Style;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.eventstudio.annotation.EventStation;
import org.sejda.injector.Auto;

import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;

/**
 * Stage for the log panel
 * 
 * @author Andrea Vacondio
 * 
 */
@Auto
public class LogStage extends Stage {

    @EventStation
    public static final String LOGSTAGE_EVENTSTATION = "LogStage";

    @Inject
    public LogStage(LogPane logPane, LogListView logView, List<Image> logos, StylesConfig styles) {
        BorderPane containerPane = new BorderPane();
        containerPane.getStyleClass().addAll(Style.CONTAINER.css());
        containerPane.setCenter(logPane);
        containerPane.setBottom(new ClosePane());
        Scene scene = new Scene(containerPane);
        scene.getStylesheets().addAll(styles.styles());
        scene.setOnKeyReleased(new HideOnEscapeHandler(this));
        setScene(scene);
        setTitle(DefaultI18nContext.getInstance().i18n("Log register"));
        getIcons().addAll(logos);
        setMaximized(true);
        eventStudio().addAnnotatedListeners(this);
        this.onShowingProperty().addListener((o, oldVal, newVal) -> logView.scrollToBottomIfShowing());
        eventStudio().add(logView, LOGSTAGE_EVENTSTATION);
    }

    @EventListener
    void requestShow(ShowStageRequest event) {
        if (!isShowing()) {
            centerOnScreen();
            show();
        }
        requestFocus();
    }
}
