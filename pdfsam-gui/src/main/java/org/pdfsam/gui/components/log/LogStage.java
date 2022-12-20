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
package org.pdfsam.gui.components.log;

import jakarta.inject.Inject;
import jakarta.inject.Named;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.eventstudio.annotation.EventStation;
import org.pdfsam.injector.Auto;
import org.pdfsam.model.ui.HideStageRequest;
import org.pdfsam.model.ui.ShowStageRequest;
import org.pdfsam.ui.components.commons.ClosePane;
import org.pdfsam.ui.components.support.Style;

import java.util.List;

import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

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
    public LogStage(LogPane logPane, LogListView logView, @Named("icons") List<Image> logos) {
        BorderPane containerPane = new BorderPane();
        containerPane.getStyleClass().addAll(Style.CONTAINER.css());
        containerPane.setCenter(logPane);
        containerPane.setBottom(new ClosePane((a) -> eventStudio().broadcast(HideStageRequest.INSTANCE, "LogStage")));
        Scene scene = new Scene(containerPane);
        scene.setOnKeyReleased(k -> {
            if (this.isShowing() && new KeyCodeCombination(KeyCode.ESCAPE).match(k)) {
                eventStudio().broadcast(HideStageRequest.INSTANCE, "LogStage");
            }
        });
        setScene(scene);
        app().runtimeState().subscribeThemedScene(scene);
        setTitle(i18n().tr("Log register"));
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

    @EventListener
    void requestHide(HideStageRequest event) {
        if (isShowing()) {
            hide();
        }
    }
}
