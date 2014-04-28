/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 01/nov/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import java.util.Collection;
import java.util.List;

import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.ui.ClosePane;
import org.pdfsam.ui.HideOnEscapeHandler;
import org.pdfsam.ui.support.ShowRequestEvent;
import org.pdfsam.ui.support.Style;
import org.sejda.eventstudio.annotation.EventListener;

/**
 * Stage for the log panel
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
public class LogStage extends Stage {

    public static final String LOGSTAGE_EVENTSTATION = "LogStage";
    @Inject
    private LogPane logPane;
    @Inject
    private Collection<Image> logos;
    @Resource(name = "styles")
    private List<String> styles;

    @PostConstruct
    void init() {
        BorderPane containerPane = new BorderPane();
        containerPane.getStyleClass().addAll(Style.CONTAINER.css());
        containerPane.setCenter(logPane);
        containerPane.setBottom(new ClosePane());
        Scene scene = new Scene(containerPane);
        scene.getStylesheets().addAll(styles);
        scene.setOnKeyReleased(new HideOnEscapeHandler(this));
        setScene(scene);
        setTitle(DefaultI18nContext.getInstance().i18n("Log register"));
        getIcons().addAll(logos);
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener(station = LOGSTAGE_EVENTSTATION)
    void requestShow(ShowRequestEvent event) {
        if (!isShowing()) {
            centerOnScreen();
            show();
        }
        requestFocus();
    }
}
