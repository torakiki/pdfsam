/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 31/ott/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javafx.animation.KeyFrame;
import javafx.animation.KeyValue;
import javafx.animation.Timeline;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.StackPane;
import javafx.util.Duration;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.gui.quickbar.QuickbarPane;
import org.pdfsam.ui.module.BaseTaskExecutionModule;
import org.sejda.eventstudio.annotation.EventListener;

/**
 * Panel containing the main area where modules are displayed and a quickbar to access them
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
public class ContentPane extends BorderPane {

    /**
     * 
     */
    private static final int MODULE_FADE_MILLIS = 500;
    @Inject
    private QuickbarPane navigation;
    private Map<String, BaseTaskExecutionModule> modules = new HashMap<>();
    private StackPane center = new StackPane();

    @Inject
    public ContentPane(List<BaseTaskExecutionModule> modulesMap) {
        for (BaseTaskExecutionModule module : modulesMap) {
            modules.put(module.id(), module);
        }
    }

    @PostConstruct
    private void init() {
        setLeft(navigation);
        setCenter(center);
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void onSetCurrentModuleRequest(SetCurrentModuleRequest request) {
        BaseTaskExecutionModule requested = modules.get(request.getModuleId());
        if (requested != null) {
            center.setOpacity(0.0);
            center.getChildren().clear();
            center.getChildren().add(requested.modulePanel());
            Timeline fadeIn = new Timeline(new KeyFrame(Duration.ZERO, new KeyValue(center.opacityProperty(), 0.0)),
                    new KeyFrame(new Duration(MODULE_FADE_MILLIS), new KeyValue(center.opacityProperty(), 1.0)));
            fadeIn.play();
        }
    }
}
