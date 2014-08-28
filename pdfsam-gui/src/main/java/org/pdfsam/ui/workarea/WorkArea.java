/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 02/mag/2014
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
package org.pdfsam.ui.workarea;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javafx.animation.FadeTransition;
import javafx.scene.control.Label;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.StackPane;
import javafx.util.Duration;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.module.Module;
import org.pdfsam.ui.event.SetActiveModuleRequest;
import org.pdfsam.ui.event.SetTitleEvent;
import org.pdfsam.ui.support.Style;
import org.sejda.eventstudio.annotation.EventListener;

/**
 * Main workarea. It contains a quickbar to quickly access modules and a main area where the module pane is shown.
 * 
 * @author Andrea Vacondio
 *
 */
@Named
public class WorkArea extends BorderPane {

    private Map<String, Module> modules = new HashMap<>();
    private StackPane center = new StackPane();
    private FadeTransition fade = new FadeTransition(new Duration(300), center);

    @Inject
    public WorkArea(List<Module> modulesList, QuickbarWrokarea navigation) {
        getStyleClass().addAll(Style.CONTAINER.css());
        Label emptyArea = new Label(DefaultI18nContext.getInstance().i18n("Please select a module"));
        emptyArea.getStyleClass().add("empty-notice");
        for (Module module : modulesList) {
            modules.put(module.id(), module);
        }
        fade.setFromValue(0);
        fade.setToValue(1);
        center.getChildren().setAll(emptyArea);
        setLeft(navigation);
        setCenter(center);
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void onSetActiveModule(SetActiveModuleRequest request) {
        request.getActiveModuleId().ifPresent(id -> {
            Module requested = modules.get(id);
            if (requested != null) {
                center.getChildren().setAll(requested.modulePanel());
                fade.play();
                eventStudio().broadcast(new SetTitleEvent(requested.descriptor().getName()));
            }
        });
    }
}