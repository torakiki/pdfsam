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
import java.util.Optional;

import javax.inject.Inject;

import org.pdfsam.module.Module;
import org.pdfsam.ui.commons.SetActiveModuleRequest;
import org.pdfsam.ui.event.SetTitleEvent;
import org.pdfsam.ui.quickbar.QuickbarPane;
import org.pdfsam.ui.support.Style;
import org.sejda.eventstudio.annotation.EventListener;

import javafx.animation.FadeTransition;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.ScrollPane.ScrollBarPolicy;
import javafx.scene.layout.BorderPane;
import javafx.util.Duration;

/**
 * Main workarea. It contains a quickbar to quickly access modules and a main area where the module pane is shown.
 * 
 * @author Andrea Vacondio
 *
 */
public class WorkArea extends BorderPane {

    private Map<String, Module> modules = new HashMap<>();
    private Optional<Module> current = Optional.empty();
    private ScrollPane center = new ScrollPane();
    private FadeTransition fade = new FadeTransition(new Duration(300), center);

    @Inject
    public WorkArea(List<Module> modules, QuickbarModuleButtonsPane modulesButtons) {
        getStyleClass().addAll(Style.CONTAINER.css());
        setId("work-area");
        for (Module module : modules) {
            this.modules.put(module.id(), module);
        }
        fade.setFromValue(0);
        fade.setToValue(1);
        center.setHbarPolicy(ScrollBarPolicy.NEVER);
        center.setFitToWidth(true);
        center.setFitToHeight(true);
        setCenter(center);
        setLeft(new QuickbarPane(modulesButtons));
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void onSetActiveModule(SetActiveModuleRequest request) {
        request.getActiveModuleId().ifPresent(id -> {
            Module requested = modules.get(id);
            if (requested != null) {
                current = Optional.of(requested);
                center.setContent(requested.modulePanel());
                fade.play();
            }
        });
        eventStudio().broadcast(new SetTitleEvent(current.map(m -> m.descriptor().getName()).orElse("")));
    }
}