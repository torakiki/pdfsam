/*
 * This file is part of the PDF Split And Merge source code
 * Created on 24/mar/2014
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
package org.pdfsam.gui.components.workarea;

import jakarta.inject.Inject;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.gui.components.quickbar.BaseQuickbarButtonsPane;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.ui.SetActiveToolRequest;

import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Set;

import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * Panel showing buttons to access the most used and most recently used modules
 *
 * @author Andrea Vacondio
 */
class QuickbarToolButtonsPane extends BaseQuickbarButtonsPane {

    private final Set<ToolButton> buttons = new HashSet<>();

    @Inject
    QuickbarToolButtonsPane() {
        this(app().runtimeState().tools().values());
    }

    QuickbarToolButtonsPane(Collection<Tool> tools) {
        Comparator<Tool> comparator = Comparator.comparing(t -> t.descriptor().category());
        tools.stream().sorted(comparator.thenComparing(m -> m.descriptor().name())).map(ToolButton::new).forEach(b -> {
            b.displayTextProperty().bind(displayTextProperty());
            getChildren().add(b);
            this.buttons.add(b);
        });
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void onSetCurrentModuleRequest(SetActiveToolRequest r) {
        buttons.forEach(b -> b.setSelected(r.id().equals(b.toolBinding())));
    }

}
