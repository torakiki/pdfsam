/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 24/mar/2014
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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

import java.util.HashSet;
import java.util.Set;

import javax.inject.Inject;

import org.pdfsam.ui.commons.SetActiveModuleRequest;
import org.pdfsam.ui.quickbar.BaseQuickbarButtonsPane;
import org.sejda.eventstudio.annotation.EventListener;

/**
 * Panel showing buttons to access the most used and most recently used modules
 * 
 * @author Andrea Vacondio
 *
 */
class QuickbarModuleButtonsPane extends BaseQuickbarButtonsPane {

    private Set<ModuleButton> buttons = new HashSet<>();

    @Inject
    QuickbarModuleButtonsPane(QuickbarModuleButtonsProvider provider) {
        provider.buttons().forEach(b -> {
            b.displayTextProperty().bind(displayTextProperty());
            getChildren().add(b);
            this.buttons.add(b);
        });
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void onSetCurrentModuleRequest(SetActiveModuleRequest r) {
        r.getActiveModuleId().ifPresent(id -> buttons.forEach(b -> b.setSelected(b.moduleId().equals(id))));
    }

}
