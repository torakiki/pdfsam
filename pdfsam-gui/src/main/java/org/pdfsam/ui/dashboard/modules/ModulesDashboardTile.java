/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/mag/2014
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
package org.pdfsam.ui.dashboard.modules;

import static org.pdfsam.ui.commons.SetActiveModuleRequest.activeteModule;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.pdfsam.module.Module;
import org.pdfsam.ui.commons.UrlButton;

import de.jensd.fx.glyphs.GlyphsDude;
import de.jensd.fx.glyphs.materialdesignicons.MaterialDesignIcon;
import javafx.scene.layout.VBox;

/**
 * Tile for the dashboard modules pane. It displays details about the {@link Module} and by clicking it the module will be opened in the Workarea.
 * 
 * @author Andrea Vacondio
 *
 */
class ModulesDashboardTile extends DashboardTile {

    private VBox toolButtons = new VBox(5);

    ModulesDashboardTile(Module module) {
        super(module.descriptor().getName(), module.descriptor().getDescription(), module.graphic());
        setOnAction(e -> eventStudio().broadcast(activeteModule(module.id())));

        module.descriptor().getSupportURL().ifPresent(url -> {
            UrlButton helpButton = UrlButton.urlButton(null, url, null, "pdfsam-toolbar-button");
            helpButton.setGraphic(GlyphsDude.createIcon(MaterialDesignIcon.HELP_CIRCLE, "1.4em"));
            toolButtons.getChildren().add(helpButton);
            toolButtons.getStyleClass().add("dashboard-modules-toolbar");
            addBottomPanel(toolButtons);
        });
    }
}
