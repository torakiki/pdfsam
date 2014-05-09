/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 05/mag/2014
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

import java.util.List;

import javafx.scene.layout.FlowPane;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.module.Module;

/**
 * Panel showing modules button to in the dashboard. It's used a dashboard home where the users can select the modules the want to use.
 * 
 * @author Andrea Vacondio
 *
 */
@Named
public class ModulesDashboardPane extends FlowPane {

    @Inject
    public ModulesDashboardPane(List<Module> modules) {
        getStyleClass().addAll("dashboard-container", "dashboard-modules");
        modules.stream().sorted((a, b) -> a.descriptor().getPriority() - b.descriptor().getPriority())
                .map(ModulesDashboardTile::new).forEach(getChildren()::add);
    }
}
