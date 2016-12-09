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

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.List;

import javax.inject.Inject;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.Module;
import org.pdfsam.premium.PremiumModulesEvent;
import org.sejda.eventstudio.annotation.EventListener;

import javafx.application.Platform;
import javafx.scene.control.Label;
import javafx.scene.layout.FlowPane;
import javafx.scene.layout.VBox;

/**
 * Panel showing modules button to in the dashboard. It's used a dashboard home where the users can select the modules the want to use.
 * 
 * @author Andrea Vacondio
 *
 */
public class ModulesDashboardPane extends VBox {

    @Inject
    public ModulesDashboardPane(List<Module> modules) {
        FlowPane modulesPane = new FlowPane();
        getStyleClass().addAll("dashboard-container");
        modulesPane.getStyleClass().add("dashboard-modules");
        modules.stream().sorted((a, b) -> a.descriptor().getPriority() - b.descriptor().getPriority())
                .map(ModulesDashboardTile::new).forEach(modulesPane.getChildren()::add);
        this.getChildren().add(modulesPane);
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void onPremiumModules(PremiumModulesEvent e) {
        if (!e.premiumModules.isEmpty()) {
            Label premiumTile = new Label(DefaultI18nContext.getInstance().i18n("Premium features"));
            premiumTile.getStyleClass().add("modules-tile-title");
            FlowPane modulesPane = new FlowPane();
            modulesPane.getStyleClass().add("dashboard-modules");
            e.premiumModules.stream().sorted((a, b) -> a.getId() - b.getId()).map(PremiumModuleTile::new)
                    .forEach(modulesPane.getChildren()::add);
            Platform.runLater(() -> this.getChildren().addAll(premiumTile, modulesPane));
        }
    }
}
