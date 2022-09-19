/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 03/mag/2014
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
package org.pdfsam.ui.banner;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import javax.inject.Inject;

import org.pdfsam.i18n.I18nContext;
import org.pdfsam.ui.dashboard.PreferencesDashboardItem;
import org.pdfsam.ui.event.SetActiveDashboardItemRequest;

import javafx.application.Platform;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;

/**
 * Context menu showing app functionalities. This is supposed to be activated by the menu button
 * 
 * @author Andrea Vacondio
 *
 */
class AppContextMenu extends ContextMenu {
    @Inject
    AppContextMenu(WorkspaceMenu workspace, ModulesMenu modulesMenu) {
        MenuItem exit = new MenuItem(i18n().tr("E_xit"));
        exit.setOnAction(e -> Platform.exit());
        exit.setAccelerator(new KeyCodeCombination(KeyCode.Q, KeyCombination.SHORTCUT_DOWN));
        getItems().addAll(workspace, modulesMenu);
        if (!Boolean.getBoolean(PreferencesDashboardItem.PDFSAM_DISABLE_SETTINGS_DEPRECATED)
                && !Boolean.getBoolean(PreferencesDashboardItem.PDFSAM_DISABLE_SETTINGS)) {
            MenuItem settings = new MenuItem(i18n().tr("_Settings"));
            settings.setOnAction(
                    e -> eventStudio().broadcast(new SetActiveDashboardItemRequest(PreferencesDashboardItem.ID)));
            getItems().add(settings);
        }

        getItems().addAll(new SeparatorMenuItem(), exit);
    }
}
