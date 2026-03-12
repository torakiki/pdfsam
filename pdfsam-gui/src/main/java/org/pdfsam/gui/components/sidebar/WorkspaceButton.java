/*
 * This file is part of the PDF Split And Merge source code
 * Created on 20/01/23
 * Copyright 2023 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.gui.components.sidebar;

import jakarta.inject.Inject;
import javafx.geometry.Side;
import javafx.scene.AccessibleRole;
import javafx.scene.control.Tooltip;
import org.kordamp.ikonli.javafx.FontIcon;
import org.kordamp.ikonli.unicons.UniconsLine;
import org.pdfsam.gui.components.content.workspace.WorkspaceMenu;

import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Button to open the workspace menu.
 *
 * @author Andrea Vacondio
 */
public class WorkspaceButton extends SidebarButton {

    @Inject
    public WorkspaceButton(WorkspaceMenu workspaceMenu) {
        super(i18n().tr("Workspace"));
        var icon = new FontIcon(UniconsLine.SUITCASE);
        icon.setAccessibleRole(AccessibleRole.IMAGE_VIEW);
        setGraphic(icon);
        setAccessibleRole(AccessibleRole.MENU_BUTTON);
        setTooltip(new Tooltip(i18n().tr("Manage your workspaces")));
        setOnAction(e -> workspaceMenu.show(this, Side.RIGHT, 0, 0));
    }
}
