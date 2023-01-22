/*
 * This file is part of the PDF Split And Merge source code
 * Created on 11/dic/2014
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
package org.pdfsam.gui.components.content.workspace;

import javafx.scene.control.MenuItem;
import org.apache.commons.lang3.StringUtils;
import org.pdfsam.model.ui.workspace.LoadWorkspaceRequest;

import java.io.File;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.sejda.commons.util.RequireUtils.requireNotBlank;

/**
 * Menu item for the recent workspaces menu
 *
 * @author Andrea Vacondio
 */
class WorkspaceMenuItem extends MenuItem {

    WorkspaceMenuItem(String path) {
        requireNotBlank(path, "Workspace item cannot be blank");
        this.setText(StringUtils.abbreviate(path, path.length(), 60));
        this.setOnAction(a -> eventStudio().broadcast(new LoadWorkspaceRequest(new File(path))));
        this.setMnemonicParsing(false);
    }
}
