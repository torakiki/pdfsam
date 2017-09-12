/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/nov/2013
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
package org.pdfsam.ui.banner;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;

import javax.inject.Inject;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.support.io.FileType;
import org.pdfsam.ui.RecentWorkspacesService;
import org.pdfsam.ui.io.FileChoosers;
import org.pdfsam.ui.io.RememberingLatestFileChooserWrapper;
import org.pdfsam.ui.io.RememberingLatestFileChooserWrapper.OpenType;
import org.pdfsam.ui.workspace.LoadWorkspaceEvent;
import org.pdfsam.ui.workspace.SaveWorkspaceEvent;
import org.pdfsam.ui.workspace.WorkspaceLoadedEvent;
import org.sejda.eventstudio.annotation.EventListener;

import javafx.scene.control.Menu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SeparatorMenuItem;

/**
 * Menu displaying workspace related items
 * 
 * @author Andrea Vacondio
 * 
 */
class WorkspaceMenu extends Menu {

    private RecentWorkspacesService service;
    private Menu recent;

    @Inject
    public WorkspaceMenu(RecentWorkspacesService service) {
        super(DefaultI18nContext.getInstance().i18n("_Workspace"));
        this.service = service;
        setId("workspaceMenu");
        MenuItem load = new MenuItem(DefaultI18nContext.getInstance().i18n("_Load"));
        load.setId("loadWorkspace");
        load.setOnAction(e -> loadWorkspace());
        MenuItem save = new MenuItem(DefaultI18nContext.getInstance().i18n("_Save"));
        save.setOnAction(e -> saveWorkspace());
        save.setId("saveWorkspace");
        recent = new Menu(DefaultI18nContext.getInstance().i18n("Recen_t"));
        recent.setId("recentWorkspace");
        service.getRecentlyUsedWorkspaces().stream().map(WorkspaceMenuItem::new).forEach(recent.getItems()::add);
        getItems().addAll(load, save, new SeparatorMenuItem(), recent);
        eventStudio().addAnnotatedListeners(this);
    }

    public void saveWorkspace() {
        RememberingLatestFileChooserWrapper fileChooser = FileChoosers.getFileChooser(FileType.JSON,
                DefaultI18nContext.getInstance().i18n("Select the workspace file to save"));
        fileChooser.setInitialFileName("PDFsam_workspace.json");
        File chosenFile = fileChooser.showDialog(OpenType.SAVE);
        if (chosenFile != null) {
            eventStudio().broadcast(new SaveWorkspaceEvent(chosenFile));
        }
    }

    public void loadWorkspace() {
        RememberingLatestFileChooserWrapper fileChooser = FileChoosers.getFileChooser(FileType.JSON,
                DefaultI18nContext.getInstance().i18n("Select the workspace to load"));
        File chosenFile = fileChooser.showDialog(OpenType.OPEN);
        if (chosenFile != null) {
            eventStudio().broadcast(new LoadWorkspaceEvent(chosenFile));
        }
    }

    @EventListener
    public void onWorkspaceLoaded(WorkspaceLoadedEvent e) {
        recent.getItems().clear();
        service.getRecentlyUsedWorkspaces().stream().map(WorkspaceMenuItem::new).forEach(recent.getItems()::add);
    }
}
