/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/nov/2013
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
package org.pdfsam.ui.components.banner;

import javafx.scene.control.Menu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SeparatorMenuItem;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.model.ui.workspace.SaveWorkspaceRequest;
import org.pdfsam.core.support.io.FileType;
import org.pdfsam.service.ui.RecentWorkspacesService;
import org.pdfsam.ui.io.FileChoosers;
import org.pdfsam.ui.io.RememberingLatestFileChooserWrapper;
import org.pdfsam.ui.io.RememberingLatestFileChooserWrapper.OpenType;
import org.pdfsam.ui.workspace.LoadWorkspaceEvent;
import org.pdfsam.ui.workspace.WorkspaceLoadedEvent;

import javax.inject.Inject;
import java.io.File;
import java.util.Optional;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * Menu displaying workspace related items
 * 
 * @author Andrea Vacondio
 * 
 */
class WorkspaceMenu extends Menu {

    private RecentWorkspacesService service;
    private Menu recent;
    private Optional<File> latestWorkspace = Optional.empty();

    @Inject
    public WorkspaceMenu(RecentWorkspacesService service) {
        super(i18n().tr("_Workspace"));
        this.service = service;
        setId("workspaceMenu");
        MenuItem load = new MenuItem(i18n().tr("_Load"));
        load.setId("loadWorkspace");
        load.setOnAction(e -> loadWorkspace());
        MenuItem save = new MenuItem(i18n().tr("_Save"));
        save.setOnAction(e -> saveWorkspace());
        save.setId("saveWorkspace");
        recent = new Menu(i18n().tr("Recen_ts"));
        recent.setId("recentWorkspace");
        service.getRecentlyUsedWorkspaces().stream().map(WorkspaceMenuItem::new).forEach(recent.getItems()::add);
        MenuItem clear = new MenuItem(i18n().tr("_Clear recents"));
        clear.setOnAction(e -> clearWorkspaces());
        clear.setId("clearWorkspaces");
        getItems().addAll(load, save, new SeparatorMenuItem(), recent, clear);
        eventStudio().addAnnotatedListeners(this);
    }

    public void saveWorkspace() {
        RememberingLatestFileChooserWrapper fileChooser = FileChoosers.getFileChooser(
                i18n().tr("Select the workspace file to save"), FileType.JSON);

        latestWorkspace.ifPresentOrElse(f -> {
            fileChooser.setInitialDirectory(f.getParentFile());
            fileChooser.setInitialFileName(f.getName());
        }, () -> fileChooser.setInitialFileName("PDFsam_workspace.json"));

        File chosenFile = fileChooser.showDialog(OpenType.SAVE);
        if (chosenFile != null) {
            latestWorkspace = Optional.of(chosenFile);
            eventStudio().broadcast(new SaveWorkspaceRequest(chosenFile));
        }
    }

    public void loadWorkspace() {
        RememberingLatestFileChooserWrapper fileChooser = FileChoosers.getFileChooser(
                i18n().tr("Select the workspace to load"), FileType.JSON);
        latestWorkspace.ifPresent(f -> {
            fileChooser.setInitialDirectory(f.getParentFile());
            fileChooser.setInitialFileName(f.getName());
        });
        File chosenFile = fileChooser.showDialog(OpenType.OPEN);
        if (chosenFile != null) {
            eventStudio().broadcast(new LoadWorkspaceEvent(chosenFile));
        }
    }

    public void clearWorkspaces() {
        service.clear();
        recent.getItems().clear();
    }

    @EventListener
    public void onWorkspaceLoaded(WorkspaceLoadedEvent e) {
        recent.getItems().clear();
        service.getRecentlyUsedWorkspaces().stream().map(WorkspaceMenuItem::new).forEach(recent.getItems()::add);
        latestWorkspace = Optional.of(e.workspace());
    }
}
