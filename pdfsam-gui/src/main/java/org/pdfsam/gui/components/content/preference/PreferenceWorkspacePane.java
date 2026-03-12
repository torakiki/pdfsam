/*
 * This file is part of the PDF Split And Merge source code
 * Created on 30/ott/2013
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.gui.components.content.preference;

import jakarta.inject.Inject;
import jakarta.inject.Named;
import javafx.geometry.HPos;
import javafx.geometry.VPos;
import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import org.pdfsam.ui.components.support.FXValidationSupport;
import org.pdfsam.ui.components.support.Style;

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.pdfsam.ui.components.support.Views.helpIcon;

/**
 * Preference pane displaying the workspace section
 *
 * @author Andrea Vacondio
 *
 */
class PreferenceWorkspacePane extends VBox {

    @Inject
    public PreferenceWorkspacePane(@Named("workingDirectory") PreferenceBrowsableDirectoryField workingDirectory,
            @Named("workspace") PreferenceBrowsableFileField workspace,
            @Named("saveWorkspaceOnExit") PreferenceCheckBox saveWorkspaceOnExit,
            @Named("savePwdInWorkspace") PreferenceCheckBox savePwdInWorkspace) {
        String workingDirHelpText = i18n().tr("Select a directory where documents will be saved and loaded by default");
        workingDirectory.getTextField().setPromptText(workingDirHelpText);
        workingDirectory.getTextField().setAccessibleText(workingDirHelpText);
        workingDirectory.setBrowseWindowTitle(i18n().tr("Select a directory"));
        var workigDirPane = new GridPane();
        workigDirPane.getStyleClass().addAll(Style.GRID.css());
        workigDirPane.getStyleClass().addAll(Style.VITEM.css());
        GridPane.setValignment(workingDirectory, VPos.BOTTOM);
        GridPane.setHalignment(workingDirectory, HPos.LEFT);
        GridPane.setHgrow(workingDirectory, Priority.ALWAYS);
        workigDirPane.add(workingDirectory, 0, 0);
        var workingDirLabel = helpIcon(workingDirHelpText);
        GridPane.setValignment(workingDirLabel, VPos.CENTER);
        workigDirPane.add(workingDirLabel, 1, 0);

        String workspaceHelpText = i18n().tr(
                "Select a previously saved workspace that will be automatically loaded at startup");
        workspace.getTextField().setPromptText(workspaceHelpText);
        workspace.getTextField().setAccessibleText(workspaceHelpText);
        workspace.setBrowseWindowTitle(i18n().tr("Select a workspace"));

        var workspaceDirPane = new GridPane();
        workspaceDirPane.getStyleClass().addAll(Style.GRID.css());
        workspaceDirPane.getStyleClass().addAll(Style.VITEM.css());
        GridPane.setValignment(workspace, VPos.BOTTOM);
        GridPane.setHalignment(workspace, HPos.LEFT);
        GridPane.setHgrow(workspace, Priority.ALWAYS);
        workspaceDirPane.add(workspace, 0, 0);
        var workspaceDirLabel = helpIcon(workspaceHelpText);
        GridPane.setValignment(workspaceDirLabel, VPos.CENTER);
        workspaceDirPane.add(workspaceDirLabel, 1, 0);

        workspace.getTextField().validProperty().addListener((o, oldVal, newVal) -> saveWorkspaceOnExit.setDisable(
                isBlank(workspace.getTextField().getText()) || newVal != FXValidationSupport.ValidationState.VALID));
        workspace.getTextField().validate();
        var workingDirSectionLabel = new Label(i18n().tr("Default working directory:"));
        workingDirSectionLabel.setLabelFor(workingDirectory.getTextField());
        var workspaceSectionLabel = new Label(i18n().tr("Load default workspace at startup:"));
        workspaceSectionLabel.setLabelFor(workspace.getTextField());
        getChildren().addAll(workingDirSectionLabel, workigDirPane, workspaceSectionLabel, workspaceDirPane,
                saveWorkspaceOnExit, savePwdInWorkspace);
        getStyleClass().addAll(Style.CONTAINER.css());
    }
}
