/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30/ott/2013
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
package org.pdfsam.gui.components.dashboard.preference;

import jakarta.inject.Inject;
import jakarta.inject.Named;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import org.pdfsam.ui.components.support.FXValidationSupport;
import org.pdfsam.ui.components.support.Style;

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.pdfsam.ui.components.help.HelpUtils.helpIcon;

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
        workingDirectory.getTextField()
                .setPromptText(i18n().tr("Select a directory where documents will be saved and loaded by default"));
        workingDirectory.setBrowseWindowTitle(i18n().tr("Select a directory"));
        HBox workigDirPane = new HBox(workingDirectory,
                helpIcon(i18n().tr("Select a directory where documents will be saved and loaded by default")));
        HBox.setHgrow(workingDirectory, Priority.ALWAYS);
        workigDirPane.getStyleClass().add("with-help-hcontainer");

        workspace.getTextField().setPromptText(
                i18n().tr("Select a previously saved workspace that will be automatically loaded at startup"));
        workspace.setBrowseWindowTitle(i18n().tr("Select a workspace"));
        HBox workspaceDirPane = new HBox(workspace, helpIcon(
                i18n().tr("Select a previously saved workspace that will be automatically loaded at startup")));
        HBox.setHgrow(workspace, Priority.ALWAYS);
        workspaceDirPane.getStyleClass().add("with-help-hcontainer");
        workspace.getTextField().validProperty().addListener((o, oldVal, newVal) -> {
            saveWorkspaceOnExit.setDisable(
                    isBlank(workspace.getTextField().getText()) || newVal != FXValidationSupport.ValidationState.VALID);
        });
        workspace.getTextField().validate();
        getChildren().addAll(new Label(i18n().tr("Default working directory:")), workigDirPane,
                new Label(i18n().tr("Load default workspace at startup:")), workspaceDirPane, saveWorkspaceOnExit,
                savePwdInWorkspace);
        getStyleClass().addAll(Style.CONTAINER.css());
    }
}
