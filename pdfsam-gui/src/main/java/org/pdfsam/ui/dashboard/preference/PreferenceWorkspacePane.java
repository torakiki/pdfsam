/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30/ott/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.ui.dashboard.preference;

import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.VBox;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.I18nContext;
import org.pdfsam.module.ClearUsageRequestEvent;
import org.pdfsam.ui.support.Style;

/**
 * Preference pane displaying the workspace section
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
class PreferenceWorkspacePane extends VBox {

    @Inject
    @Named("workingDirectory")
    private PreferenceBrowsableDirectoryField workingDirectory;
    @Inject
    @Named("workspace")
    private PreferenceBrowsableFileField workspace;

    @PostConstruct
    public void post() {
        I18nContext i18n = DefaultI18nContext.getInstance();
        workspace.getTextField().setPromptText(
                i18n.i18n("Select a previously saved workspace that will be automatically loaded at startup"));
        workspace.getTextField().setTooltip(
                new Tooltip(i18n
                        .i18n("Select a previously saved workspace that will be automatically loaded at startup")));
        workspace.setBrowseWindowTitle(i18n.i18n("Select a workspace"));
        workspace.getStyleClass().add("spaced-vitem");

        workingDirectory.getTextField().setPromptText(
                i18n.i18n("Select a directory where documents will be saved and loaded by default"));
        workingDirectory.getTextField().setTooltip(
                new Tooltip(i18n.i18n("Select a directory where documents will be saved and loaded by default")));
        workingDirectory.setBrowseWindowTitle(i18n.i18n("Select a directory"));
        workingDirectory.getStyleClass().add("spaced-vitem");

        Button clearButton = new Button(i18n.i18n("Clear usage statistics"));
        clearButton.getStyleClass().addAll(Style.BUTTON.css());
        clearButton.setOnAction(e -> eventStudio().broadcast(new ClearUsageRequestEvent()));

        getChildren().addAll(new Label(i18n.i18n("Load default workspace at startup:")), workspace,
                new Label(i18n.i18n("Default working directory:")), workingDirectory, clearButton);
        getStyleClass().addAll(Style.CONTAINER.css());
    }
}
