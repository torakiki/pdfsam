/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 28/ott/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui.preference;

import javafx.scene.Node;
import javafx.scene.control.Label;
import javafx.scene.control.TitledPane;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.VBox;

import javax.annotation.PostConstruct;
import javax.inject.Named;

import org.pdfsam.context.BooleanUserPreference;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.context.I18nContext;
import org.pdfsam.context.StringUserPreference;
import org.pdfsam.support.io.FileType;

/**
 * Panel showing preferences/options that the user can set or modify.
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
public class PreferencePane extends VBox {


    public PreferencePane() {
        getStyleClass().add("pdfsam-container preferences-container");
    }

    @PostConstruct
    void init() {
        getChildren().addAll(appearencePane(), behaviorPane(), workspacePane(), thumbnailsPane());
    }

    private TitledPane appearencePane() {
        return preferencePane(DefaultI18nContext.getInstance().i18n("Appearance"), new PreferenceAppearencePane());
    }

    private TitledPane behaviorPane() {
        VBox vbox = new VBox();
        I18nContext i18n = DefaultI18nContext.getInstance();

        BooleanPreferenceCheckBox checkForUpdates = new BooleanPreferenceCheckBox(BooleanUserPreference.CHECK_UPDATES,
                DefaultI18nContext.getInstance().i18n("Check for updates at startup"), DefaultUserContext.getInstance()
                        .isCheckForUpdates());
        checkForUpdates.setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n(
                "Set whether new version availability should be checked on startup (restart needed)")));
        checkForUpdates.getStyleClass().add("preference");

        BooleanPreferenceCheckBox playSounds = new BooleanPreferenceCheckBox(BooleanUserPreference.PLAY_SOUNDS,
                DefaultI18nContext.getInstance().i18n("Play alert sounds"), DefaultUserContext.getInstance()
                        .isPlaySounds());
        playSounds.setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Turn on or off alert sounds")));
        playSounds.getStyleClass().add("preference");

        BooleanPreferenceCheckBox askConfirmation = new BooleanPreferenceCheckBox(
                BooleanUserPreference.ASK_OVERWRITE_CONFIRMATION, DefaultI18nContext.getInstance().i18n(
                        "Ask for confirmation when overwrite checkbox is selected"), DefaultUserContext.getInstance()
                        .isAskOverwriteConfirmation());
        askConfirmation.setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n(
                "Show a dialog box asking the user for confirmation when the \"overwrite\" is selected")));
        askConfirmation.getStyleClass().add("preference");
        vbox.getChildren().addAll(checkForUpdates, playSounds, askConfirmation);

        return preferencePane(i18n.i18n("Behavior"), vbox);
    }

    private TitledPane workspacePane() {
        VBox vbox = new VBox();
        I18nContext i18n = DefaultI18nContext.getInstance();

        PreferenceBrowsableField workspace = new PreferenceBrowsableField(StringUserPreference.WORKSPACE_PATH);
        workspace.getTextField().setPromptText(
                i18n.i18n("Select a previously saved workspace that will be automatically loaded at startup"));
        workspace.setFileType(FileType.XML);
        workspace.getStyleClass().add("preference");
        workspace.getTextField().setText(DefaultUserContext.getInstance().getDefaultWorkspacePath());

        PreferenceBrowsableField workingDirectory = new PreferenceBrowsableField(StringUserPreference.WORKING_PATH);
        workingDirectory.getTextField().setPromptText(
                i18n.i18n("Select a directory where documents will be saved and loaded by default"));
        workingDirectory.getStyleClass().add("preference");
        workingDirectory.getTextField().setText(DefaultUserContext.getInstance().getDefaultWorkingPath());

        vbox.getChildren().addAll(new Label(i18n.i18n("Load default workspace at startup:")), workspace,
                new Label(i18n.i18n("Default working directory:")), workingDirectory);
        return preferencePane(i18n.i18n("Workspace"), vbox);
    }

    private TitledPane thumbnailsPane() {
        return preferencePane(DefaultI18nContext.getInstance().i18n("Thumbnails"), new PreferenceThumbnailsPane());
    }

    private TitledPane preferencePane(String titleString, Node node) {
        TitledPane pane = new TitledPane(titleString, node);
        pane.getStyleClass().add("preference-pane");
        return pane;
    }
}
