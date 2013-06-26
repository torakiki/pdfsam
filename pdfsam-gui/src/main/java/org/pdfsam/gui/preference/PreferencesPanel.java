/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 05/apr/2012
 * Copyright 2012 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import java.awt.Color;

import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.SpringLayout;

import org.pdfsam.context.BooleanUserPreference;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.context.IntUserPreference;
import org.pdfsam.context.StringUserPreference;
import org.pdfsam.gui.FlatBorder;
import org.pdfsam.gui.SharedJFileChooser;
import org.pdfsam.gui.view.AbstractValidableTextField;
import org.pdfsam.gui.view.Views;
import org.pdfsam.support.filter.FileFilterType;
import org.pdfsam.support.validation.Validators;

/**
 * Panel showing preferences and allowing the user to set them.
 * 
 * @author Andrea Vacondio
 * 
 */
public class PreferencesPanel extends JPanel {

    @SuppressWarnings("serial")
    public PreferencesPanel() {

        SpringLayout layout = new SpringLayout();
        this.setLayout(layout);
        setBackground(Color.WHITE);
        setBorder(new FlatBorder());

        JPanel appearance = appearancePanel();
        JPanel behavoir = behaviorPanel();
        JPanel workspace = workspacePanel();
        JPanel thumbPanel = thumbnailsPanel();

        add(appearance);
        add(behavoir);
        add(workspace);
        add(thumbPanel);

        layout.putConstraint(SpringLayout.NORTH, appearance, 10, SpringLayout.NORTH, this);
        layout.putConstraint(SpringLayout.WEST, appearance, 5, SpringLayout.WEST, this);
        layout.putConstraint(SpringLayout.EAST, appearance, -5, SpringLayout.EAST, this);

        layout.putConstraint(SpringLayout.NORTH, behavoir, 15, SpringLayout.SOUTH, appearance);
        layout.putConstraint(SpringLayout.WEST, behavoir, 0, SpringLayout.WEST, appearance);
        layout.putConstraint(SpringLayout.EAST, behavoir, 0, SpringLayout.EAST, appearance);

        layout.putConstraint(SpringLayout.NORTH, workspace, 15, SpringLayout.SOUTH, behavoir);
        layout.putConstraint(SpringLayout.WEST, workspace, 0, SpringLayout.WEST, behavoir);
        layout.putConstraint(SpringLayout.EAST, workspace, 0, SpringLayout.EAST, behavoir);

        layout.putConstraint(SpringLayout.NORTH, thumbPanel, 15, SpringLayout.SOUTH, workspace);
        layout.putConstraint(SpringLayout.WEST, thumbPanel, 0, SpringLayout.WEST, workspace);
        layout.putConstraint(SpringLayout.EAST, thumbPanel, 0, SpringLayout.EAST, workspace);

    }

    private JPanel thumbnailsPanel() {
        // thumbnails
        PreferencePanel thumbPanel = new PreferencePanel(DefaultI18nContext.getInstance().i18n("Thumbnails"));
        BooleanPreferenceCheckBox highQualityThumbnails = new BooleanPreferenceCheckBox(
                BooleanUserPreference.HIGH_QUALITY_THUMB, DefaultI18nContext.getInstance().i18n(
                        "High quality thumbnails"), DefaultUserContext.getInstance().isHighQualityThumbnails());
        highQualityThumbnails.setToolTipText(DefaultI18nContext.getInstance().i18n(
                "Generate high quality thumbnails (slower)"));
        AbstractValidableTextField thumbSizeField = new AbstractValidableTextField(Validators.newIntRangeString(130,
                390)) {

            @Override
            protected void onValidInput() {
                DefaultUserContext.getInstance().setIntegerPreference(IntUserPreference.THUMBNAILS_SIZE,
                        Integer.parseInt(getText()));

            }
        };
        thumbSizeField.setText(Integer.toString(DefaultUserContext.getInstance().getThumbnailsSize()));
        thumbSizeField.setColumns(6);
        thumbSizeField.setMaximumSize(thumbSizeField.getPreferredSize());
        JPanel thumbSize = Views.newLabeledComponentWhiteBackground(thumbSizeField, DefaultI18nContext.getInstance()
                .i18n("Size in px:"),
                DefaultI18nContext.getInstance().i18n("Pixel size of the thumbnails (between 130px and 390px)"));

        JPanel thumbCreator = Views.newLabeledComponentWhiteBackground(new JComboBox<String>(), DefaultI18nContext
                .getInstance().i18n("Thumbnails creator:"),
                DefaultI18nContext.getInstance().i18n("Library used to generate thumbnails"));

        thumbPanel.addPeferenceComponent(highQualityThumbnails);
        thumbPanel.addPeferenceComponent(thumbSize);
        thumbPanel.addPeferenceComponent(thumbCreator);
        return thumbPanel;
    }

    private JPanel workspacePanel() {
        // workspace
        PreferencePanel workspace = new PreferencePanel(DefaultI18nContext.getInstance().i18n("Workspace"));
        BrowsableField defaultWorkspace = new BrowsableField(DefaultI18nContext.getInstance().i18n(
                "Load default workspace at startup:"), StringUserPreference.WORKSPACE_PATH) {
            @Override
            protected JFileChooser getChooser() {
                return SharedJFileChooser.getInstance(FileFilterType.XML, JFileChooser.FILES_AND_DIRECTORIES);
            }
        };
        defaultWorkspace.setValidator(Validators.decorateAsValidBlankString(FileFilterType.XML));
        defaultWorkspace.setTooltip(DefaultI18nContext.getInstance().i18n(
                "Select a previously saved workspace that will be automatically loaded at startup"));
        defaultWorkspace.setDefaultFieldValue(DefaultUserContext.getInstance().getDefaultWorkspacePath());

        BrowsableField defaultWorkingPath = new BrowsableField(DefaultI18nContext.getInstance().i18n(
                "Default working directory:"), StringUserPreference.WORKING_PATH) {
            @Override
            protected JFileChooser getChooser() {
                return SharedJFileChooser.getInstance(FileFilterType.DIRECTORIES, JFileChooser.DIRECTORIES_ONLY);
            }
        };
        defaultWorkingPath.setValidator(Validators.decorateAsValidBlankString(FileFilterType.DIRECTORIES));
        defaultWorkingPath.setTooltip(DefaultI18nContext.getInstance().i18n(
                "Select a directory where documents will be saved and loaded by default"));
        defaultWorkingPath.setDefaultFieldValue(DefaultUserContext.getInstance().getDefaultWorkingPath());
        workspace.addPeferenceComponent(defaultWorkspace);
        workspace.addPeferenceComponent(defaultWorkingPath);
        return workspace;
    }

    private JPanel behaviorPanel() {
        PreferencePanel behavoir = new PreferencePanel(DefaultI18nContext.getInstance().i18n("Behavior"));
        // behavoir
        BooleanPreferenceCheckBox checkForUpdates = new BooleanPreferenceCheckBox(BooleanUserPreference.CHECK_UPDATES,
                DefaultI18nContext.getInstance().i18n("Check for updates at startup"), DefaultUserContext.getInstance()
                        .isCheckForUpdates());
        checkForUpdates.setToolTipText(DefaultI18nContext.getInstance().i18n(
                "Set whether new version availability should be checked on startup (restart needed)"));

        BooleanPreferenceCheckBox playSounds = new BooleanPreferenceCheckBox(BooleanUserPreference.PLAY_SOUNDS,
                DefaultI18nContext.getInstance().i18n("Play alert sounds"), DefaultUserContext.getInstance()
                        .isPlaySounds());
        playSounds.setToolTipText(DefaultI18nContext.getInstance().i18n("Turn on or off alert sounds"));

        BooleanPreferenceCheckBox askConfirmation = new BooleanPreferenceCheckBox(
                BooleanUserPreference.ASK_OVERWRITE_CONFIRMATION, DefaultI18nContext.getInstance().i18n(
                        "Ask for confirmation when overwrite checkbox is selected"), DefaultUserContext.getInstance()
                        .isAskOverwriteConfirmation());
        askConfirmation.setToolTipText(DefaultI18nContext.getInstance().i18n(
                "Show a dialog box asking the user for confirmation when the \"overwrite\" is selected"));
        behavoir.addPeferenceComponent(checkForUpdates);
        behavoir.addPeferenceComponent(playSounds);
        behavoir.addPeferenceComponent(askConfirmation);
        return behavoir;
    }

    private JPanel appearancePanel() {
        // appearance
        PreferencePanel appearance = new PreferencePanel(DefaultI18nContext.getInstance().i18n("Appearance"));
        JPanel language = Views.newLabeledComponentWhiteBackground(new LocalesComboBox(), DefaultI18nContext
                .getInstance().i18n("Language:"),
                DefaultI18nContext.getInstance().i18n("Set your preferred language (restart needed)"));

        JPanel theme = Views.newLabeledComponentWhiteBackground(new ThemesComboBox(), DefaultI18nContext.getInstance()
                .i18n("Theme:"),
                DefaultI18nContext.getInstance().i18n("Set your preferred look and feel (restart needed)"));
        appearance.addPeferenceComponent(theme);
        appearance.addPeferenceComponent(language);
        return appearance;
    }

}
