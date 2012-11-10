/*
 * Created on 05/apr/2012
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam.gui.preference;

import java.awt.Color;

import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SpringLayout;

import org.pdfsam.context.BooleanUserPreference;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.context.StringUserPreference;
import org.pdfsam.support.Components;
import org.pdfsam.support.SharedJFileChooser;
import org.pdfsam.support.filter.FileFilterType;

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
        PreferencePanel thumbPanel = new PreferencePanel(DefaultI18nContext.getInstance().getI18n().tr("Thumbnails"));
        BooleanPreferenceCheckBox highQualityThumbnails = new BooleanPreferenceCheckBox(
                BooleanUserPreference.HIGH_QUALITY_THUMB, DefaultI18nContext.getInstance().getI18n()
                        .tr("High quality thumbnails"), DefaultUserContext.getInstance().isHighQualityThumbnails());
        highQualityThumbnails.setBalloonTooltip(DefaultI18nContext.getInstance().getI18n()
                .tr("Generate high quality thumbnails (slower)"));
        // TODO field
        JPanel thumbSize = Components.newLabeledComponent(new JTextField(), DefaultI18nContext.getInstance().getI18n()
                .tr("Size:"));
        thumbSize.setBackground(Color.WHITE);
        JPanel thumbCreator = Components.newLabeledComponent(new JComboBox(), DefaultI18nContext.getInstance()
                .getI18n().tr("Thumbnails creator:"));
        thumbCreator.setBackground(Color.WHITE);

        thumbPanel.addPeferenceComponent(highQualityThumbnails);
        // thumbPanel.addPeferenceComponent(thumbSize);
        // thumbPanel.addPeferenceComponent(thumbCreator);
        return thumbPanel;
    }

    private JPanel workspacePanel() {
        // workspace
        PreferencePanel workspace = new PreferencePanel(DefaultI18nContext.getInstance().getI18n().tr("Workspace"));
        BrowsableField defaultWorkspace = new BrowsableField(DefaultI18nContext.getInstance().getI18n()
                .tr("Load default workspace at startup:"), StringUserPreference.WORKSPACE_PATH) {
            @Override
            JFileChooser getChooser() {
                return SharedJFileChooser.getInstance(FileFilterType.XML, JFileChooser.FILES_AND_DIRECTORIES);
            }
        };

        defaultWorkspace.setBalloonTooltip(DefaultI18nContext.getInstance().getI18n()
                .tr("Select a previously saved workspace that will be automatically loaded at startup"));
        defaultWorkspace.setDefaultFieldValue(DefaultUserContext.getInstance().getDefaultWorkspacePath());

        BrowsableField defaultWorkingPath = new BrowsableField(DefaultI18nContext.getInstance().getI18n()
                .tr("Default working directory:"), StringUserPreference.WORKING_PATH) {
            @Override
            JFileChooser getChooser() {
                return SharedJFileChooser.getInstance(FileFilterType.DIRECTORIES, JFileChooser.DIRECTORIES_ONLY);
            }
        };
        defaultWorkingPath.setBalloonTooltip(DefaultI18nContext.getInstance().getI18n()
                .tr("Select a directory where documents will be saved and loaded by default"));
        defaultWorkingPath.setDefaultFieldValue(DefaultUserContext.getInstance().getDefaultWorkingPath());
        workspace.addPeferenceComponent(defaultWorkspace);
        workspace.addPeferenceComponent(defaultWorkingPath);
        return workspace;
    }

    private JPanel behaviorPanel() {
        PreferencePanel behavoir = new PreferencePanel(DefaultI18nContext.getInstance().getI18n().tr("Behavior"));
        // behavoir
        BooleanPreferenceCheckBox checkForUpdates = new BooleanPreferenceCheckBox(BooleanUserPreference.CHECK_UPDATES,
                DefaultI18nContext.getInstance().getI18n().tr("Check for updates at startup"), DefaultUserContext
                        .getInstance().isCheckForUpdates());
        checkForUpdates.setBalloonTooltip(DefaultI18nContext.getInstance().getI18n()
                .tr("Set whether new version availability should be checked on startup (restart needed)"));

        BooleanPreferenceCheckBox playSounds = new BooleanPreferenceCheckBox(BooleanUserPreference.PLAY_SOUNDS,
                DefaultI18nContext.getInstance().getI18n().tr("Play alert sounds"), DefaultUserContext.getInstance()
                        .isPlaySounds());
        playSounds.setBalloonTooltip(DefaultI18nContext.getInstance().getI18n().tr("Turn on or off alert sounds"));

        BooleanPreferenceCheckBox askConfirmation = new BooleanPreferenceCheckBox(
                BooleanUserPreference.ASK_OVERWRITE_CONFIRMATION, DefaultI18nContext.getInstance().getI18n()
                        .tr("Ask for confirmation when overwrite checkbox is selected"), DefaultUserContext
                        .getInstance().isAskOverwriteConfirmation());
        askConfirmation.setBalloonTooltip(DefaultI18nContext.getInstance().getI18n()
                .tr("Show a dialog box asking the user for confirmation when the \"overwrite\" is selected"));
        behavoir.addPeferenceComponent(checkForUpdates);
        behavoir.addPeferenceComponent(playSounds);
        behavoir.addPeferenceComponent(askConfirmation);
        return behavoir;
    }

    private JPanel appearancePanel() {
        // appearance
        PreferencePanel appearance = new PreferencePanel(DefaultI18nContext.getInstance().getI18n().tr("Appearance"));
        JPanel language = Components.newLabeledComponent(new LocalesComboBox(), DefaultI18nContext.getInstance()
                .getI18n().tr("Language:"));
        language.setBackground(Color.WHITE);
        JPanel theme = Components.newLabeledComponent(new ThemesComboBox(), DefaultI18nContext.getInstance().getI18n()
                .tr("Theme:"));
        theme.setBackground(Color.WHITE);
        appearance.addPeferenceComponent(theme);
        appearance.addPeferenceComponent(language);
        return appearance;
    }

}
